#********************************************************
# ---- Residency Interview Predictor by Ben Wissel ---- *
#********************************************************

# ---- Packages and general settings ----
#****************************************
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(rsconnect)) install.packages("rsconnect", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(plyr)) install.packages("plyr", repos = "http://cran.us.r-project.org")
if(!require(DT)) install.packages("DT", repos = "http://cran.us.r-project.org")
if(!require(ranger)) install.packages("ranger", repos = "http://cran.us.r-project.org")
if(!require(gtsummary)) install.packages("gtsummary", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")

rsconnect::setAccountInfo(name='wissel', 
                          token='F0E3038A8DA12F086FDE7F33CB610549', 
                          secret='kdcK8H1SgMIwl5B9TZsXdb9Y/sjAMZRzj5O9VTYX')

setwd("/Users/wis2lp/Documents/InterviewPredictor/")

# ---- Using online data and Google Analytics ? ----
#*************************************************
if(Sys.getenv("SHINY_PORT") == ""){
  #Working locally in RStudio
  print("LOCAL MODE")
  use_online_data = F
  use_google_analytics = F
} else{
  print("ONLINE MODE")
  use_online_data = T
  use_google_analytics = T
} 


# ---- Loading initial data----
#******************************

# Applicant data
stud_data <- read.csv("STAR_Responses_2017_2022.csv", stringsAsFactors=F) %>%
  mutate(STAR.ID = as.character(STAR.ID),
         PhD = ifelse(Other.Degrees == "PhD", "Y","N"),
         MPH = ifelse(Other.Degrees == "MPH", "Y","N")) %>%
  filter(Specialty == "Neurological Surgery") %>%
  select(STAR.ID, Step.1, Step.2.CK, Cumulative.Quartile, AOA.Sigma, PhD,X..Peer.Rev.Publications,
         X..Abstracts..Pres..Posters,X..Research.Experiences,X..Leadership.Positions)

# Applications and program interview offers
app_data <- read.csv("STAR_Programs_2017_2022.csv", stringsAsFactors=F) %>%
  mutate(STAR.ID = as.character(STAR.ID)) %>%
  filter(STAR.ID %in% unique(stud_data$STAR.ID),
         !grepl("University of Puerto Rico",Program.Institution), # get rid of programs that do not exist anymore
         !grepl("Wayne State University",Program.Institution),
         !grepl("National Capital Consortium",Program.Institution),
         !grepl("Clinical Center at the National Institutes of Health",Program.Institution),
         !grepl("Philadelphia College of Osteopathic Medicine",Program.Institution),
         !grepl("St Barnabas Medical Center",Program.Institution),
         !grepl("Riverside",Program.Institution),
         !grepl("Preliminary",Program.Institution)) %>%
  select(STAR.ID, Program.Institution, ACGME.Code, Interview.Offer, Matched, Geographic.Connection, Away.Rotation)

# Correct/standardize program names based off of ACGME codes
acgme_dic <- app_data %>% 
  select(ACGME.Code, Program.Institution) %>%
  group_by(ACGME.Code) %>% slice_tail() %>% ungroup() %>%
  slice(-1)
for(acgme_code in acgme_dic$ACGME.Code){
  app_data$Program.Institution[app_data$ACGME.Code==acgme_code] <- acgme_dic$Program.Institution[acgme_dic$ACGME.Code==acgme_code]
}
app_data <- app_data %>%
  mutate(Program.Institution = ifelse(grepl("Baylor College of Medicine",Program.Institution),"Baylor College of Medicine",Program.Institution),
         Program.Institution = ifelse(grepl("Beaumont Health",Program.Institution),"Beaumont Health",Program.Institution),
         Program.Institution = ifelse(grepl("Beth Israel",Program.Institution),"Beth Israel",Program.Institution),
         Program.Institution = ifelse(grepl("Brigham and Women's Hospital",Program.Institution),"Brigham and Women's Hospital",Program.Institution),
         Program.Institution = ifelse(grepl("Brown University",Program.Institution),"Brown University",Program.Institution),
         Program.Institution = ifelse(grepl("Carilion Clinic",Program.Institution),"Carilion Clinic-Virginia Tech",Program.Institution),
         Program.Institution = ifelse(grepl("Dartmouth",Program.Institution),"Dartmouth-Hitchcock",Program.Institution),
         Program.Institution = ifelse(grepl("Dartmouth-Hitchcock",Program.Institution),"Dartmouth-Hitchcock",Program.Institution),
         Program.Institution = ifelse(grepl("George Washington University",Program.Institution),"George Washington University",Program.Institution),
         Program.Institution = ifelse(grepl("Dartmouth-Hitchcock",Program.Institution),"Dartmouth-Hitchcock",Program.Institution),
         Program.Institution = ifelse(grepl("Loyola University",Program.Institution),"Loyola University",Program.Institution),
         Program.Institution = ifelse(grepl("Massachusetts General Hospital",Program.Institution),"Massachusetts General Hospital",Program.Institution),
         Program.Institution = ifelse(grepl("Georgetown University",Program.Institution),"Georgetown University",Program.Institution),
         Program.Institution = ifelse(grepl("NYU|New York University",Program.Institution),"New York University",Program.Institution),
         Program.Institution = ifelse(grepl("Rutgers",Program.Institution),"Rutgers",Program.Institution),
         Program.Institution = ifelse(grepl("Stanford",Program.Institution),"Stanford",Program.Institution),
         Program.Institution = ifelse(grepl("Jackson Health System",Program.Institution),"University of Miami/Jackson Health System",Program.Institution),
         Program.Institution = ifelse(grepl("Northwell",Program.Institution),"Zucker School of Medicine at Hofstra/Northwell",Program.Institution),
         Program.Institution = ifelse(grepl("Westchester",Program.Institution),"New York Medical College at Westchester Medical Center",Program.Institution),
         Program.Institution = ifelse(grepl("St Joseph's Hospital",Program.Institution),"Barrow Neurological Institute",Program.Institution),
         Program.Institution = ifelse(grepl("University of Florida",Program.Institution),"University of Florida",Program.Institution),
         Program.Institution = ifelse(grepl("San Antonio",Program.Institution),"University of Texas Health Science Center San Antonio",Program.Institution),
         Program.Institution = ifelse(grepl("University of Texas Southwestern",Program.Institution),"University of Texas Southwestern",Program.Institution),
         Program.Institution = ifelse(grepl("University of New Mexico",Program.Institution),"University of New Mexico",Program.Institution),
         Program.Institution = ifelse(grepl("University of Arizona",Program.Institution),"University of Arizona",Program.Institution),
         Program.Institution = ifelse(grepl("Texas A&M College of Medicine",Program.Institution),"Texas A&M College of Medicine",Program.Institution),
         Program.Institution = ifelse(grepl("UCLA",Program.Institution),"UCLA Medical Center",Program.Institution),
         Program.Institution = ifelse(grepl("Southern Illinois University",Program.Institution),"Southern Illinois University",Program.Institution),
         Program.Institution = ifelse(grepl("UMass|University of Massachusetts",Program.Institution),"University of Massachusetts",Program.Institution),
         Program.Institution = ifelse(grepl("University of Louisville",Program.Institution),"University of Louisville",Program.Institution),
         Program.Institution = ifelse(grepl("University of Arkansas",Program.Institution),"University of Arkansas",Program.Institution),
         Program.Institution = ifelse(grepl("University of Virginia",Program.Institution),"University of Virginia",Program.Institution),
         Program.Institution = ifelse(grepl("University of California (San Diego)",Program.Institution, fixed = T),"University of California (San Diego)",Program.Institution),
         Program.Institution = ifelse(grepl("University of Michigan",Program.Institution),"University of Michigan",Program.Institution),
         Program.Institution = ifelse(grepl("University of Wisconsin",Program.Institution),"University of Wisconsin",Program.Institution),
         Program.Institution = ifelse(grepl("University of Pennsylvania",Program.Institution),"University of Pennsylvania",Program.Institution),
         Program.Institution = ifelse(grepl("University of Utah",Program.Institution),"University of Utah",Program.Institution)
  )

# Merge the program applications with the student metrics data, and make some formatting adjustments
prog_apps <- app_data %>% left_join(stud_data, by = c("STAR.ID"="STAR.ID")) %>% 
  mutate(Step.1 = ifelse(Step.1=="N/A",NA,Step.1) %>% substr(1,3) %>% as.numeric(), # "270 or higher" gets converted to 270
         Step.2.CK = ifelse(Step.2.CK=="N/A",255,Step.2.CK) %>% substr(1,3) %>% as.numeric(),
         X..Peer.Rev.Publications = ifelse(is.na(X..Peer.Rev.Publications),6,X..Peer.Rev.Publications),
         X..Abstracts..Pres..Posters = ifelse(is.na(X..Abstracts..Pres..Posters),10,X..Abstracts..Pres..Posters),
         X..Leadership.Positions = ifelse(is.na(X..Leadership.Positions),4,X..Leadership.Positions),
         Cumulative.Quartile = ifelse(Cumulative.Quartile=="N/A",0,Cumulative.Quartile) %>% substr(1,1) %>% as.numeric()) %>%
  select(Program.Institution, Interview.Offer, Step.1, Step.2.CK, Cumulative.Quartile, AOA.Sigma, PhD,X..Peer.Rev.Publications, Away.Rotation, Geographic.Connection)

# Set default "me" data
me <- prog_apps[1,3:ncol(prog_apps)] %>% mutate(
  Step.1 = 240,
  Step.2.CK = 250,
  Cumulative.Quartile = 1,
  X..Peer.Rev.Publications = 0
  # X..Abstracts..Pres..Posters = 0,
  # X..Research.Experiences = 0,
  # X..Leadership.Positions = 0
)

# Set up the prediction output dataframe
store_preds <- data.frame(matrix(0, ncol = 11, nrow = length(unique(app_data$Program.Institution))))
colnames(store_preds) <- c("School","N","Offers","InterviewProbability","Percentile",names(summary(1:5)))
store_preds$School <- unique(app_data$Program.Institution)


# Train all the random forests for each program, and store them in a list
rf_list <- vector("list", length = length(unique(app_data$Program.Institution)))
names(rf_list) <- unique(app_data$Program.Institution)

for(school in unique(prog_apps$Program.Institution)){
  school_data <- prog_apps %>% filter(grepl(school,Program.Institution, fixed = T)) %>% select(-Program.Institution)
  
  rf <- ranger::ranger(Interview.Offer ~., data=school_data,
                       probability = T, num.trees = 1000, seed=777)
  
  rf_list[[which(names(rf_list) %in% school)]] <- rf
  
  pred_sum <- rf$predictions[,2] %>% summary()
  
  store_preds[store_preds$School==school, ] <- c(school,
                                                 nrow(school_data),
                                                 sum(school_data$Interview.Offer=="Y"), 
                                                 0,0, # Fillers for prediction and percentile (made after we get the user's input)
                                                 pred_sum)
}

# ---- Functions ----
#********************

# None yet, may make some later


# ---- UI ----
#**************
ui <- tagList(
  # # Add Google Analytics
  # if(use_google_analytics){
  #   tags$head(includeHTML("google-analytics.html"))
  # },
  navbarPage(theme = shinytheme("paper"), collapsible = TRUE, id="nav",
             title = "Residency Interview Predictor",
             
             tabPanel("Main Page",
                      sidebarLayout(  
                        sidebarPanel(width = 4,
                                     numericInput(inputId = 'Step1', label = 'Enter Step 1 Score', value = 240),
                                     numericInput(inputId = 'Step2', label = 'Enter Step 2 CK Score', value = 250),
                                     helpText("If you have not taken Step 2 CK yet, guess what your score will be (e.g. 10 points higher than Step 1)."),
                                     radioButtons("Cumulative.Quartile", "Class Rank (Quartile)", 
                                                  list("1st" = 1, "2nd" = 2, 
                                                       "3rd" = 3, "4th" = 4, "School Doesn't Rank" = 0), inline = T, selected = 1),
                                     # conditionalPanel(
                                     #   condition = "input.regionType != 'Country'", 
                                     #   selectInput(inputId = "region",
                                     #               label = "Select one or more regions",
                                     #               choices = "",
                                     #               multiple = T, 
                                     #               selectize = T
                                     #   )
                                     # ),
                                     radioButtons("AOA.Sigma", "AOA", list("Yes" = "Yes", "No" = "No", "No School Chapter" = "No School Chapter"), inline = T, selected = "No"),
                                     radioButtons("PhD", "PhD", list("Yes" = "Y", "No" = "N"), inline = T, selected = "N"),
                                     numericInput(inputId = 'X..Peer.Rev.Publications', label = 'Peer-Reviewed Publications', value = 0),
                                     # radioButtons("Away.Rotation", "Away Rotation", list("Yes" = "Y", "No" = "N"), inline = T, selected = "N"),
                                     # radioButtons("Geographic.Connection", "Geographic Connection", list("Yes" = "Y", "No" = "N"), inline = T, selected = "N"),
                                     actionButton("button", "Submit"),
                                     tags$br(),
                                     downloadButton("downloadPredictions", "Download predictions")
                                     #mobileDetect('isMobile')
                                     
                        ),
                        mainPanel(
                          DTOutput(outputId = 'predTable')
                        )
                      )
             ),
             tabPanel("About this site",
                      tags$div(
                        #HTML('App:&nbsp'), prettyDate(file.info("app.R")$mtime),
                        tags$br(),tags$br(),tags$h4("Summary"),
                        "This tool allows residency applicants to input their stats and see their likelihood of receiving and interview invitation.", tags$br(), tags$br(),
                        tags$br(),tags$br(),tags$h4("Sources"),
                        tags$b("Texas STAR: "), HTML(paste0(a(href='https://www.utsouthwestern.edu/education/medical-school/about-the-school/student-affairs/texas-star.html','UT Southwestern', target="_blank"), sep = '')), 
                        tags$br(),tags$br(),tags$h4("Contact"),
                        HTML('<b>Benjamin Wissel</b><br>Email: <a href="mailto:benjamindwissel@gmail.com?Subject=About%20Interview%20Predictor" target="_top">benjamindwissel@gmail.com</a>'),
                        tags$br(),
                        "Twitter: ", tags$a(href="https://twitter.com/BDWissel", "@bdwissel", target="_blank"),
                        tags$br(),tags$br(),tags$h4("Acknowledgements"),
                        HTML(paste0("This site would not be possible without the help of many. Special thanks to ", 
                                    a(href="https://profiles.utsouthwestern.edu/profile/28846/angela-mihalic.html?_ga=2.88011226.357659904.1665450785-1585356671.1665450785&", "Angela Mihalic, MD", target="_blank"), 
                                    " for her leadership in collecting the data used to generate the predictions.", sep = "")),
                        tags$br(),tags$br(),tags$br(),
                        HTML(sprintf('<font color="white">%s</font>',paste(system("hostnamectl --static"), collapse = "; ")))
                        
                      )
             )
             # #Add the logo
             # tags$script(HTML("var header = $('.navbar> .container-fluid > .navbar-collapse');
             #                  header.append('<div style=\"float:right; margin-top:10px;\"><img src=\"headerLogo.jpg\" height=\"40px\"></div>');"))
             
  ))


# ---- SERVER ----
#*****************
server <- function(input, output, session) {
  
  
  referenceMe = reactive(paste("Authors: Benjamin Wissel and Angela Mihalic, MD\n",
                               "Data from the Texas STAR (2018-2022).\n",
                               "\nhttps://www.utsouthwestern.edu/education/medical-school/about-the-school/student-affairs/texas-star.html", sep = ""))
  
  # Get the user input
  user_stats <- eventReactive(input$button, {
    #pulled.data[which(pulled.data$MRN==input$MRN, arr.ind = TRUE),]
    
    prog_apps[1,3:ncol(prog_apps)] %>% mutate(
      Step.1 = round_any(input$Step1, 5, f = floor),
      Step.2.CK = round_any(input$Step2, 5, f = floor),
      Cumulative.Quartile = input$Cumulative.Quartile,
      X..Peer.Rev.Publications = ifelse(input$X..Peer.Rev.Publications>11,11,input$X..Peer.Rev.Publications),
      AOA.Sigma = input$AOA.Sigma,
      PhD = input$PhD)
    
  })
  
  #Calculate the predictions to be displayed
  display_preds <- eventReactive(input$button, {
    for(school in unique(prog_apps$Program.Institution)){
      pred_user <- predict(rf_list[[which(names(rf_list) %in% school)]], user_stats())$predictions[,2]
      store_preds[store_preds$School==school, 4] <- pred_user
      store_preds[store_preds$School==school, 5] <- percent_rank(c(pred_user,
                                                                   rf_list[[which(names(rf_list) %in% school)]]$predictions[,2]))[1]
    }
    store_preds[,!colnames(store_preds) %in% "School"] <- sapply(store_preds[,!colnames(store_preds) %in% "School"],as.numeric)
    store_preds$Diff <- store_preds$InterviewProbability - store_preds$Median
    store_preds
    
  })
  
  output$predTable = renderDT({
    datatable(display_preds(), rownames = F, 
              options = list(pageLength = 20, columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>% 
      formatPercentage(4:12, digits = 1)
  })
  
  # ---- Download Predictions table ----
  output$downloadPredictions <- downloadHandler(
    filename = function() {
      paste("interview_predictions_", as.integer(Sys.time()), ".csv", sep="")
    },
    content = function(file) {
      #Get the predictions
      ggsave(file, display_preds(), device = "csv")
      
    }
  )
  
  
}

shinyApp(ui = ui, server = server)

deployApp()
