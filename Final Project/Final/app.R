######## Abdelmalek Hajjam
######## Data608 - Fall 2020
######## Final project


library(stringi)
library(dplyr)
library(maps)
library(mapview)
library(ggplot2)
library(plotly)
library(ggiraph)
library(googleVis)
library(ggvis)
library(shiny)
library(shinydashboard)
library(RColorBrewer)
library(DT)


Sys.setlocale('LC_ALL','C') 
options(shiny.trace=F)


header <- dashboardHeader(
  title = "Final Application"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("App Description", tabName = "tabIntroduction", icon = icon("question-circle")),
    menuItem("Plotting and Statistics", tabName = "tabScatterPlot", icon = icon("bar-chart")),
    menuItem("Explore This", tabName = "tabExplorer", icon = icon("list")),    
    menuItem("Maps & Data Viz", tabName = "tabVisualization", icon = icon("globe")),
    menuItem("Reference Info", tabName = "tabReference", icon = icon("external-link"))
    
  )
) 

body <- dashboardBody(
  
  tabItems(
    
    tabItem("tabIntroduction",
            h1("Data 608 Final Project - Fall 2020", style="text-align:center; color:#9b59b6"),
            br(),
            div(
            fluidRow(
              
                column(12, 
                       tags$div(
                         
                         tags$span(   
                           h4(tags$strong("The Goal of this Application: ")),
                           h4(tags$li("Provide statistical analysis and visalization of the data")),
                           h4(tags$li("Provide visualization of the data through United States map")),
                           h4(tags$li("Provide a rich user interface for exploring the data")),
                           br()
                         )
                       )
                )
              
            ), style="background-color:beige; padding:10px; border-left:5px solid #9b59b6"),
            br(),
            
            div(
            fluidRow(
              
                
                column(12, 
                       tags$div(
                         h4(tags$strong("Dataset: ")),
                         h4(tags$li("The dataset is from the United States Department of Labor, Employment & Training Administration.")),
                         tags$span(   
                           h4(tags$li("It is about the prevailing wage data of foreign employers seeking to file applications in the Permanent Labor Certification Program (prevailing wage data of US natives are not included.)")),
                           h4(tags$li("Data came from the United States Department of Labor, specifically from the Office of Foreign labor. Data and informations are found at: https://www.foreignlaborcert.doleta.gov/performancedata.cfm")),
                           h4(tags$li("This data is from 2015 year. This data has been processed, massaged and filtered. The final data for this application has 167,278 cases and  17 columns.")),
                           br()
                         )
                       )
                )
              
            ),style="background-color:beige; padding:10px; border-left:5px solid #9b59b6"),
            br(),
            
            div(
            fluidRow(

                column(12, 
                       tags$div(
                         h4(tags$strong("About this Shiny Application: ")),
                         tags$span(      
                           h4("Foreigner Labor Wage Explorer is the shiny dashboard application designed to explore and compare foreigner labor salary data among 8 chosen professions, such as:"),
                          h4(tags$li("Data Scientist")), 
                          h4(tags$li("Software Engineer")), 
                          h4(tags$li("Data Analyst")), 
                          h4(tags$li("Business Analyst")), 
                          h4(tags$li("Management Consultant")), 
                          h4(tags$li("Assistant Professor")), 
                          h4(tags$li("Attorney")), 
                          h4(tags$li("Teacher! ")),
                           
                           tags$br()
                         )
                       )
                       
                )
            
            ),style="background-color:beige; padding:10px; border-left:5px solid #9b59b6")
    
    ),
    
  
    tabItem("tabScatterPlot",
            
            div(
            fluidRow(
              div(
                h2("Wages - Box Plot and Scatter Plot"),
                
                h4("The plot area basically shows two types of visualizations: a scatter plot showing all the salary data by 8 professions, and a box plot showing values of minimum, 25 percent quintile, median, 75 percent quintile, and maximum."),
                h4("All Job Titles are plotted in different colors. "),
                h4("Please wait for the data to load initially. It takes some time, because it's trying to load all data points.")
              ,style="padding:20px;")
            ), style="background-color:beige; padding:20px; border-left:5px solid #9b59b6"),
            br(),
            div(
            fluidRow(
              column(4, 
                     selectizeInput('singleSelectForStatesForScatterPlot', 'States:', 
                                    c("All"= '', "Alabama" = "AL", "Alaska" = "AK","Arizona" = "AZ", "Arkansas" = "AR", 
                                      "California" = "CA", "Colorado" = "CO", "Connecticut" = "CT", "Delaware" = "DE", "District of Columbia" = "DC", 
                                      "Florida" = "FL", "Georgia" = "GA", "Guam" = "GU", "Hawaii" = "HI", "Idaho" = "ID", "Illinois" = "IL", 
                                      "Indiana" = "IN", "Iowa" = "IA","Kansas" = "KS", "Kentucky" = "KY", "Louisiana" = "LA", "Maine" = "ME",
                                      "Maryland" = "MD", "Massachusetts" = "MA", "Michigan" = "MI", "Minnesota" = "MN", "Mississippi" = "MS",
                                      "Missouri" = "MO", "Montana" = "MT", "Nebraska" = "NE", "Nevada" = "NV", "New Hampshire" = "NH",
                                      "New Jersey" = "NJ", "New Mexico" = "NM", "New York" = "NY","North Carolina" = "NC", "North Dakota" = "ND",
                                      "Northern Mariana Islands" = "MP", "Ohio" = "OH", "Oklahoma" = "OK", "Oregon" = "OR", "Palau" = "PW",
                                      "Pennsylvania" = "PA", "Puerto Rico" = "PR", "Rhode Island" = "RI", "South Carolina" = "SC", 
                                      "South Dakota" = "SD", "Tennessee" = "TN", "Texas" = "TX", "Utah" = "UT", "Vermont" = "VT", 
                                      "Virgin Islands" = "VI", "Virginia" = "VA", "Washington" = "WA", "West Virginia" = "WV", "Wisconsin" = "WI",
                                      "Wyoming" = "WY"),
                                    multiple = F)
              ),
              column(4, 
                     sliderInput("sliderForSalaryRangeForScatterPlot", "Salary Range:", 
                                 min = 0, max = 350000, value = c(12000, 250000), step = 5000)
              ),
              column(4, checkboxInput("checkboxForShowDataPoint", label = "Show data points", value = TRUE))
            ),
            br(), style="background-color:beige;padding:40px;"),
            br(),
            fluidRow( plotOutput("myQScatterChart") ), 
            
            br(),
            
            fluidRow(
              infoBoxOutput("minBoxInScatterSummary"),
              infoBoxOutput("medBoxInScatterSummary"),
              infoBoxOutput("maxBoxInScatterSummary")
            ),
            fluidRow(
              infoBoxOutput("q1BoxInScatterSummary"),
              infoBoxOutput("meanBoxInScatterSummary"),
              infoBoxOutput("q3BoxInScatterSummary")
            ) 
    ),
    
    tabItem("tabExplorer",
            
            div(
            h2("Salary Data Explorer"),
            
            fluidRow(
              div(
                h4("This rich user interface has many reactive controls. The data table on the bottom is reactive to all controls changes."),
                h4("Any time a control is triggered, the Data table raises an event and update itself."),style="background-color:beige; padding:10px; border-left:5px solid #9b59b6")
           ), style="background-color:beige; padding:10px; border-left:5px solid #9b59b6"),
           
           br(),
           
           div(
           
            fluidRow(
              column(3, checkboxInput("checkboxForDS", label = "Data Scientist", value = TRUE) ),
              column(3, checkboxInput("checkboxForSW", label = "Software Engineer", value = TRUE)),
              column(3, checkboxInput("checkboxForDA", label = "Data Analyst", value = TRUE)),
              column(3, checkboxInput("checkboxForBA", label = "Business Analyst", value = TRUE))
            ),
            fluidRow(
              column(3, checkboxInput("checkboxForAP", label = "Assistant Professor", value = TRUE)),
              column(3, checkboxInput("checkboxForMC", label = "Management Consultant", value = TRUE)),
              column(3, checkboxInput("checkboxForAT", label = "Attorney", value = TRUE)),
              column(3, checkboxInput("checkboxForTC", label = "Teacher", value = TRUE))
            ), style="background-color:beige; padding:10px;"),
            
           br(),
           
           div(
           
            fluidRow(
              column(6,
                     selectizeInput('multiSelectForStates', 'States:', 
                                    c("Choose multiple"= '', "Alabama" = "AL", "Alaska" = "AK","Arizona" = "AZ", "Arkansas" = "AR", 
                                      "California" = "CA", "Colorado" = "CO", "Connecticut" = "CT", "Delaware" = "DE", "District of Columbia" = "DC", 
                                      "Florida" = "FL", "Georgia" = "GA", "Guam" = "GU", "Hawaii" = "HI", "Idaho" = "ID", "Illinois" = "IL", 
                                      "Indiana" = "IN", "Iowa" = "IA","Kansas" = "KS", "Kentucky" = "KY", "Louisiana" = "LA", "Maine" = "ME",
                                      "Maryland" = "MD", "Massachusetts" = "MA", "Michigan" = "MI", "Minnesota" = "MN", "Mississippi" = "MS",
                                      "Missouri" = "MO", "Montana" = "MT", "Nebraska" = "NE", "Nevada" = "NV", "New Hampshire" = "NH",
                                      "New Jersey" = "NJ", "New Mexico" = "NM", "New York" = "NY","North Carolina" = "NC", "North Dakota" = "ND",
                                      "Northern Mariana Islands" = "MP", "Ohio" = "OH", "Oklahoma" = "OK", "Oregon" = "OR", "Palau" = "PW",
                                      "Pennsylvania" = "PA", "Puerto Rico" = "PR", "Rhode Island" = "RI", "South Carolina" = "SC", 
                                      "South Dakota" = "SD", "Tennessee" = "TN", "Texas" = "TX", "Utah" = "UT", "Vermont" = "VT", 
                                      "Virgin Islands" = "VI", "Virginia" = "VA", "Washington" = "WA", "West Virginia" = "WV", "Wisconsin" = "WI",
                                      "Wyoming" = "WY"),
                                    multiple = TRUE
                     )
              ),
              column(6, 
                     sliderInput("sliderForSalaryRange", "Salary Range:", 
                                 min = 0, max = 350000, value = c(12000, 250000), step = 5000)
              )
            ),
            
            fluidRow(
              column(6, textInput("searchInputForCity","City Search:","") ),
              column(6, textInput("searchInputForEmployer","Employer Name Search:",""))
            ), style="background-color:beige; padding:10px; "),
           
            fluidRow(br()),
            fluidRow(
              DT::dataTableOutput("myTable")
            )
    ),
    
    tabItem("tabVisualization",
            
            div(
            fluidRow(
              div(
                h2("Salary and Location - Job Comparison"),
                h4("The Salary Comparison Map provides a way to compare salary distribution of two professions in the United States."),
                h4("You can choose two professions (job titles), and data table will show the updated result."), 
                h4("You can also sort the results in the table by state, average salary and the number of jobs.")
              , style="background-color:beige; padding:10px; border-left:5px solid #9b59b6")
            ), style="background-color:beige; padding:10px; border-left:5px solid #9b59b6"),
            
            br(),
            
            div(
            fluidRow(
              column(6, 
                     selectizeInput('singleSelectForJobTitleForComparison1', 'Choose the 1st Job Title:', 
                                    c("Choose one"= '', "Data Scientist" = "data scientist", "Software Engineer" = "software engineer", 
                                      "Data Analyst" = "data analyst", "Business Analyst" = "business analyst", "Assistant Professor" = "assistant professor", 
                                      "Management Consultant" = "management consultant", "Attorney" = "attorney", "Teacher" = "teacher"
                                    ),
                                    multiple = FALSE
                     )),
              column(6, 
                     selectizeInput('singleSelectForJobTitleForComparison2', 'Choose the 2nd Job Title:', 
                                    c("Choose one"= '', "Data Scientist" = "data scientist", "Software Engineer" = "software engineer", 
                                      "Data Analyst" = "data analyst", "Business Analyst" = "business analyst", "Assistant Professor" = "assistant professor", 
                                      "Management Consultant" = "management consultant", "Attorney" = "attorney", "Teacher" = "teacher"
                                    ),
                                    multiple = FALSE
                     ))
            ) , style="background-color:beige; padding:10px;"),
            br(),
            
            fluidRow(
             box(
               title = "Map 1", solidHeader = TRUE,
               collapsible = TRUE,
               ggiraphOutput("myGvisMap1")
             ),
             box(
               title = "Map 2", solidHeader = TRUE,
               collapsible = TRUE,
               ggiraphOutput("myGvisMap2")
             )
            ), 
            
            fluidRow(
              box(
                title = "DataTable 1", solidHeader = TRUE,
                collapsible = TRUE,
                DT::dataTableOutput("myComparisonTableByJobTitle1")
                
              ),
              box(
                title = "DataTable 2", solidHeader = TRUE,
                collapsible = TRUE,
                DT::dataTableOutput("myComparisonTableByJobTitle2")
              )
           )
    ),
    
    tabItem("tabReference",
            
            div(
            fluidRow(
              div(
                
                column(12, 
                       tags$div(
                         h2("Reference :"),
                         
                         h4(tags$li(tags$a(href="https://docs.rstudio.com/shinyapps.io/getting-started.html", "https://docs.rstudio.com/shinyapps.io/getting-started.html"))),
                         h4(tags$li(tags$a(href="https://rstudio.github.io/shinydashboard/", "https://rstudio.github.io/shinydashboard/"))),
                         h4(tags$li(tags$a(href="https://shiny.rstudio.com/articles/", "https://shiny.rstudio.com/articles/"))),
                         h4(tags$li(tags$a(href="https://shiny.rstudio.com/gallery", "https://shiny.rstudio.com/gallery"))),
                         h4(tags$li(tags$a(href="https://rstudio.com/resources/webinars/", "https://rstudio.com/resources/webinars/"))),
                         h4(tags$li(tags$a(href="https://www.youtube.com/c/shinydeveloperseries", "https://www.youtube.com/c/shinydeveloperseries"))),
                         tags$br()
                       )
                )
              ,style="background-color:beige; padding:10px; border-left:5px solid #9b59b6")
            ),style="background-color:beige; padding:10px; border-left:5px solid #9b59b6") 
       
    ) #end tabItem
  )
)

#Reading the Data
fifty_states <- read.csv("https://raw.githubusercontent.com/fung1091/data608/master/Final%20Project/fifty_states.csv")
myURL <- "https://github.com/theoracley/Data608/blob/master/Final%20Project/salary.rdata"

read.csv(url(myURL))
salary_refined <- x


#The Server
server <- function(input, output) { 
  
  options("scipen"=10) 
  
 
  updateInputDataForDTable <- reactive({  
    
    dataFilteredForDTable <- salary_refined
    
    if(input$checkboxForDS != T){
      dataFilteredForDTable <- dataFilteredForDTable[(dataFilteredForDTable$JOB_TITLE_SUBGROUP != "data scientist"),]        
    } 
    if(input$checkboxForSW != T){
      dataFilteredForDTable <- dataFilteredForDTable[(dataFilteredForDTable$JOB_TITLE_SUBGROUP != "software engineer"),]        
    } 
    if(input$checkboxForDA != T){
      dataFilteredForDTable <- dataFilteredForDTable[(dataFilteredForDTable$JOB_TITLE_SUBGROUP != "data analyst"),]        
    } 
    if(input$checkboxForBA != T){
      dataFilteredForDTable <- dataFilteredForDTable[(dataFilteredForDTable$JOB_TITLE_SUBGROUP != "business analyst"),]        
    } 
    if(input$checkboxForAP != T){
      dataFilteredForDTable <- dataFilteredForDTable[(dataFilteredForDTable$JOB_TITLE_SUBGROUP != "assistant professor"),]        
    } 
    if(input$checkboxForMC != T){
      dataFilteredForDTable <- dataFilteredForDTable[(dataFilteredForDTable$JOB_TITLE_SUBGROUP != "management consultant"),]        
    } 
    if(input$checkboxForAT != T){
      dataFilteredForDTable <- dataFilteredForDTable[(dataFilteredForDTable$JOB_TITLE_SUBGROUP != "attorney"),]        
    } 
    if(input$checkboxForTC != T){
      dataFilteredForDTable <- dataFilteredForDTable[(dataFilteredForDTable$JOB_TITLE_SUBGROUP != "teacher"),]        
    } 
    
    #Do the filtering
    if(!is.null(input$multiSelectForStates) ){
      targetStates <- unlist(strsplit(input$multiSelectForStates," "))
      dataFilteredForDTable <- dataFilteredForDTable %>% filter(WORK_STATE_ABBREVIATION %in% targetStates)
    }
    
    #Do it by City
    if(input$searchInputForCity != ""){
      dataFilteredForDTable <- dataFilteredForDTable %>% 
        filter(grepl(input$searchInputForCity,dataFilteredForDTable$WORK_CITY, ignore.case = TRUE)) 
    }
    
    # Do it by employer
    if(input$searchInputForEmployer != ""){
      dataFilteredForDTable <- dataFilteredForDTable %>% 
        filter(grepl(input$searchInputForEmployer, dataFilteredForDTable$EMPLOYER_NAME, ignore.case = TRUE)) 
    }
    
    dataFilteredForDTable
  })
  
  
 #wage Scotter Plot
  updateInputDataForScatterPlot <- reactive({  
    dataFilteredForScatterPlot <- salary_refined
    dataFilteredForScatterPlot <- dataFilteredForScatterPlot %>% group_by(JOB_TITLE_SUBGROUP) 
    
    dataFilteredForScatterPlot <- dataFilteredForScatterPlot[(input$sliderForSalaryRangeForScatterPlot[1] <= dataFilteredForScatterPlot$PAID_WAGE_PER_YEAR &
                                                                dataFilteredForScatterPlot$PAID_WAGE_PER_YEAR <= input$sliderForSalaryRangeForScatterPlot[2]),]
    
    if(input$singleSelectForStatesForScatterPlot != ""){
      dataFilteredForScatterPlot <- dataFilteredForScatterPlot[(input$singleSelectForStatesForScatterPlot == dataFilteredForScatterPlot$WORK_STATE_ABBREVIATION),]
    }
    
    dataFilteredForScatterPlot <- dataFilteredForScatterPlot %>% mutate(JOB_GROUP_CODE = ifelse(JOB_TITLE_SUBGROUP == "assistant professor", 1,
                                                                                                 ifelse(JOB_TITLE_SUBGROUP == "attorney", 2,
                                                                                                        ifelse(JOB_TITLE_SUBGROUP == "business analyst", 3,
                                                                                                               ifelse(JOB_TITLE_SUBGROUP == "data analyst", 4,
                                                                                                                      ifelse(JOB_TITLE_SUBGROUP == "data scientist", 5,
                                                                                                                             ifelse(JOB_TITLE_SUBGROUP == "management consultant", 6,
                                                                                                                                    ifelse(JOB_TITLE_SUBGROUP == "software engineer", 7,
                                                                                                                                           ifelse(JOB_TITLE_SUBGROUP == "teacher", 8)))))))))
    
    
    manualQuartile <- function(x) {
      x <- sort(x)
      n <- length(x)
      m <- (n+1)/2
      if (floor(m) != m) { l <- (m-1)/2; u <- (m+1)/2
      } else { l <- m-1; u <- m+1 }
      c(Min=min(x), Q1=median(x[1:l]), Median = median(x), Mean=mean(x), Q3=median(x[u:n]), Max=max(x))
    }
    res_mq <- manualQuartile(dataFilteredForScatterPlot$PAID_WAGE_PER_YEAR)
    
    
    output$minBoxInScatterSummary <- renderInfoBox({
      infoBox( "MIN:", sprintf("$%.2f",res_mq['Min']), icon = icon("fa fa-exclamation-circle"), color = "yellow" ) })
    
    output$meanBoxInScatterSummary <- renderInfoBox({
      infoBox( "MEAN:", sprintf("$%.2f",res_mq['Mean']), icon = icon("fa fa-info-circle"), color = "light-blue" ) })
    
    output$maxBoxInScatterSummary <- renderInfoBox({ 
      infoBox( "MAX:", sprintf("$%.2f",res_mq['Max']), icon = icon("fa fa-exclamation-circle"), color = "red" ) })
    
    output$q1BoxInScatterSummary <- renderInfoBox({
      infoBox( "Q1:", sprintf("$%.2f",res_mq['Q1']), icon = icon("fa fa-exclamation-circle"), color = "aqua" ) })
    
    output$medBoxInScatterSummary <- renderInfoBox({
      infoBox( "MEDIAN:", sprintf("$%.2f",res_mq['Median']), icon = icon("fa fa-info-circle"), color = "maroon" ) })
    
    output$q3BoxInScatterSummary <- renderInfoBox({
      infoBox( "Q3:", sprintf("$%.2f",res_mq['Q3']), icon = icon("fa fa-exclamation-circle"), color = "blue" ) })
    
    
    dataFilteredForScatterPlot
  })
  
  
  #Let's manipulate the data
  updateInputDataForMapOverall <- reactive({  
    dataFilteredForMap <- salary_refined
    dataFilteredForMap <- dataFilteredForMap %>% group_by(WORK_STATE) %>% summarise(AVG_SALARY= round(mean(PAID_WAGE_PER_YEAR), 2))
    dataFilteredForMap
    
  })
  
  
  updateInputDataForMapByJobTitle1 <- reactive({  
    dataFilteredForMapByJobTitle1 <- salary_refined
    dataFilteredForMapByJobTitle1 <- dataFilteredForMapByJobTitle1 %>% mutate(WORK_STATE = tolower(WORK_STATE))
    dataFilteredForMapByJobTitle1 <- dataFilteredForMapByJobTitle1 %>% group_by(WORK_STATE, JOB_TITLE_SUBGROUP) %>% summarise(AVG_SALARY= round(mean(PAID_WAGE_PER_YEAR), 2), NUM_POS = n())
    
    if(input$singleSelectForJobTitleForComparison1 != ""){
      dataFilteredForMapByJobTitle1 <- dataFilteredForMapByJobTitle1[(input$singleSelectForJobTitleForComparison1 == dataFilteredForMapByJobTitle1$JOB_TITLE_SUBGROUP),]
    }
    
    dataFilteredForMapByJobTitle1
  })
  
  updateInputDataForMapByJobTitle2 <- reactive({  
    dataFilteredForMapByJobTitle2 <- salary_refined
    dataFilteredForMapByJobTitle2 <- dataFilteredForMapByJobTitle2 %>% mutate(WORK_STATE = tolower(WORK_STATE))
    dataFilteredForMapByJobTitle2 <- dataFilteredForMapByJobTitle2 %>% group_by(WORK_STATE, JOB_TITLE_SUBGROUP) %>% summarise(AVG_SALARY= round(mean(PAID_WAGE_PER_YEAR), 2), NUM_POS = n())
    
    if(input$singleSelectForJobTitleForComparison2 != ""){
      dataFilteredForMapByJobTitle2 <- dataFilteredForMapByJobTitle2[(input$singleSelectForJobTitleForComparison2 == dataFilteredForMapByJobTitle2$JOB_TITLE_SUBGROUP),]
    } 
    
    dataFilteredForMapByJobTitle2
  })
  
 
  
  # Rendering the table
  output$myTable <- DT::renderDataTable(DT::datatable({ 
    dataForDTable <- updateInputDataForDTable()
    dataForDTable <- dataForDTable[, c(1,2,4,5,7,9)]
    colnames(dataForDTable) <- c("JOB_TITLE_GROUP","JOB_TITLE","EMPLOYER", "WAGE_YEARLY", "CITY", "STATE")
    
    dataForDTable
    
  }, rownames = FALSE, extensions = c('ColReorder','Scroller'), options = list(
    deferRender = TRUE,  
    searching = F,
    dom = 'RC<"clear">lfrtip',
    lengthMenu = list(c(10, 5, 15, 25, 25, 50, 100), c('10', '5', '15', '20', '25','50','100')),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#cce6ff', 'color': '#2a4e6c'});",
      "}")
  ) 
  ) %>% formatCurrency(c('WAGE_YEARLY'), "$") ) 
  
  
 #maping with the fifty states data we found online, it help in long and lat
  output$myGvisMap1 <- renderggiraph({
    
    mapData <- updateInputDataForMapByJobTitle1() # View(mapData)
    
    mapData$AVG_SALARY<-round(mapData$AVG_SALARY)
    mapData$WORK_STATE <- as.character(mapData$WORK_STATE)
    mapData <- mapData[order(as.numeric(-mapData$AVG_SALARY)),]
    
    #creating the tooltip with help of girafe package
    states_ <- paste0(
      "<table><tr><td>State:&nbsp;</td>",
      sprintf("<td style='color:#3498db'>%s</td>", gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2",mapData$WORK_STATE,
                                                        perl = TRUE)),
      "</tr>"
    )
    table_ <- paste0(
      "<tr><td>Avg. Salary:&nbsp;</td>",
      sprintf("<td style='color:#3498db'>$%.2f</td>", mapData$AVG_SALARY),
      "</tr></table>"
    )
    
    onclick <- sprintf(
      "window.open(\"%s%s\")",
      "http://en.wikipedia.org/wiki/",
      mapData$WORK_STATE
    )
    
    mapData$labs <- paste0(states_, table_)
    mapData$onclick = onclick
    
    gg_map1 <- ggplot(mapData, aes(map_id = mapData$WORK_STATE)) + 
      scale_fill_gradient(low="beige", high="red") +
      theme(legend.position = "right")
    
    gg_map1 <- gg_map1 + geom_map_interactive(aes(
      fill = mapData$AVG_SALARY,
      tooltip = labs,
      data_id = mapData$WORK_STATE,
      onclick = onclick
    ),
    map = fifty_states) +
      expand_limits(x = fifty_states$long, y = fifty_states$lat)
    girafe(ggobj = gg_map1, width_svg = 10, height_svg = 5)
    
  })  
  
  
  output$myGvisMap2 <- renderggiraph({
    
    mapData <- updateInputDataForMapByJobTitle2() 
    
    mapData$AVG_SALARY<-round(mapData$AVG_SALARY)
    mapData$WORK_STATE <- as.character(mapData$WORK_STATE)
    mapData <- mapData[order(as.numeric(-mapData$AVG_SALARY)),]
    
    #creating the tooltip with help of girafe package
    states_ <- paste0(
      "<table><tr><td>State:&nbsp;</td>",
      sprintf("<td style='color:#3498db'>%s</td>", gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2",mapData$WORK_STATE,
                                                        perl = TRUE)),
      "</tr>"
    )
    table_ <- paste0(
      "<tr><td>Avg. Salary:&nbsp;</td>",
      sprintf("<td style='color:#3498db'>$%.2f</td>", mapData$AVG_SALARY),
      "</tr></table>"
    )
    
    onclick <- sprintf(
      "window.open(\"%s%s\")",
      "http://en.wikipedia.org/wiki/",
      mapData$WORK_STATE
    )
    
    #Tooltip_css <- "background-color:transparent;”
    
    mapData$labs <- paste0(states_, table_)
    mapData$onclick = onclick
    
    gg_map <- ggplot(mapData, aes(map_id = mapData$WORK_STATE)) + 
     
      scale_fill_gradient(low="beige", high="red")  +
      theme(legend.position = "right")
       
    gg_map <- gg_map + geom_map_interactive(aes(
      fill = mapData$AVG_SALARY,
      tooltip = labs,
      data_id = mapData$WORK_STATE,
      onclick = onclick
    ),
    map = fifty_states) +
      expand_limits(x = fifty_states$long, y = fifty_states$lat)
    girafe(ggobj = gg_map, width_svg = 10, height_svg = 5)
    
    
  })
  
  
  
  #tables for the maps
  output$myComparisonTableByJobTitle1 <- DT::renderDataTable(DT::datatable({ 
    
    dataForDTable1 <- updateInputDataForMapByJobTitle1()
    colnames(dataForDTable1) <- c("STATE","JOB_TITLE","AVG_SALARY", "JOBS") 
    dataForDTable1
    
  }, rownames = FALSE, extensions = c('ColReorder','Scroller'), options = list(
    deferRender = TRUE,  
    searching = T,
    order = list(list(2, 'desc'), list(3, 'desc')),
    dom = 'RC<"clear">lfrtip',
    lengthMenu = list(c(10, 5, 15, 25, 25, 50, 100), c('10', '5', '15', '20', '25','50','100')),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#cce6ff', 'color': '#2a4e6c'});",
      "}")
  )) %>% formatCurrency(c('AVG_SALARY'), "$") ) 
  
  
  output$myComparisonTableByJobTitle2 <- DT::renderDataTable(DT::datatable({ 
    
    dataForDTable2 <- updateInputDataForMapByJobTitle2()
    colnames(dataForDTable2) <- c("STATE","JOB_TITLE","AVG_SALARY", "JOBS") 
    dataForDTable2
    
  }, rownames = FALSE, extensions = c('ColReorder','Scroller'), options = list(
    deferRender = TRUE,  
    searching = T,
    order = list(list(2, 'desc'), list(3, 'desc')),
    dom = 'RC<"clear">lfrtip',
    lengthMenu = list(c(10, 5, 15, 25, 25, 50, 100), c('10', '5', '15', '20', '25','50','100')),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#cce6ff', 'color': '#2a4e6c'});",
      "}")
  )) %>% formatCurrency(c('AVG_SALARY'), "$") ) 
  
  
 #rendering Scotter Plot
  output$myQScatterChart <- renderPlot({
    
    dataForScatterPlot <- updateInputDataForScatterPlot()
    
    if(input$checkboxForShowDataPoint == T){
     qplot(JOB_TITLE_SUBGROUP, PAID_WAGE_PER_YEAR, data=dataForScatterPlot, geom="boxplot", group = JOB_TITLE_SUBGROUP, color=JOB_TITLE_SUBGROUP) + 
        labs(title = "Total review counts by the salary", x="Job Title", y="PAID WAGE PER YEAR") +
        geom_jitter(position=position_jitter(width=.9), size=1, alpha=.3) + 
        theme(legend.position="none") 
    
    } else{
      qplot(JOB_TITLE_SUBGROUP, PAID_WAGE_PER_YEAR, data=dataForScatterPlot, geom="boxplot", group = JOB_TITLE_SUBGROUP, color=JOB_TITLE_SUBGROUP) + 
        labs(title = "Total review counts by the salary", x="Job Title", y="PAID WAGE PER YEAR") + 
        theme(legend.position="none")
     
    }
  })
  
}

#The UI
ui = dashboardPage(header, sidebar, body, skin = "black")

#The app
shinyApp(ui, server)

