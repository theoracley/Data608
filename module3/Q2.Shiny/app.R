#Data608 - Spring 2020

#Abdelmalek Hajjam


### Question 2:

# Often you are asked whether particular States are improving their mortality rates (per cause) faster than or slower than the national average. Create a visualization that lets your clients see this for themselves for one cause of death at a time. Keep in mind that the national average should be weighted by the national population.

library(shiny)
library(shinythemes)
library(plotly)
library(tidyverse)
library(reshape2)


#load our model3 data
m3 <- read.csv("https://raw.githubusercontent.com/theoracley/data608/master/module3/data/cleaned-cdc-mortality-1999-2010-2.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)

#if state is empty, remove the row
m3 <-m3[!(m3$State==''),]

#Preserve complete cases
m3 <- m3[complete.cases(m3), ]

#Our column names
colnames(m3) <- c('Disease', 'State', 'Year', 'Deaths', 'Population', 'Crude.Rate')


m3$Disease <- as.character(m3$Disease)
m3$Crude.Rate <- as.numeric(m3$Crude.Rate)

#get Categories
m3$Disease <- substr(m3$Disease, regexpr('>', m3$Disease)+1, nchar(m3$Disease))

#only interested in year  2010
m3.2010 <- m3[m3$Year == 2010, ]


#calculate national average of crude mortality rate from the m3 data
temp <- m3.2010 %>% group_by(Disease)  %>% summarise(total_population = sum(Population)) %>% inner_join(m3.2010, by="Disease")
temp$Weighted_CrudeRate <- (temp$Population/temp$total_population) * temp$Crude.Rate

#Add disease_national_avg to the 2010 subset (m3.2010)
m3.2010 <-
    temp %>% group_by(Disease) %>% summarise(disease_national_avg = sum(Weighted_CrudeRate)) %>% inner_join(m3.2010, by="Disease")

#Display National average for each disease
national_avg_2010 <- as.data.frame(unique(m3.2010 %>% group_by(Disease) %>% select(Disease, disease_national_avg)))

# Let's Rank
m3.2010$Rank <-  m3.2010$Crude.Rate %>%
    rank() %>%
    round(0)

m3.2010$Rank <- max(m3.2010$Rank+1) - m3.2010$Rank



# UI
ui <- fluidPage(
    
    titlePanel("Crude Mortality Rate Across All States Vs National Average"),
    
    sidebarLayout(
        
        # Input
        sidebarPanel(
            
            # Select variable for y-axis
            selectInput(inputId = "disease", 
                        label = "Select disease:",
                        choices = unique(m3.2010$Disease),
                        selected = "Neoplasms",
                        width = '500px')
            
        ),
        
        # Output:
        mainPanel(
            plotOutput(outputId = "bargraph")
        )
    )
)


server <- function(input, output, session) {
    
    #This is an event reactive element that responds to a specific event (in this case the Disease selection)
    #and sets the valueExpr to a specific value (in this case the plot title)
    ordered <- eventReactive(
        eventExpr = input$disease, 
        valueExpr = {m3.2010 %>% filter(Disease==input$disease) %>% arrange(Crude.Rate)},
        ignoreNULL = FALSE
    )
    
    national_avg <- eventReactive(
        eventExpr = input$disease,
        valueExpr = {unlist(national_avg_2010 %>% filter(Disease==input$disease) %>% select(disease_national_avg))}
    )
    
    #Create bar graph
    output$bargraph <- renderPlot({ggplot(data = ordered(), aes(x=reorder(State,-Crude.Rate), y=Crude.Rate)) + 
            geom_bar(stat="identity", width=0.7, color="#1F3552", fill="darkred", 
                     position=position_dodge()) +
            
            geom_hline(yintercept=national_avg(), color="red", size=1) + 
            ggtitle("Crude Mortality for Selected Disease for 2010 with National Average") +
            xlab("") + ylab("") + 
            theme_minimal()}, height = 600, width = 1000)
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)

