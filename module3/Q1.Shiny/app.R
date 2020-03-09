#Data608 - Spring 2020

#Abdelmalek Hajjam

# Question 1:
# As a researcher, you frequently compare mortality rates from particular causes across different States. You need a visualization that will let you see 
#(for 2010 only) the crude mortality rate, across all States, from one cause (for example, Neoplasms, which are effectively cancers). Create a visualizati#on that allows you to rank States by crude mortality for each cause of death. 



library(shiny)
library(shinythemes)
library(plotly)
library(tidyverse)
library(reshape2)


#load our model3 data
m3 <- read.csv("https://raw.githubusercontent.com/theoracley/data608/master/module3/data/cleaned-cdc-mortality-1999-2010-2.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)

 #if state value is empty, then romove the row
 m3 <-m3[!(m3$State==''),]

 #Preserve complete cases 
 m3 <- m3[complete.cases(m3), ]


 colnames(m3) <- c('Disease', 'State', 'Year', 'Deaths', 'Population', 'Crude.Rate')


m3$Disease <- as.character(m3$Disease)
m3$Crude.Rate <- as.numeric(m3$Crude.Rate)

#get categories
m3$Disease <- substr(m3$Disease, regexpr('>', m3$Disease)+1, nchar(m3$Disease))

#Only interested in year 2010
m3.2010 <- m3[m3$Year == 2010, ]


#calculate national average of crude mortality rate from the m3
temp <- m3.2010 %>% group_by(Disease)  %>% summarise(total_population = sum(Population)) %>% inner_join(m3.2010, by="Disease")
temp$Weighted_CrudeRate <- (temp$Population/temp$total_population) * temp$Crude.Rate

#Add disease_national_avg to our 2010 data (m3.2010)
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
    titlePanel("Crude Mortality Rate Across All States by Causes"),
    sidebarLayout(

        # Input
        sidebarPanel(

            # Select variable for y-axis
            selectInput(inputId = "disease",
                        label = "Select disease:",
                        choices = unique(m3.2010$Disease),
                        selected = "Neoplasms",
                        width = '500px'),

         h4("Top 5 States in 2010 for the Selected Disease"),
         tableOutput('MyRanks'), # Option to display Top 5 States Ranked
         h1('')

        ),

        # Output:
        mainPanel(
            plotOutput(outputId = "bargraph", width = "100%")
        )
    )
)

# Define server function required to create the scatterplot-
server <- function(input, output, session) {

    ordered <- eventReactive(
        eventExpr = input$disease,
        valueExpr = {m3.2010 %>% filter(Disease==input$disease) %>% arrange(desc(Crude.Rate))},
        ignoreNULL = FALSE
    )
    
    selecteddata <- reactive({
                dfSlice <- m3.2010 %>%
                    filter(Disease==input$disease) %>%
                    mutate(Rank=min_rank(Rank)) %>%
                    arrange(Rank)
    })


    #Create bar graph
    output$bargraph <- renderPlot({ggplot(data = ordered(), aes(x=reorder(State,Crude.Rate), y=Crude.Rate)) +
            geom_bar(stat="identity", width=0.7, color="#1F3552", fill="darkred",
                     position=position_dodge()) +
            geom_text(aes(label=round(Crude.Rate, digits=2)), hjust=1.3, size=3.0, color="white") +
            coord_flip() +
            ggtitle("Crude Mortality for Selected Disease for 2010") +
            xlab("") + ylab("") +
            theme_minimal()}, height = 1000, width = 600)
    
    # Generate a summary of the m3
        output$MyRanks <- renderTable({
            # Remove Year and Disease nad return top 5 Ranks 
            head(selecteddata()[,c(-1,-4)],5) 
        })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)


