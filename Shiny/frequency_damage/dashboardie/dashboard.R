library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(maps)
library(scales)

df.prep.5 <- read_csv('df_prep_5.csv')

# Define UI
ui <- fluidPage(
  titlePanel("Natural Disaster Dashboard"),
  tabsetPanel(
    tabPanel("Disaster Plot", fluidPage(
      fluidRow(
        column(width = 3,
               selectInput("disasterType", "Disaster Type:", unique(df.prep.5$Type)),
               selectInput("state", "State:", NULL),
               selectInput("county", "County:", NULL)
        ),
        column(width = 9,
               plotOutput("disasterPlot")
        )
      )
    )),
    tabPanel("Heatmap", fluidPage(
      selectInput("variable", "Variable:",
                  choices = c("Approved", "Damage", "IncomeCapita", "Population")),
      leafletOutput("map"),
      plotOutput("plot")
    ))
  )
)

# Define server
server <- function(input, output, session) {
  
  # Filter possible states based on selected disaster type
  observeEvent(input$disasterType, {
    filteredStates <- unique(df.prep.5 %>% 
                               filter(Type == input$disasterType) %>% 
                               pull(State))
    updateSelectInput(session, "state", choices = filteredStates)
    updateSelectInput(session, "county", choices = NULL)
  })
  
  # Filter possible counties based on selected disaster type and state
  observeEvent(c(input$disasterType, input$state), {
    filteredCounties <- unique(df.prep.5 %>% 
                                 filter(Type == input$disasterType, State == input$state) %>% 
                                 pull(County))
    updateSelectInput(session, "county", choices = filteredCounties)
  })
  
  # Filter data based on selected disaster type, state, and county
  filteredData <- reactive({
    df.prep.5 %>% 
      filter(Type == input$disasterType, State == input$state, County == input$county)
  })
  
  # Render disaster plot
  output$disasterPlot <- renderPlot({
    if (nrow(filteredData()) == 0) {
      # If there is no data for the selected filters, display a message
      plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), ann = FALSE)
      text(0.5, 0.5, "No data for selected filters.", cex = 1.5)
    } else {
      # If there is data, plot it
      ggplot(filteredData(), aes(x = DisasterBegin, xend = DisasterEnd, y = Type, yend = Type)) +
        geom_segment(size = 5, color = "cyan3") +
        labs(x = 'Time', y = 'Disaster Type') +
        ggtitle(paste(input$county, input$state, input$disasterType, sep = " - "))
    }
  })
  
  # Create reactive data for selected variable
  filteredData2 <- reactive({
    df.prep.5 %>% select(Longitude, Latitude, !!input$variable)
  })
  
  # Create the heatmap
  output$map <- renderLeaflet({
    leaflet(filteredData2()) %>%
      addTiles() %>%
      addHeatmap(lng = ~Longitude,
                 lat = ~Latitude,
                 intensity = ~filteredData2()[[3]], 
                 blur = 20, max = 0.05)
  })
}
#Run the app
shinyApp(ui, server)




