library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)

# Read in the data
df <- read.csv("df_prep_5.csv")

# Define UI
ui <- fluidPage(
  
  # Add inputs for choosing the variable to plot
  selectInput("variable", "Variable:",
              choices = c("Approved", "Damage", "IncomeCapita", "Population")),
  
  # Add leaflet map
  leafletOutput("map")
)

# Define server
server <- function(input, output) {
  
  # Create reactive data for selected variable
  filteredData <- reactive({
    df %>% select(Longitude, Latitude, !!input$variable)
  })
  
  # Create the map with heat
  output$map <- renderLeaflet({
    leaflet(filteredData()) %>%
      addTiles() %>%
      addHeatmap(lng = ~Longitude, lat = ~Latitude, intensity = ~!!input$variable)
  })
  
}

# Run the app
shinyApp(ui, server)

