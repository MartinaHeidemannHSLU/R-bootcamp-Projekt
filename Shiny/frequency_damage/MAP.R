library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(maps)
library(scales)

# Read in the data
df <- read.csv("df_prep_5.csv")

# Create county map data
map_dam <- map_data("county")

# Merge damage data with county map data
map_dam <- left_join(map_dam, df %>% 
                       group_by(County) %>% 
                       summarize(total_damage = sum(Damage)) %>% 
                       select(County, total_damage), 
                     by = c("region" = "County"))

# Define UI
ui <- fluidPage(
  
  # Add inputs for choosing the variable to plot
  selectInput("variable", "Variable:",
              choices = c("Approved", "Damage", "IncomeCapita", "Population")),
  
  # Add leaflet map
  leafletOutput("map"),
  
  # Add ggplot2 choropleth map
  plotOutput("plot")
)

server <- function(input, output) {
  
  # Create reactive data for selected variable
  filteredData <- reactive({
    df %>% select(Longitude, Latitude, !!input$variable)
  })
  
  # Create the map with heat
  output$map <- renderLeaflet({
    leaflet(filteredData()) %>%
      addTiles() %>%
      addHeatmap(lng = ~Longitude, lat = ~Latitude, 
                 intensity = filteredData()[[2]], blur = 20, max = 0.05) %>%
      addLegend(pal = colorNumeric(palette = "YlOrRd", domain = filteredData()[[2]]), 
                values = filteredData()[[2]], position = "bottomright")
  })
}

shinyApp(ui, server)