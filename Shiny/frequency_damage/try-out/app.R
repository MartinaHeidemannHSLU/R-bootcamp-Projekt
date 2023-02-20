#this works as a heatmap
library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)
library(leaflet.extras)

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
      addHeatmap(lng = ~Longitude, lat = ~Latitude, filteredData()[[2]], blur = 20, max = 0.05) %>%
      addLegend("bottomright", 
                pal = colorNumeric(palette = "YlOrRd", domain = filteredData()[[2]]),
                values = filteredData()[[2]],
                title = input$variable,
                labFormat = labelFormat(suffix = ""),
                opacity = 1)
  })
  
  # Create a normal map with a legend of intensity
  output$plot <- renderPlot({
    ggplot() +
      geom_polygon(map_dam, mapping = aes(x = long, y = lat, group = group, fill = damage2)) +
      labs(x = 'Longitude', y = 'Latitude', fill = '$ damage') +
      coord_quickmap() +
      scale_fill_gradient(limits = c(min(map_dam$damage2), max(map_dam$damage2)),
                          breaks = c(min(map_dam$damage2), max(map_dam$damage2)),
                          labels = c("Low", "High"), na.value = "grey50")
  })
  
}

# Run the app
shinyApp(ui, server)