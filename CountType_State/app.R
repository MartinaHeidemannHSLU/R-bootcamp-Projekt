#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)

df1 <- read.csv("/Users/milicapajkic/Documents/GitHub/R-bootcamp-Projekt/df.group.states.csv")

shinyApp(
    ui = fluidPage(
        selectInput("type", "Select Type:", unique(df1$Type)),
        leafletOutput("map")
    ),
    server = function(input, output) {
        output$map <- renderLeaflet({
            leaflet() %>%
                addTiles() %>%
                addCircleMarkers(
                    data = df1 %>%
                        filter(Type == input$type),
                    lng = ~long,
                    lat = ~lat,
                    popup = ~number_sightings
                )
        })
    }
)

# Run the application 
shinyApp(ui = ui, server = server)