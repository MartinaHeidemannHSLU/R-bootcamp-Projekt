library(shiny)
library(ggplot2)
library(ggiraph)
library(hrbrthemes)
library(colormap)
library(widgetframe)

map_dam <- read.csv("/Users/milicapajkic/Documents/GitHub/R-bootcamp-Projekt/map_dam.csv")
ui <- fluidPage(
    titlePanel("Interactive Map Example"),
    mainPanel(
        plotOutput("mapPlot", height = "600px")
    )
)

server <- function(input, output) {
    output$mapPlot <- renderPlot({
        g <- ggplot(map_dam) +
            geom_polygon_interactive(
                color='black',
                aes(x = long, y = lat, group = group, fill = damage2,
                    tooltip=sprintf("%s<br/>%s",region,damage))) +
            labs(x = 'Longitude', y = 'Latitude', fill = '$ damage') +
            coord_quickmap() +
            hrbrthemes::theme_ipsum() +
            colormap::scale_fill_colormap(
                colormap=colormap::colormaps$copper, reverse = T) +
            labs(title='Internet Usage in Africa in 2015', subtitle='As Percent of Population',
                 caption='Source: World Bank Open Data.')
        
        g
    })
}


shinyApp(ui = ui, server = server)