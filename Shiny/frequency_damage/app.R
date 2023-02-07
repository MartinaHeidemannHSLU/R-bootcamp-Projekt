library(shiny)
library(ggplot2)

ui <- fluidPage(
    selectInput("disasterType", "Disaster Type:", unique(df.prep.5$Type)),
    plotOutput("disasterPlot")
)

server <- function(input, output) {
    filteredData <- reactive({
        df.prep.5[df.prep.5$Type == input$disasterType, ]
    })
    
    output$disasterPlot <- renderPlot({
        ggplot(filteredData(), aes(x = DisasterBegin, xend = DisasterEnd, y = Type, yend = Type)) +
            geom_segment(size = 5, color = "cyan3") +
            labs(x = 'Time', y = 'Disaster Type')
    })
}

shinyApp(ui, server)

