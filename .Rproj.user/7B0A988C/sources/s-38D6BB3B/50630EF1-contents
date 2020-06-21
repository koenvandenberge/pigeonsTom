# a subject is a pigeon, uniquely identified by its number.
# pigeons can be linked to people.
# last two numbers on ring are the year the pigeon was born.

# an initial, simple app would be to plot the speed of each pigeon over the
# different races.

# However, speed depends on the distance.


library(shiny)
library(ggplot2)
library(plotly)
library(magrittr)

source("R/getPigeonsTom.R")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Overzicht van alle duiven"),
    
    selectizeInput(
        inputId = "duifSelected", 
        label = "Selecteer je duiven", 
        choices = unique(dfSnel$duif), 
        selected = unique(dfSnel$duif),
        multiple = TRUE
    ),

    plotlyOutput(outputId = "overviewPlot")

)

# Define server logic required to draw a histogram
server <- function(input, output, ...) {
    
    output$overviewPlot <- renderPlotly({
        dfSnelPlot <- dfSnel[dfSnel$duif %in% input$duifSelected,]
        plot_ly(dfSnelPlot, y = ~snelheid, x= ~race, color = ~duif,
                type = "scatter") 
        })

}

# Run the application 
shinyApp(ui = ui, server = server)
