# a subject is a pigeon, uniquely identified by its ring number.
# pigeons can be linked to people.
# last two numbers on ring are the year the pigeon was born.

## since for young pigeons, speed increases as the season continues,
## two panels are given: one on absolute speed, and one on speed as
## compared to the top X (50 by default) pigeons, in each race.

library(shiny)
library(ggplot2)
library(plotly)
library(magrittr)
library(lubridate)
library(dplyr)
library(shinyWidgets)

source("R/getPigeonsTom.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    setBackgroundImage(
        src = "tom.jpg"
    ),
    
    # Application title
    titlePanel("Overzicht van alle duiven"),
    
    sidebarPanel(
    
    sidebarLayout(
        selectizeInput(
            inputId = "duifSelected", 
            label = "Selecteer je duiven", 
            choices = unique(dfTom$duif), 
            selected = unique(dfTom$duif),
            multiple = TRUE
        ),
    
        sliderInput("nTop",
                    "Relatieve snelheid: aantal top duiven waarmee vergeleken wordt",
                    value = 50,
                    min = 5,
                    max = 90)
        )),


    mainPanel(
        tabsetPanel(type="tabs",
                    tabPanel("Absolute snelheid", plotlyOutput(outputId = "absolutePlot")),
                    tabPanel("Relatieve snelheid", plotlyOutput(outputId = "relativePlot")),
                    tabPanel("Wedstrijd statistieken", plotOutput(outputId = "statsPlot"))                    )
    )
    

    #plotlyOutput(outputId = "overviewPlot")

)

# Define server logic required to draw a histogram
server <- function(input, output, ...) {
    
    output$absolutePlot <- renderPlotly({
        dfTomPlot <- dfTom[dfTom$duif %in% input$duifSelected,]
        plot_ly(dfTomPlot, y = ~snelheidIk, x= ~race, color = ~duif,
                type = "scatter") %>% 
            add_trace(y = ~snelheidIk, name = NULL, mode = 'lines+markers') %>% 
            layout(xaxis = list(title="Wedstrijd"), 
                   yaxis = list(title="Snelheid"))
        })
    
    output$relativePlot <- renderPlotly({
        ## get avg speed of top
        topSpeeds <- dfSnel %>%
            group_by(race) %>% 
            top_n(n=input$nTop) %>%
            summarize(avSpeed=mean(snelheidIk))
        
        dfTomPlot <- dfTom[dfTom$duif %in% input$duifSelected,]
        dfTomPlot <- dfTomPlot %>% left_join(., topSpeeds)
        dfTomPlot$relativeSpeed <- dfTomPlot$snelheidIk / dfTomPlot$avSpeed
        plot_ly(dfTomPlot, y = ~relativeSpeed, x= ~race, color = ~duif,
                type = "scatter") %>% 
            add_trace(y = ~relativeSpeed, name = NULL, mode = 'lines+markers') %>% 
            layout(xaxis = list(title="Wedstrijd"), 
                   yaxis = list(title="Relatieve snelheid"))
    })
    
    output$statsPlot <- renderPlot({
        par(mfrow=c(1,2), mar=c(9,5,4,3))
        # Meeste prijzen over alle wedstrijden
        tab <- sort(table(dfSnel$Naam), decreasing=TRUE)[1:5]
        names(tab) <- trimws(names(tab))
        barplot(tab, las=2,
                ylab = "Aantal prijzen", main="Aantal prijzen over alle wedstrijden heen",
                cex.axis = 1, cex.names=1, cex.main=5/4,
                col=c("darkseagreen2", rep("grey",4)))
        
        #Tom: aantal prijzen per wedstrijd
        aantalWedstrijden <- dfTom %>% dplyr::group_by(race) %>% summarise(nr=n())
        barplot(height=aantalWedstrijden$nr, names=aantalWedstrijden$race,
                las=2, cex.axis = 1, cex.names=1, cex.main=5/4,
                main="Prijzen per wedstrijd voor Tom", ylab="Aantal prijzen")
    })

}

# Run the application 
shinyApp(ui = ui, server = server)