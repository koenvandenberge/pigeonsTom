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
library(tidyr)

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
                    tabPanel("Wedstrijd statistieken", plotlyOutput(outputId = "statsPlot")),
                    tabPanel("Coëfficiëntentabel", tableOutput(outputId = "tabel"))
                    )
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
    
    output$statsPlot <- renderPlotly({
        par(mfrow=c(1,2), mar=c(9,5,4,3))
        # Meeste prijzen over alle wedstrijden
        tab <- sort(table(dfSnel$Naam), decreasing=TRUE)[1:5]
        names(tab) <- trimws(names(tab))
        figBar1 <- plot_ly(
            x = factor(names(tab), levels=names(tab)),
            y = tab,
            name = "Prijzen over alle wedstrijden",
            type = "bar")

        #Tom: aantal prijzen per wedstrijd
        prijsPerWedstrijd <- dfTom %>% 
            dplyr::group_by(race) %>% 
            summarise(nr=n(),
                      total=unique(AD))
        figBar2 <- plot_ly(prijsPerWedstrijd, 
                       x = ~race, y = ~nr, type = 'bar', 
                       name = 'Aantal prijzen')
        figBar2 <- figBar2 %>% add_trace(y = ~total-nr, name = 'Aantal duiven mee')
        figBar2 <- figBar2 %>% layout(yaxis = list(title = 'Aantal'), 
                              barmode = 'stack')
        fig <- subplot(figBar1, figBar2)
        fig
    })
    
    output$tabel <- renderTable({
        dfTabLong <- dfTom[,c("Ring", "race", "coef")]
        dfTabWide <- tidyr::spread(dfTabLong, race, coef)
        ## NA betekent dat ze geen prijs gevlogen hebben
        dfTabWide[is.na(dfTabWide)] <- 55
        ### soms zijn er thuis gebleven duiven
        ringenThuisNoyon280720 <- c(410057520, 410055820, 410059220)
        dfTabWide[dfTabWide$Ring %in% ringenThuisNoyon280720,"Noyon 28 jun 2020"] <- NA
        dfTabWide$gemiddelde <- rowMeans(dfTabWide[,-1], na.rm=TRUE)
        dfTabWide <- dfTabWide[order(dfTabWide$gemiddelde, decreasing=FALSE),]
        dfTabWide$Ring <- as.factor(dfTabWide$Ring)
        dfTabWide
    })

}

# Run the application 
shinyApp(ui = ui, server = server)