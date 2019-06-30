#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(quantmod)
library(PerformanceAnalytics)

# Define UI for application that draws a histogram
ui <- shinyUI(pageWithSidebar(
    
    # Application title
    headerPanel("Value at Risk"),
    
    # Sidebar with a slider input for number of observations
    sidebarPanel(
        selectInput("src", "Online databases :", 
                    choices = c("Yahoo", "FRED")),
        selectInput("perild", "period :", 
                    choices = c("daily", "weekly","monthly")),
        selectInput("method", "method :", 
                    choices = c("historical", "gaussian", "kernel")),
        
        textInput("ticker", label = "Tickers:", value = "000001.SS"),
        textInput("from", label = "Date with form yyyy-mm-dd:", value = "2018-06-28"),
        textInput("to", label = "Date with form yyyy-mm-dd:", value = "today"),
        numericInput("p", "percentile :", 0.95,min = 0,max = 1,step = 0.01)
    ),
    
    # Show a factorial of n
    mainPanel(
        verbatimTextOutput("view"),
        plotOutput("view2")
    )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
    srcInput <- reactive({
        switch(input$src,
               "Yahoo" = "yahoo",
               "FRED" = "FRED")
    })
    periodInput <- reactive({
        switch(input$perild,
               "daily" = "daily",
               "weekly" = "weekly",
               "monthly"="monthly")
    })
    methodInput <- reactive({
        switch(input$method,
               "historical" = "historical",
               "kernel" = "kernel")
    })
    
    output$view <- renderPrint({
        if(input$to=="today")
            SHC <- getSymbols(Symbols = input$ticker, src=srcInput(), from = input$from, auto.assign = F)
        else
            SHC <- getSymbols(Symbols = input$ticker, src=srcInput(), from = input$from, to = input$to, auto.assign = F)
        daily_rets <- periodReturn(SHC,period = periodInput())
        fit <- VaR(daily_rets, p=input$p,method = methodInput())
        cat(fit)
    })
    output$view2 <- renderPlot({
        if(input$to=="today")
            SHC <- getSymbols(Symbols = input$ticker, src=srcInput(), from = input$from, auto.assign = F)
        else
            SHC <- getSymbols(Symbols = input$ticker, src=srcInput(), from = input$from, to = input$to, auto.assign = F)
        daily_rets <- periodReturn(SHC,period = periodInput())
        plot(daily_rets)
    })
})

# Run the application 
shinyApp(ui = ui, server = server)
