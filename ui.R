
library(shiny)

# Define UI for application that draws the forecasts
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Unemployment rate by gender in Colombia forecasting"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            h5("The International Labour Organization claims that the current global labour force 
            participation rate for women is close to 49%. For men, it's 75% which represents a 
            difference in 26 percent points. While the gap in Colombia is 5 percent points
            on average, this phenomena is deepened in situations of economic uncertainty, when general unemployment
            increases."),
            h5("This app allows you to forecast the unemployment rates by gender in Colombia, where 
            historical data is available until May, 2020. You can decide the number of months ahead to forecast and
            which gender's unemployment rates you want to see!"),
            sliderInput("sliderHorizon",
                        "What time horizon do you want to forecast (months)?",
                        min = 1,
                        max = 12,
                        value = 1), 
            checkboxInput("WomenModel", "Show/Hide Women Series", value = T), 
            checkboxInput("MenModel", "Show/Hide Men Series", value = T),
            h6("Note: Please be patient, results take about 1 min to be shown. Thanks!")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("plot1"), 
            h3("Forecasted women unemployment rate:"), 
            textOutput("pred1"), 
            h3("Forecasted men unemployment rate:"), 
            textOutput("pred2")
        )
    )
))
