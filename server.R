### UNEMPLOYMENT RATES BY GENDER FORECAST ###

# Charge libraries
library(shiny)
suppressMessages(library(data.table))  ## data table language
suppressMessages(library(openxlsx))    ## reads Excel files
suppressMessages(library(forecast))    ## allows time series forecasting
suppressMessages(library(stats))       ## for time series treatment

# Define server logic 
shinyServer(function(input, output) {
    
    dirname <-  '~/data_science_JHU/4-Developing_Data_Products/project/ShinyAppProject'
    if (!dir.exists(dirname))dir.create(dirname,recursive=TRUE)
    
    # read data
    un_data <- as.data.table(read.xlsx("unemployment_gender.xlsx"))

    # fix date format
    un_data[, date := as.Date(date, origin = "1899-12-30")]
    
    # create dummy indicating months where COVID is active in the economy
    un_data[, covid := ifelse(date >= '2020-03-01', 1, 0)]
    
    # first convert women unemployment rate data to time series
    women_ts <- stats::ts(data = un_data$un_women, start = c(2005, 1), frequency = 12)

    # now convert men unemployment rate data to time series
    men_ts <- stats::ts(data = un_data$un_men, start = c(2005, 1), frequency = 12)
    
    # exogenous variable (dummy)
    xreg <- as.matrix(data.frame(covid = un_data$covid))
    
    
    # women model
    w_model <- forecast::auto.arima(women_ts, xreg = xreg)
    
    # men model
    m_model <- forecast::auto.arima(men_ts, xreg = xreg)
    
    dt_wforecast <- reactive({
        th_input <- input$sliderHorizon *31
        
        dt_forecast <- data.table(date = seq(from = max(un_data$date), 
                                          to = max(un_data$date) + th_input,
                                          by = "month"))
        xreg <- as.matrix(data.frame(covid = ifelse(dt_forecast[2:nrow(dt_forecast),]$date >=  '2020-05-01' & 
                                                        dt_forecast[2:nrow(dt_forecast),]$date <=  '2020-08-01', 1, 0)))
        pronostico1 <- forecast(w_model, nrow(dt_forecast)-1 , level = 68, xreg = xreg)
        
        # create forecast table 
        dt_pronostico1 <- data.table(date = dt_forecast[-1,]$date, 
                                     base = as.numeric(pronostico1$mean))
        as.numeric(dt_pronostico1[th_input/31, 2])
        })
    
    
    dt_mforecast <- reactive({
        th_input <- input$sliderHorizon *31
        
        dt_forecast <- data.table(date = seq(from = max(un_data$date), 
                               to = max(un_data$date) + th_input,
                               by = "month"))
        xreg <- as.matrix(data.frame(covid = ifelse(dt_forecast[2:nrow(dt_forecast),]$date >=  '2020-05-01' & 
                                                        dt_forecast[2:nrow(dt_forecast),]$date <=  '2020-08-01', 1, 0)))        
        pronostico2 <- forecast(m_model, nrow(dt_forecast)-1 , level = 68, xreg = xreg)
        
        # create forecast table
        dt_pronostico2 <- data.table(date = dt_forecast[-1,]$date, 
                                     base = as.numeric(pronostico2$mean))
        as.numeric(dt_pronostico2[th_input/31, 2])
        })
    
    
    updateplot <- reactive({
        th_input <- input$sliderHorizon *31
        
        dt_forecast <- data.table(date = seq(from = max(un_data$date), 
                                             to = max(un_data$date) + th_input,
                                             by = "month"))
        xreg <- as.matrix(data.frame(covid = ifelse(dt_forecast[2:nrow(dt_forecast),]$date >=  '2020-05-01' & 
                                                        dt_forecast[2:nrow(dt_forecast),]$date <=  '2020-08-01', 1, 0)))
        pronostico1 <- forecast(w_model, nrow(dt_forecast)-1 , level = 68, xreg = xreg)
        
        # create forecast table 
        dt_pronostico1 <- data.table(date = dt_forecast[-1,]$date, 
                                     base = as.numeric(pronostico1$mean))
        
        x <- data.frame(date = max(un_data$date), 
                        base = last(un_data$un_women))
        
        dt_pronostico1 <- rbind(x, dt_pronostico1)
        
        
        
        
        dt_forecast <- data.table(date = seq(from = max(un_data$date), 
                                             to = max(un_data$date) + th_input,
                                             by = "month"))
        xreg <- as.matrix(data.frame(covid = ifelse(dt_forecast[2:nrow(dt_forecast),]$date >=  '2020-05-01' & 
                                                        dt_forecast[2:nrow(dt_forecast),]$date <=  '2020-08-01', 1, 0)))        
        pronostico2 <- forecast(m_model, nrow(dt_forecast)-1 , level = 68, xreg = xreg)
        
        # create forecast table
        dt_pronostico2 <- data.table(date = dt_forecast[-1,]$date, 
                                     base = as.numeric(pronostico2$mean))
        
        x <- data.frame(date = max(un_data$date), 
                        base = last(un_data$un_men))
        
        dt_pronostico2 <- rbind(x, dt_pronostico2)
        
        unplot <- ggplot2::ggplot() + ggplot2::theme_minimal()
        
        if(input$WomenModel){
            unplot <- unplot + 
                ggplot2::geom_line(data = un_data[157:nrow(un_data),], 
                                   ggplot2::aes(x = date, y = un_women, col = "Women historic"), size = 1.2) + 
                ggplot2::geom_line(data = dt_pronostico1, 
                                   ggplot2::aes(x = date, y = base, col = "Women forecast"), size = 1.2)
            
            unplot
        }
        if(input$MenModel){
            unplot <- unplot +
                ggplot2::geom_line(data = un_data[157:nrow(un_data),], 
                                   ggplot2::aes(x = date, y = un_men, col = "Men historic"), size = 1.2) + 
                ggplot2::geom_line(data = dt_pronostico2, 
                                   ggplot2::aes(x = date, y = base, col = "Men forecast"), size = 1.2)
            unplot
        }
        
        unplot <- unplot + ggplot2::labs(x = "Date", y = "Unemployment rate", colour = NULL) + 
            ggplot2::scale_colour_manual(name = '', values =c('blue2', 'darkgrey', 'salmon', 'grey25')) +
            ggplot2::theme(legend.position = "bottom", 
                           plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 20), 
                           axis.title = ggplot2::element_text(size = 15), 
                           axis.text = ggplot2::element_text(size = 15), 
                           legend.text = ggplot2::element_text(size = 15))
        unplot
        
             })
    
    
    output$plot1 <- renderPlot({
        updateplot()
        
    })
    
    output$pred1 <- renderText({
        dt_wforecast()
    })
    
    output$pred2 <- renderText({
        dt_mforecast()
    })
    
    
    })

