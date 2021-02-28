# Business Analytics with Data Science and Machine Learning ----
# Building Business Data Products ----
# STOCK ANALYZER APP - LAYOUT -----

# LIBRARIES ----
library(shiny)
library(shinyWidgets)
library(lubridate)
library(plotly)
library(tidyverse)
library(quantmod)
library(rvest)
library(glue)

source(file = "00_scripts/stock_analysis_functions.R")

# UI ----
ui <- fluidPage(title = "Stock Analyzer",
    # 1.0 HEADER ----
    div(
      h1("Stock Analyzer"),
    ),
    # 2.0 APPLICATION UI -----
    div(
      column(
        width = 4,
        wellPanel(
          
          p("Stock Index"),
          pickerInput(inputId = "indices",
                      choices = c("dax", "sp500", "dow", "nasdaq")),
          p("Stocks"),
          uiOutput("Stocks"),
          hr(),
          dateRangeInput(inputId = "date_range", 
                         label   = h4("Date Range"), 
                         start   = "2018-01-01", 
                         end     = today(),       
                         min     = "2018-01-01", 
                         max     = today(), 
                         startview = "year"),
          actionButton(inputId = "analyze", 
                       label   = "Analyze",
                       icon    = icon("download")),
          p(textOutput(outputId = "selected_symbol")),
          hr(),
          sliderInput(inputId = "mavg_short_slider", 
                      label   = h5("Short Moving Average"), 
                      min     = 5,
                      max     = 40, 
                      value   = c(20), 
                      step    = 1),
          
          sliderInput(inputId = "mavg_long_slider", 
                      label   = h5("Long Moving Average"), 
                      min     = 50,
                      max     = 120, 
                      value   = c(50), 
                      step    = 1)
          
        )
      ), 
      column(
        width = 8,
        div(
          
          h2(textOutput(outputId = "plot_header"))
          
        ),
        
        div(
          
          plotlyOutput("plotly_plot")
        
          )
      )
    ),
    # 3.0 ANALYST COMMENTARY ----
    div(
      column
      (
        width = 12,
        
        h3("Analyst Commentary"),
        textOutput(outputId = "analyst_commentary")
      )
      
    )
)

# SERVER ----
server <- function(input, output, session) {

  # Create stock list ----    
  output$Stocks <- renderUI({
    stock_list_tbl <- get_stock_list(input$indices)
    pickerInput(inputId = "stock_selection", 
                choices = stock_list_tbl$label)
  })
  
  # Stock Symbol ----
  stock_symbol <- eventReactive(input$analyze, {
    get_symbol_from_user_input(input$stock_selection)
  })
  
  output$selected_symbol <- renderText({stock_symbol()})
  
  #Plot Header ---------
  header_plot <- eventReactive(input$analyze, {
    input$stock_selection
  }, ignoreNULL = FALSE)
  
  output$plot_header <- renderText({header_plot()})
  
  # Get Stock Data ----
  stock_data_tbl <- reactive({
    stock_symbol() %>% 
      get_stock_data(from = input$date_range[1], 
                     to   = input$date_range[2],
                     mavg_short = input$mavg_short_slider,
                     mavg_long  = input$mavg_long_slider)
    
  })
  
  #Plot Data
  output$plotly_plot <- renderPlotly({stock_data_tbl()} %>% plot_stock_data()) 
  
  # Analysis commentary
  stock_symbol_value<-renderText(stock_symbol())
  
  output$analyst_commentary <- renderText(generate_commentary(stock_data_tbl() ,stock_symbol_value()))

}

# RUN APP ----
shinyApp(ui = ui, server = server)