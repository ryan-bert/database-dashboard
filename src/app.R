library(shiny)
library(shinydashboard)
library(DT)
library(DBI)
library(RPostgres)
library(dplyr)
library(ggplot2)
library(jsonlite)

# Load PostgreSQL credentials
credentials_path <- file.path("~/Documents/Credentials/Raspberry Pi/financial-database.json")
credentials <- fromJSON(credentials_path)

# Connect to the database
conn <- dbConnect(
  Postgres(),
  dbname = credentials$dbname,
  host = credentials$host,
  port = as.integer(credentials$port),
  user = credentials$user,
  password = credentials$password
)

# Load data from the database
stocks_df <- dbGetQuery(conn, "SELECT * FROM equities") %>% mutate(Date = as.Date(Date))
etf_df <- dbGetQuery(conn, "SELECT * FROM etfs") %>% mutate(Date = as.Date(Date))
crypto_df <- dbGetQuery(conn, "SELECT * FROM crypto") %>% mutate(Date = as.Date(Date))
bonds_df <- dbGetQuery(conn, "SELECT * FROM bonds") %>% mutate(Date = as.Date(Date))
future_df <- dbGetQuery(conn, "SELECT * FROM futures") %>% mutate(Date = as.Date(Date))

# Close connection after loading data
dbDisconnect(conn)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Financial Database"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Stocks", tabName = "stocks", icon = icon("chart-line")),
      menuItem("ETFs", tabName = "etfs", icon = icon("chart-bar")),
      menuItem("Crypto", tabName = "crypto", icon = icon("bitcoin")),
      menuItem("Bonds", tabName = "bonds", icon = icon("university")),
      menuItem("Futures", tabName = "futures", icon = icon("money-bill"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "stocks",
              h2("Stock Data"),
              selectInput("stock_ticker", "Select Ticker:", choices = unique(stocks_df$Ticker)),
              plotOutput("stock_plot"),
              DTOutput("stocks_table")
      ),
      tabItem(tabName = "etfs",
              h2("ETF Data"),
              selectInput("etf_ticker", "Select Ticker:", choices = unique(etf_df$Ticker)),
              plotOutput("etf_plot"),
              DTOutput("etfs_table")
      ),
      tabItem(tabName = "crypto",
              h2("Crypto Data"),
              selectInput("crypto_ticker", "Select Ticker:", choices = unique(crypto_df$Ticker)),
              plotOutput("crypto_plot"),
              DTOutput("crypto_table")
      ),
      tabItem(tabName = "bonds",
              h2("Bond Data"),
              selectInput("bonds_ticker", "Select Ticker:", choices = unique(bonds_df$Ticker)),
              plotOutput("bonds_plot"),
              DTOutput("bonds_table")
      ),
      tabItem(tabName = "futures",
              h2("Futures Data"),
              selectInput("futures_ticker", "Select Ticker:", choices = unique(future_df$Ticker)),
              plotOutput("futures_plot"),
              DTOutput("futures_table")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # STOCKS: Filtered Data for Selected Ticker
  stock_filtered <- reactive({
    req(input$stock_ticker)
    stocks_df %>% filter(Ticker == input$stock_ticker)
  })
  
  output$stock_plot <- renderPlot({
    ggplot(stock_filtered(), aes(x = Date, y = Price)) +
      geom_line(color = "blue", size = 1) +
      theme_minimal() +
      labs(title = paste("Stock Price: ", input$stock_ticker), x = "Date", y = "Price")
  })
  
  output$stocks_table <- renderDT({
    datatable(stocks_df, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # ETFs
  etf_filtered <- reactive({
    req(input$etf_ticker)
    etf_df %>% filter(Ticker == input$etf_ticker)
  })
  
  output$etf_plot <- renderPlot({
    ggplot(etf_filtered(), aes(x = Date, y = Price)) +
      geom_line(color = "green", size = 1) +
      theme_minimal() +
      labs(title = paste("ETF Price: ", input$etf_ticker), x = "Date", y = "Price")
  })
  
  output$etfs_table <- renderDT({
    datatable(etf_df, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # CRYPTO
  crypto_filtered <- reactive({
    req(input$crypto_ticker)
    crypto_df %>% filter(Ticker == input$crypto_ticker)
  })
  
  output$crypto_plot <- renderPlot({
    ggplot(crypto_filtered(), aes(x = Date, y = Price)) +
      geom_line(color = "orange", size = 1) +
      theme_minimal() +
      labs(title = paste("Crypto Price: ", input$crypto_ticker), x = "Date", y = "Price")
  })
  
  output$crypto_table <- renderDT({
    datatable(crypto_df, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # BONDS
  bonds_filtered <- reactive({
    req(input$bonds_ticker)
    bonds_df %>% filter(Ticker == input$bonds_ticker)
  })
  
  output$bonds_plot <- renderPlot({
    ggplot(bonds_filtered(), aes(x = Date, y = Price)) +
      geom_line(color = "purple", size = 1) +
      theme_minimal() +
      labs(title = paste("Bond Price: ", input$bonds_ticker), x = "Date", y = "Price")
  })
  
  output$bonds_table <- renderDT({
    datatable(bonds_df, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # FUTURES
  futures_filtered <- reactive({
    req(input$futures_ticker)
    future_df %>% filter(Ticker == input$futures_ticker)
  })
  
  output$futures_plot <- renderPlot({
    ggplot(futures_filtered(), aes(x = Date, y = Price)) +
      geom_line(color = "red", size = 1) +
      theme_minimal() +
      labs(title = paste("Futures Price: ", input$futures_ticker), x = "Date", y = "Price")
  })
  
  output$futures_table <- renderDT({
    datatable(future_df, options = list(pageLength = 10, scrollX = TRUE))
  })
}

# Run the app
shinyApp(ui, server)