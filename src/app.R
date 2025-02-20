library(shiny)
library(DBI)
library(RPostgres)
library(jsonlite)
library(dplyr)
library(DT)
library(ggplot2)
library(plotly)

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

# Load and format data
equities_df <- dbGetQuery(conn, "SELECT * FROM equities") %>%
  mutate(
    Price = round(Price, 2),
    Return = round(Return, 6),
    Date = as.Date(Date)
  )

etfs_df <- dbGetQuery(conn, "SELECT * FROM etfs") %>%
  mutate(
    Price = round(Price, 2),
    Return = round(Return, 6),
    Date = as.Date(Date)
  )

crypto_df <- dbGetQuery(conn, "SELECT * FROM crypto") %>%
  mutate(
    Price = round(Price, 2),
    Return = round(Return, 6),
    Date = as.Date(Date)
  )

bonds_df <- dbGetQuery(conn, "SELECT * FROM bonds") %>%
  mutate(
    Yield = round(Yield, 2),
    Date = as.Date(Date)
  )

futures_df <- dbGetQuery(conn, "SELECT * FROM futures") %>%
  mutate(
    Price = round(Price, 2),
    Return = round(Return, 6),
    Date = as.Date(Date)
  )

forex_df <- dbGetQuery(conn, "SELECT * FROM forex") %>%
  mutate(
    Price = round(Price, 2),
    Return = round(Return, 6),
    Date = as.Date(Date)
  )

# Define custom CSS class for navigation underline
my_underline <- "relative after:absolute after:left-0 after:right-0 after:bottom-[-4px] after:h-[3px] after:bg-white after:scale-x-0 hover:after:scale-x-100 after:origin-left after:transition-transform after:duration-500"

ui <- tagList(
  tags$head(
    tags$script(src = "https://cdn.tailwindcss.com"),
    tags$style(HTML("
      table.dataTable {
        background-color: #222 !important;
        color: white !important;
        margin-left: 0 !important;
      }
      thead {
        background-color: #333 !important;
        color: #fff !important;
      }
      tbody tr {
        background-color: #222 !important;
      }
      tbody tr:hover {
        background-color: #444 !important;
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button {
        color: white !important;
      }
      .dataTables_wrapper .dataTables_filter {
          text-align: left !important; 
          display: flex !important; 
          align-items: center !important; 
          justify-content: flex-start !important; 
          gap: 8px !important;
          width: 100% !important;
          margin-left: 0 !important;
      }
      .dataTables_wrapper .dataTables_filter label {
        color: white !important;
        font-size: 14px !important;
      }
      .dataTables_wrapper .dataTables_filter input {
        background-color: #333 !important;
        color: white !important;
        border: 1px solid #555 !important;
        font-size: 12px !important;
      }
      .dataTables_wrapper .dataTables_length label {
        color: white !important;
        font-size: 14px !important;
      }
      .dataTables_wrapper .dataTables_length select {
        background-color: #333 !important;
        color: white !important;
      }
      .dataTables_wrapper .dataTables_paginate {
        margin-top: 1rem !important;
        color: white !important;
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button {
        padding: 0.3em 0.8em !important;
        border: 1px solid #555 !important;
        background: #333 !important;
        color: white !important;
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button.current {
        background: #555 !important;
      }
      .dataTables_wrapper .dataTables_info {
        color: white !important;
      }
    ")),
    tags$script(HTML("Shiny.setInputValue('page', 'equities', {priority: 'event'});"))
  ),
  
  tags$div(class = "bg-black text-white min-h-screen",
    tags$div(class = "text-lg flex justify-center py-2 w-full",
      tags$div(class = "space-x-6",
        tags$a(href = "#", class = my_underline, "Equities", onclick = "Shiny.setInputValue('page', 'equities', {priority: 'event'})"),
        tags$a(href = "#", class = my_underline, "ETFs", onclick = "Shiny.setInputValue('page', 'etfs', {priority: 'event'})"),
        tags$a(href = "#", class = my_underline, "Crypto", onclick = "Shiny.setInputValue('page', 'crypto', {priority: 'event'})"),
        tags$a(href = "#", class = my_underline, "Bonds", onclick = "Shiny.setInputValue('page', 'bonds', {priority: 'event'})"),
        tags$a(href = "#", class = my_underline, "Futures", onclick = "Shiny.setInputValue('page', 'futures', {priority: 'event'})"),
        tags$a(href = "#", class = my_underline, "Forex", onclick = "Shiny.setInputValue('page', 'forex', {priority: 'event'})")
      )
    ),
    
    tags$div(class = "p-8 flex justify-start", uiOutput("page_content"))
  )
)

server <- function(input, output, session) {
  
  # Set default page if none is provided
  observe({
    if (is.null(input$page)) {
      updateQueryString("?page=equities", mode = "replace", session)
    }
  })
  
  # Render the page content based on the selected page
  output$page_content <- renderUI({
    page <- input$page %||% "equities"
    
    if (page == "equities") {
      tagList(
        h1("Equities Page", class = "text-3xl"),
        selectInput("equities_ticker", "Select Ticker", choices = unique(equities_df$ticker)),
        plotlyOutput("equities_plot"),
        DTOutput("equities_table")
      )
    } else if (page == "etfs") {
      tagList(
        h1("ETFs Page", class = "text-3xl"),
        selectInput("etfs_ticker", "Select Ticker", choices = unique(etfs_df$ticker)),
        plotlyOutput("etfs_plot"),
        DTOutput("etfs_table")
      )
    } else if (page == "crypto") {
      tagList(
        h1("Crypto Page", class = "text-3xl"),
        selectInput("crypto_ticker", "Select Ticker", choices = unique(crypto_df$ticker)),
        plotlyOutput("crypto_plot"),
        DTOutput("crypto_table")
      )
    } else if (page == "bonds") {
      tagList(
        h1("Bonds Page", class = "text-3xl"),
        selectInput("bonds_ticker", "Select Ticker", choices = unique(bonds_df$ticker)),
        plotlyOutput("bonds_plot"),
        DTOutput("bonds_table")
      )
    } else if (page == "futures") {
      tagList(
        h1("Futures Page", class = "text-3xl"),
        selectInput("futures_ticker", "Select Ticker", choices = unique(futures_df$ticker)),
        plotlyOutput("futures_plot"),
        DTOutput("futures_table")
      )
    } else if (page == "forex") {
      tagList(
        h1("Forex Page", class = "text-3xl"),
        selectInput("forex_ticker", "Select Ticker", choices = unique(forex_df$ticker)),
        plotlyOutput("forex_plot"),
        DTOutput("forex_table")
      )
    }
  })
  
  # Create reactive plot outputs for each page
  
  output$equities_plot <- renderPlotly({
    req(input$equities_ticker)
    data_sub <- equities_df %>% filter(ticker == input$equities_ticker)
    p <- ggplot(data_sub, aes(x = Date, y = Price)) +
      geom_line(color = "cyan") +
      labs(title = paste("Price vs Date for", input$equities_ticker),
           x = "Date", y = "Price") +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "#222", color = "#222"),
            panel.background = element_rect(fill = "#222"),
            text = element_text(color = "white"),
            axis.text = element_text(color = "white"),
            axis.title = element_text(color = "white"))
    ggplotly(p)
  })
  
  output$etfs_plot <- renderPlotly({
    req(input$etfs_ticker)
    data_sub <- etfs_df %>% filter(ticker == input$etfs_ticker)
    p <- ggplot(data_sub, aes(x = Date, y = Price)) +
      geom_line(color = "cyan") +
      labs(title = paste("Price vs Date for", input$etfs_ticker),
           x = "Date", y = "Price") +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "#222", color = "#222"),
            panel.background = element_rect(fill = "#222"),
            text = element_text(color = "white"),
            axis.text = element_text(color = "white"),
            axis.title = element_text(color = "white"))
    ggplotly(p)
  })
  
  output$crypto_plot <- renderPlotly({
    req(input$crypto_ticker)
    data_sub <- crypto_df %>% filter(ticker == input$crypto_ticker)
    p <- ggplot(data_sub, aes(x = Date, y = Price)) +
      geom_line(color = "cyan") +
      labs(title = paste("Price vs Date for", input$crypto_ticker),
           x = "Date", y = "Price") +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "#222", color = "#222"),
            panel.background = element_rect(fill = "#222"),
            text = element_text(color = "white"),
            axis.text = element_text(color = "white"),
            axis.title = element_text(color = "white"))
    ggplotly(p)
  })
  
  output$bonds_plot <- renderPlotly({
    req(input$bonds_ticker)
    data_sub <- bonds_df %>% filter(ticker == input$bonds_ticker)
    # If bonds_df does not have Price, consider plotting Yield instead
    p <- ggplot(data_sub, aes(x = Date, y = Yield)) +
      geom_line(color = "cyan") +
      labs(title = paste("Yield vs Date for", input$bonds_ticker),
           x = "Date", y = "Yield") +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "#222", color = "#222"),
            panel.background = element_rect(fill = "#222"),
            text = element_text(color = "white"),
            axis.text = element_text(color = "white"),
            axis.title = element_text(color = "white"))
    ggplotly(p)
  })
  
  output$futures_plot <- renderPlotly({
    req(input$futures_ticker)
    data_sub <- futures_df %>% filter(ticker == input$futures_ticker)
    p <- ggplot(data_sub, aes(x = Date, y = Price)) +
      geom_line(color = "cyan") +
      labs(title = paste("Price vs Date for", input$futures_ticker),
           x = "Date", y = "Price") +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "#222", color = "#222"),
            panel.background = element_rect(fill = "#222"),
            text = element_text(color = "white"),
            axis.text = element_text(color = "white"),
            axis.title = element_text(color = "white"))
    ggplotly(p)
  })
  
  output$forex_plot <- renderPlotly({
    req(input$forex_ticker)
    data_sub <- forex_df %>% filter(ticker == input$forex_ticker)
    p <- ggplot(data_sub, aes(x = Date, y = Price)) +
      geom_line(color = "cyan") +
      labs(title = paste("Price vs Date for", input$forex_ticker),
           x = "Date", y = "Price") +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "#222", color = "#222"),
            panel.background = element_rect(fill = "#222"),
            text = element_text(color = "white"),
            axis.text = element_text(color = "white"),
            axis.title = element_text(color = "white"))
    ggplotly(p)
  })
  
  # Render Data Tables
  output$equities_table <- renderDT({
    datatable(equities_df, options = list(pageLength = 50, dom = "lftip"), rownames = FALSE)
  })
  
  output$etfs_table <- renderDT({
    datatable(etfs_df, options = list(pageLength = 50, dom = "lftip"), rownames = FALSE)
  })
  
  output$crypto_table <- renderDT({
    datatable(crypto_df, options = list(pageLength = 50, dom = "lftip"), rownames = FALSE)
  })
  
  output$bonds_table <- renderDT({
    datatable(bonds_df, options = list(pageLength = 50, dom = "lftip"), rownames = FALSE)
  })
  
  output$futures_table <- renderDT({
    datatable(futures_df, options = list(pageLength = 50, dom = "lftip"), rownames = FALSE)
  })
  
  output$forex_table <- renderDT({
    datatable(forex_df, options = list(pageLength = 50, dom = "lftip"), rownames = FALSE)
  })
}

shinyApp(ui, server)