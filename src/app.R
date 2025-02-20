library(shiny)
library(DBI)
library(RPostgres)
library(jsonlite)
library(dplyr)
library(DT)  # Interactive tables

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

# Load stock data
equities_df <- dbGetQuery(conn, "SELECT * FROM equities") %>%
  mutate(
    Price = round(Price, 2),
    Return = round(Return, 6),
    Date = as.Date(Date)
  )

# Load ETF data
etfs_df <- dbGetQuery(conn, "SELECT * FROM etfs") %>%
  mutate(
    Price = round(Price, 2),
    Return = round(Return, 6),
    Date = as.Date(Date)
  )

# Load crypto data
crypto_df <- dbGetQuery(conn, "SELECT * FROM crypto") %>%
  mutate(
    Price = round(Price, 2),
    Return = round(Return, 6),
    Date = as.Date(Date)
  )

# Load bond data
bonds_df <- dbGetQuery(conn, "SELECT * FROM bonds") %>%
  mutate(
    Yield = round(Yield, 2),
    Date = as.Date(Date)
  )

# Load futures data
futures_df <- dbGetQuery(conn, "SELECT * FROM futures") %>%
  mutate(
    Price = round(Price, 2),
    Return = round(Return, 6),
    Date = as.Date(Date)
  )

# Load forex data
forex_df <- dbGetQuery(conn, "SELECT * FROM forex") %>%
  mutate(
    Price = round(Price, 2),
    Return = round(Return, 6),
    Date = as.Date(Date)
  )

# Define custom classes
my_underline <- "relative after:absolute after:left-0 after:right-0 after:bottom-[-4px] after:h-[3px] after:bg-white after:scale-x-0 hover:after:scale-x-100 after:origin-left after:transition-transform after:duration-500"

ui <- tagList(
  tags$head(
    tags$script(src = "https://cdn.tailwindcss.com"),
    
    # Custom Styling
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
    
    tags$div(class = "p-8 flex justify-start", uiOutput("page_content")) # Centering the tables
  )
)

server <- function(input, output, session) {
  
  observe({
    if (is.null(input$page)) {
      updateQueryString("?page=equities", mode = "replace", session)
    }
  })
  
  # Render the correct table based on the selected page
  output$page_content <- renderUI({
    page <- input$page %||% "equities"
    
    if (page == "equities") {
      tagList(
        h1("Equities Page", class = "text-3xl"),
        DTOutput("equities_table")
      )
    } else if (page == "etfs") {
      tagList(
        h1("ETFs Page", class = "text-3xl"),
        DTOutput("etfs_table")
      )
    } else if (page == "crypto") {
      tagList(
        h1("Crypto Page", class = "text-3xl"),
        DTOutput("crypto_table")
      )
    } else if (page == "bonds") {
      tagList(
        h1("Bonds Page", class = "text-3xl"),
        DTOutput("bonds_table")
      )
    } else if (page == "futures") {
      tagList(
        h1("Futures Page", class = "text-3xl"),
        DTOutput("futures_table")
      )
    } else if (page == "forex") {
      tagList(
        h1("Forex Page", class = "text-3xl"),
        DTOutput("forex_table")
      )
    }
  })

  # Render Data Tables with Formatting
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