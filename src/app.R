library(shiny)

# Define custom classes
my_underline <- "relative after:absolute after:left-0 after:right-0 after:bottom-[-4px] after:h-[3px] after:bg-white after:scale-x-0 hover:after:scale-x-100 after:origin-left after:transition-transform after:duration-500"

ui <- tagList(
  # Include Tailwind CSS
  tags$head(
    tags$script(src = "https://cdn.tailwindcss.com"),
    
    # Automatically set the initial page to "equities"
    tags$script(HTML("Shiny.setInputValue('page', 'equities', {priority: 'event'});"))
  ),
  
  # Main Container
  tags$div(class = "bg-black text-white min-h-screen",
    
    # Navbar
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
    
    # Dynamic page content
    tags$div(class = "p-8",
      uiOutput("page_content")
    )
  )
)

server <- function(input, output, session) {
  
  # Ensure page starts on "equities" if not set
  observe({
    if (is.null(input$page)) {
      updateQueryString("?page=equities", mode = "replace", session)
    }
  })
  
  # Reactive Page Content
  output$page_content <- renderUI({
    # Set default page to "equities"
    page <- input$page %||% "equities"

######################### EQUIITIES PAGE #########################

    if (page == "equities") {
      tagList(
        h1("Equities Page", class = "text-3xl")
      )

######################### ETFs PAGE #########################

    } else if (page == "etfs") {
      tagList(
        h2("ETFs Page", class = "text-2xl font-bold"),
        p("Welcome to the ETFs section.", class = "text-lg")
      )

######################### CRYPTO PAGE #########################

    } else if (page == "crypto") {
      tagList(
        h2("Crypto Page", class = "text-2xl font-bold"),
        p("Welcome to the Crypto section.", class = "text-lg")
      )

######################### BONDS PAGE #########################

    } else if (page == "bonds") {
      tagList(
        h2("Bonds Page", class = "text-2xl font-bold"),
        p("Welcome to the Bonds section.", class = "text-lg")
      )

######################### FUTURES PAGE #########################

    } else if (page == "futures") {
      tagList(
        h2("Futures Page", class = "text-2xl font-bold"),
        p("Welcome to the Futures section.", class = "text-lg")
      )

######################### FOREX PAGE #########################

    } else if (page == "forex") {
      tagList(
        h2("Forex Page", class = "text-2xl font-bold"),
        p("Welcome to the Forex section.", class = "text-lg")
      )
    }
  })
}

shinyApp(ui, server)