library(shiny)

# Define custom classes
my_underline <- "relative after:absolute after:left-0 after:right-0 after:bottom-[-4px] after:h-[3px] after:bg-white after:scale-x-0 hover:after:scale-x-100 after:origin-left after:transition-transform after:duration-500"

ui <- tagList(
  # Include Tailwind CSS
  tags$head(
    tags$script(src = "https://cdn.tailwindcss.com"),
  ),
  
  tags$div(class = "bg-black text-white min-h-screen",
    # Navbar
    tags$div(class = "flex items-center p-4 w-full",
      tags$div(class = "space-x-4",
        tags$a(href = "#", class = my_underline, "Home"),
        tags$a(href = "#", class = my_underline, "About"),
        tags$a(href = "#", class = my_underline, "Contact")
      )
    ),
  
    # Main Content (Centered)
    tags$div(class = "p-8 text-center",
      "Welcome to my Shiny app with Tailwind!"
    )
  )
)

server <- function(input, output, session) {}

shinyApp(ui, server)