library(shiny)
library(bslib)

ui <- page_sidebar(
  title = "Simple Shiny App",
  theme = bs_theme(bootswatch = "flatly"),
  
  sidebar = sidebar(
    numericInput("number", "Enter a number:", value = 5, min = 1, max = 100),
    selectInput("color", "Choose color:", 
                choices = c("red", "blue", "green", "purple"))
  ),
  
  card(
    card_header("Result"),
    card_body(
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  output$plot <- renderPlot({
    plot(1:input$number, 
         col = input$color,
         pch = 19,
         cex = 2,
         main = "Simple Plot",
         xlab = "Index",
         ylab = "Value")
  })
}

shinyApp(ui, server)
