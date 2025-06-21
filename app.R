library(shiny)
library(munsell)
library(ggplot2)
library(bslib)

theme_set(theme_classic(base_size = 16, 
                        base_family = 'serif'))



ui <- bslib::page_fluid(
  # Main plot area
  
  mainPanel(
    
    fluidRow(column(6, plotOutput("Population")),
             column(6, plotOutput("Sample")))),
  
  fluidRow(
    column(2, numericInput("Mean", "\u03BC", value = 5, min = -100, max = 100, step = 1)),
    column(2, numericInput("SD", "\u03C3", value = 3, min = 0.1, max = 10, step = 0.25)),
    column(2, numericInput("SS", "Sample Size", value = 100, min = 2, max = 20000, step = 10)),
    column(2, numericInput("NS", "Experiments", value = 10, min = 1, max = 20000, step = 10))
    , column(2, actionButton("start", "Run Experiments"))
  )
)



server <- function(input, output, session) {
  
  output$Population <- renderPlot({
    # Get values from inputs
    
    
    
    # Create the plot
    ggplot() +
      xlim(input$Mean-3*input$SD, input$Mean + 3*input$SD)+
      geom_function(fun = dnorm, args = list(mean = input$Mean,
                                             sd = input$SD),
                    color = "#1b305c") +
      xlab("Population Distribution") +
      geom_segment(aes(x = input$Mean,
                       xend = input$Mean,
                       y = 0,
                       yend = dnorm(input$Mean, mean = input$Mean, sd = input$SD)),
                   lty = 2) +
      scale_y_continuous(expand = c(0,0)) + 
      theme( axis.text.y=element_blank(),
             axis.ticks.y=element_blank(),
             axis.line.y = element_blank(),
             axis.title.y = element_blank())
    
    
    
    
  }, res = 100)
  
  
  
  
  
  
  sample <- eventReactive(input$start, {data.frame(colMeans(replicate(input$NS, 
                                                                      rnorm(n = input$SS, 
                                                                            mean = input$Mean, 
                                                                            sd = input$SD))))})
  
  
  
  output$Sample <- renderPlot({
    # Get values from inputs
    
    rand_sample <- sample()
    
    
    colnames(rand_sample) <- "X"
    
    ggplot(rand_sample, aes(x = X)) +
      xlab(paste("Means for", nrow(rand_sample), "Experiments")) +
      scale_y_continuous(expand = c(0,0)) +
      geom_histogram(color = "black",
                     linewidth = .8,
                     fill = "#1b305c",
                     bins = 30) +
      annotate("text", 
               x = Inf,
               y = Inf,
               label = paste0("Mean =", round(mean(rand_sample$X), 2),"\n",
                              "SD (aka SE) =", round(sd(rand_sample$X), 2)), hjust=1,vjust = 1) + 
      theme( axis.text.y=element_blank(),
             axis.ticks.y=element_blank(),
             axis.line.y = element_blank(),
             axis.title.y = element_blank())
    
    
  }, res = 100)
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

# shinylive::export(appdir = ".",
#                   destdir = "docs/")
