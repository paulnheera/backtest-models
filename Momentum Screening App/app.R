#***********************
# Momentum Screenig App
#***********************

library(shiny)
library(quantmod)

# Define UI for app that draws screen table ----

ui <- fluidPage(
  
  # App title ----
  titlePanel("Momentum Screen"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      selectInput(inputId = "period",
                  label = "Formation Period",
                  choices = list("Choice 1" = 3, "Choice 2" = 6,
                                 "Choice 3" = 9, "Choice 4" = 12),
                  selected = 3
                  )
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Table ----
      plotOutput(outputId = "Table")
      
    )
  )
)



# Define server logic required to draw table ----

server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$Table <- renderPlot({
    
    J <- as.numeric(input$period)
    
    chartSeries(xts::last(Close[,1],J*21))
    
  })
  
}

shinyApp(ui = ui, server = server)
