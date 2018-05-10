#***********************
# Momentum Screenig App
#***********************

library(shiny)
library(quantmod)
library(gridExtra)
library(grid)
source("~/repos/Google Finance Database/Functions/get2D.R")
source("~/repos/backtest_models/momentum_screen.R")

# ---- Load Data ----
DailyPrice <- readRDS("~/repos/Google Finance Database/DailyPrice.rds")

Adjusted = get2D(DailyPrice,"Adjusted")
Adjusted = as.xts(Adjusted[,-1],order.by = Adjusted$Date)

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
    
    Q1 = momentum_screen(Adjusted,J)
    g1 = tableGrob(Q1[1:(nrow(Q1)/2),])
    g2 = tableGrob(Q1[(nrow(Q1)/2)+1:nrow(Q1),])
    
    
    grid.arrange(g1,g2,ncol=2)
    #chartSeries(xts::last(Close[,1],J*21))
    
  })
  
}

shinyApp(ui = ui, server = server)
