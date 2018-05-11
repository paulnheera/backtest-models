#***********************
# Momentum Screenig App
#***********************

library(shiny)
library(quantmod)
library(gridExtra)
library(grid)
library(DT)
source("~/repos/Google-Finance-Database/Functions/get2D.R")
source("~/repos/backtest_models/momentum_screen.R")

# ---- Load Data ----
DailyPrice <- readRDS("~/repos/Google-Finance-Database/DailyPrice.rds")
MarketCap <- readRDS("~/repos/Google-Finance-Database/Market_Cap.rds")

Close = get2D(DailyPrice,"Close")
Close = as.xts(Close[,-1],order.by = Close$Date)

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
                  choices = list("3 Months" = 3, "6 Months" = 6,
                                 "9 Months" = 9, "12 Months" = 12),
                  selected = 3
                  )
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Table ----
      dataTableOutput(outputId = "Table")
      
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
  output$Table <- renderDataTable({
    
    J <- as.numeric(input$period)
    
    Q1 = momentum_screen(Close,J)
    
    #Join Market Cap:
    Q1 = Q1 %>% 
      left_join(MarketCap,by="Stock")
    
    Q1$MktCap = round(as.numeric(gsub("B","",Q1$MktCap)),digits = 3)
    Q1$Momentum = round(Q1$Momentum,digits = 4)
    
    
    colnames(Q1) = c("Stock","Momentum", "Market Cap (Billions)")
    
    g1 = tableGrob(Q1[1:(nrow(Q1)/2),])
    g2 = tableGrob(Q1[(nrow(Q1)/2+1):nrow(Q1),])
    
    
    #grid.arrange(g1,g2,ncol=2)
    #chartSeries(xts::last(Close[,1],J*21))
    datatable(Q1) %>% 
      formatRound(columns = c('Momentum','Market Cap (Billions)'),digits=4)
    
  })
  
}

shinyApp(ui = ui, server = server)
