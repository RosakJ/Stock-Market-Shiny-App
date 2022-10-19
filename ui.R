ui = dashboardPage(
  dashboardHeader(title = "Stock Analysis"),
  dashboardSidebar(
    selectInput("ticker_select", "Ticker", sp500$Ticker),
    selectInput("chart_select", "Chart Type", c("Candlestick", "Price Chart", "Industry Chart", "Performance Chart")),
    ## Try putting the input into the name of the charts
    checkboxInput("check", "hopefully this works", value = FALSE),
    checkboxInput("smooth", "Smooth"),
    conditionalPanel(
      condition = "input.smooth == true",
      selectInput("smoothMethod", "Method",
                  list("lm", "glm", "gam", "loess", "rlm"))
    )
    
  ),
  dashboardBody(
    
    fluidRow(
      column(
        width = 12,
        box(
          width = "100%",
          plotOutput('price_chart')
        )
      )
    ),
    fluidRow(
      column(
        width = 8,
        box(
          width = "100%",
          plotOutput('industry_chart')
        )
      ),
      column(
        width = 4,
        box(
          width = "100%",
          plotOutput('performance_chart')
        )
      )
    ),
    
  )
)
