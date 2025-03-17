# ui.R

library(shiny)

fluidPage(
  titlePanel("Nifty 50 Stock Data"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("stock", "Select Stock", choices = c(
        "RELIANCE.NS", "TCS.NS", "HDFCBANK.NS", "INFY.NS", "ICICIBANK.NS",
        "HINDUNILVR.NS", "SBIN.NS", "BAJFINANCE.NS", "BHARTIARTL.NS", "KOTAKBANK.NS",
        "LT.NS", "ASIANPAINT.NS", "AXISBANK.NS", "M&M.NS", "TITAN.NS",
        "ULTRACEMCO.NS", "HCLTECH.NS", "NTPC.NS", "WIPRO.NS", "POWERGRID.NS",
        "ADANIPORTS.NS", "JSWSTEEL.NS", "BAJAJFINSV.NS", "GRASIM.NS", "NESTLEIND.NS",
        "ONGC.NS", "MARUTI.NS", "TECHM.NS", "ADANIENT.NS",
        "SUNPHARMA.NS", "TATAMOTORS.NS", "CIPLA.NS", "COALINDIA.NS", "DIVISLAB.NS",
        "DRREDDY.NS", "EICHERMOT.NS", "HINDALCO.NS", "IOC.NS", "APOLLOHOSP.NS",
        "BPCL.NS", "HEROMOTOCO.NS", "INDUSINDBK.NS", "SBILIFE.NS", "SHREECEM.NS",
        "TATACONSUM.NS", "TATAPOWER.NS", "TATASTEEL.NS", "UPL.NS"
      )),
      dateRangeInput("dates", "Select Date Range",
                     start = Sys.Date() - 365,
                     end = Sys.Date()),
      actionButton("fetch", "Fetch Data")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Stock Data", DTOutput("stock_table"), plotOutput("stock_plot")),
        tabPanel("Stock vs. Nifty", plotOutput("comparison_plot"))
      )
    )
  )
)
