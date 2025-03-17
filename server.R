# server.R

library(shiny)
library(quantmod)
library(DT)

function(input, output, session) {
  stock_data <- reactiveVal(NULL)
  nifty_data <- reactiveVal(NULL)
  error_message <- reactiveVal("")
  nifty50_symbols <- c(
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
  )
  
  observeEvent(input$fetch, {
    req(input$stock, input$dates)
    
    tryCatch({
      # Fetch selected stock data
      stock <- getSymbols(
        input$stock, src = "yahoo",
        from = input$dates[1], to = input$dates[2],
        auto.assign = FALSE
      )
      
      # Fetch Nifty 50 index data (symbol: ^NSEI on Yahoo Finance)
      nifty <- getSymbols(
        "^NSEI", src = "yahoo",
        from = input$dates[1], to = input$dates[2],
        auto.assign = FALSE
      )
      
      # Store data in reactive values
      stock_data(stock)
      nifty_data(nifty)
      error_message("")  # Clear error message if successful
    }, error = function(e) {
      # Capture and display any error messages
      error_message(paste("Error fetching data. Details:", e$message))
      stock_data(NULL)
      nifty_data(NULL)
    })
  })
  
  # Render stock data table
  output$stock_table <- renderDT({
    req(stock_data())
    data <- as.data.frame(stock_data())
    data$Date <- rownames(data)
    datatable(data, options = list(pageLength = 10), rownames = FALSE)
  })
  
  # Render stock price plot
  output$stock_plot <- renderPlot({
    req(stock_data())
    chartSeries(
      stock_data(),
      theme = chartTheme("white"),
      name = input$stock,
      TA = NULL
    )
  })
  
  # Render comparison plot: Stock vs. Nifty 50
  output$comparison_plot <- renderPlot({
    req(stock_data(), nifty_data())
    
    # Convert stock and Nifty data to data frames for plotting
    stock_df <- as.data.frame(stock_data())
    nifty_df <- as.data.frame(nifty_data())
    
    # Align dates and normalize prices for comparison
    stock_df$Date <- as.Date(rownames(stock_df))
    nifty_df$Date <- as.Date(rownames(nifty_df))
    merged_df <- merge(stock_df[, c("Date", "stock_data.Close")],
                       nifty_df[, c("Date", "nifty_data.Close")],
                       by = "Date", all = FALSE)
    colnames(merged_df) <- c("Date", "Stock", "Nifty")
    merged_df$Stock <- merged_df$Stock / merged_df$Stock[1] * 100
    merged_df$Nifty <- merged_df$Nifty / merged_df$Nifty[1] * 100
    
    # Plot the data
    plot(merged_df$Date, merged_df$Stock, type = "l", col = "blue",
         xlab = "Date", ylab = "Normalized Price (%)",
         main = paste("Comparison: Stock vs. Nifty 50"),
         ylim = range(c(merged_df$Stock, merged_df$Nifty)))
    lines(merged_df$Date, merged_df$Nifty, col = "red")
    legend("topright", legend = c(input$stock, "Nifty 50"),
           col = c("blue", "red"), lty = 1)
  })
  
  # Render Black vs. Red table
  output$black_red_table <- renderDT({
    # Fetch the latest closing prices for all Nifty 50 stocks
    stock_changes <- sapply(nifty50_symbols, function(symbol) {
      tryCatch({
        data <- getSymbols(symbol, src = "yahoo", from = Sys.Date() - 2, to = Sys.Date(), auto.assign = FALSE)
        close_prices <- as.numeric(Cl(data))
        if (length(close_prices) >= 2) {
          change <- (close_prices[length(close_prices)] - close_prices[length(close_prices) - 1]) /
            close_prices[length(close_prices) - 1] * 100
          return(round(change, 2))
        } else {
          return(NA)
        }
      }, error = function(e) {
        return(NA)
      })
    })
    
    # Create a data frame with stock symbols and percentage changes
    stock_changes_df <- data.frame(
      Stock = names(stock_changes),
      Change = stock_changes,
      Status = ifelse(stock_changes > 0, "Black", "Red"),
      stringsAsFactors = FALSE
    )
    
    # Render the table with color coding
    datatable(stock_changes_df, rownames = FALSE, options = list(pageLength = 10)) %>%
      formatStyle(
        "Change",
        color = styleInterval(0, c("red", "black"))
      )
  })
}
