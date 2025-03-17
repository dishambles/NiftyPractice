# server.R

library(shiny)
library(quantmod)
library(DT)

function(input, output, session) {
  stock_data <- reactiveVal(NULL)
  nifty_data <- reactiveVal(NULL)
  error_message <- reactiveVal("")
  
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
}
