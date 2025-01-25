# Compute monthly returns from Equity_Conversion_Factors
compute_monthly_returns <- function(equity_data) {
  # Extract the index levels (excluding the Date column)
  index_levels <- equity_data[, -1]
  
  # Compute percentage changes (monthly returns)
  monthly_returns <- diff(as.matrix(index_levels)) / head(as.matrix(index_levels), -1)
  
  # Combine the Date column (excluding the first date) with the returns
  return_data <- data.frame(
    Date = equity_data$Date[-1],  # Dates corresponding to returns
    monthly_returns
  )
  
  return(return_data)
}