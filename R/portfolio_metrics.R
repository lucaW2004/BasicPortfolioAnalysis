#' Calculate important portfolio metrics
#'
#' This function calculates the annual returns, volatility, sharpe ratio, maximum drawdown, CAGR, beta and alpha metrics on the given portfolio.
#' @param portfolio_data A data frame containing the different positions of the portfolio and their value for each year. It has to have at least 3 years
#' (e.g., data.frame( position = c("Stock A", "Stock B"),
#'                      year_0 = c(950, 1500),
#'                      year_1 = c(1000, 1200),
#'                      year_2 = c(1000, 1400) ))
#' @param risk_free_rate A percentage specifying the risk free rate
#' @param market_returns A numeric vector containing the returns of an comparison market in percent (e.g., c(2,4,7,3)). It has to have one value less than the portfolio_data parameter has years.
#' @return A data frame with the calculated metrics as columns and the positions and the portfolio as rows. The calculated metrics are:
#' \item{annual_return_year_}{The annual return in percent for each position in the portfolio and the complete portfolio}
#' \item{volatility}{Volatility measures the range of returns of an investment}
#' \item{sharpe_ratio}{The Sharpe Ratio measures the risk-adjusted return of a portfolio}
#' \item{max_drawdown}{Maximum Drawdown measures the maximum loss from the peak to the trough of a portfolio over a specific period}
#' \item{cagr}{CAGR represents the average annual growth rate of an investment over a specific period}
#' \item{alpha}{Alpha measures the excess return of a portfolio compared to a benchmark index}
#' \item{beta}{Beta measures the sensitivity of a portfolio compared to the overall market. A beta greater than 1 indicates the portfolio is more volatile than the market}
#' @examples
#'portfolio_data <- data.frame(
#'  position = c("Stock A", "Stock B", "Stock C"),
#'  year_0 = c(950, 1500, 1350),
#'  year_1 = c(1000, 1200, 1500),
#'  year_2 = c(1100, 2100, 1600),
#'  year_3 = c(1200, 2200, 1700),
#')
#'risk_free_rate <- 2
#'market_returns <- c(3,5, 6)
#'portfolio_metrics <- calculate_portfolio_metrics(portfolio_data, risk_free_rate, market_returns, alpha=FALSE, beta=FALSE, cagr=FALSE, vola=FALSE)
#'
#'print(portfolio_metrics)
#' @export
calculate_portfolio_metrics <- function(portfolio_data, risk_free_rate, market_returns) {
  library(dplyr)

  years <- ncol(portfolio_data)-1

  # Überprüfen, ob die Eingabewerte gültig sind
  if (years <= 0) {
    stop("Die Anzahl der Jahre muss positiv sein.")
  }

  # Sicherstellen, dass das DataFrame die erforderlichen Spalten enthält
  required_columns <- c("position", paste0("year_", 0:(years-1)))
  if (!all(required_columns %in% colnames(portfolio_data))) {
    stop(paste("Das DataFrame muss die folgenden Spalten enthalten:", paste(required_columns, collapse = ", ")))
  }

  # Zeile für das gesamte Portfolio hinzufügen
  yearly_total_returns <- colSums(portfolio_data[,-1])
  total_row <- c("Portfolio", yearly_total_returns)
  portfolio_data <- rbind(portfolio_data, total_row)
  portfolio_data[,-1] <- lapply(portfolio_data[,-1], as.numeric)

  # Berechnung der jährlichen Renditen für jede Position und das gesamte Portfolio
  for (i in 0:(years - 2)) {
    value_prev_year <- portfolio_data[[paste0("year_", i)]]
    if(0 %in% value_prev_year) stop("position value cannot be equal to zero when calculating annual returns")
    portfolio_data[[paste0("annual_return_year_", i+1)]] <- ((portfolio_data[[paste0("year_", i + 1)]] / value_prev_year) - 1) * 100
  }

  # Berechnung der Volatilität (Standardabweichung der jährlichen Renditen)
  portfolio_data$volatility <- apply(portfolio_data[, paste0("annual_return_year_", 1:(years-1))], 1, sd)

  # Berechnung der Sharpe Ratio
  portfolio_volatility <- portfolio_data$volatility
  if (0 %in% portfolio_volatility) stop("volatily cannot be equal to zero when calculating the sharpe ratio")
  portfolio_data$sharpe_ratio <- (rowMeans(portfolio_data[, paste0("annual_return_year_", 1:(years-1))]) - risk_free_rate) / portfolio_volatility

  # Berechnung des Maximum Drawdown
  portfolio_data$max_drawdown <- apply(portfolio_data[, paste0("year_", 0:(years-1))], 1, function(x) {
    max_drawdown <- 0
    peak <- x[1]
    for (value in x) {
      if (value > peak) peak <- value
      if(peak==0) stop("peak cannot equal zero when calculating the maximum drawdown")
      drawdown <- (value - peak) / peak
      if (drawdown < max_drawdown) max_drawdown <- drawdown
    }
    return(max_drawdown * 100)
  })

  # Berechnung des CAGR
  initial_val <- portfolio_data[[paste0("year_", 0)]]
  if(0 %in% initial_val) stop("initial value of the position cannot be zero when calculating CAGR")
  portfolio_data$cagr <- ((portfolio_data[[paste0("year_", (years-1))]] / initial_val)^(1 / years) - 1) * 100

  # Berechnung von Beta und Alpha
  annual_returns_matrix <- as.matrix(portfolio_data[, paste0("annual_return_year_", 1:(years-1))])
  market_returns_len <- length(market_returns)
  if(market_returns_len != years-1 & market_returns_len > 0 ){
    stop("market_returns parameter has to have one value less than the portfolio_data parameter has years")
  }

  # Berechnung Beta
  market_returns_var = var(market_returns)
  if(market_returns_var==0){
    stop("variance of market returns has to be greater 0 when calculating beta")
  }
  portfolio_data$beta <- apply(annual_returns_matrix, 1, function(x) {
    cov(x, market_returns) / var(market_returns)
  })

  # Berechnung Alpha
  portfolio_data$alpha <- rowMeans(annual_returns_matrix) - (risk_free_rate + portfolio_data$beta * (mean(market_returns) - risk_free_rate))

  # Rückgabe des DataFrames mit allen Kennzahlen
  return(portfolio_data)
}
