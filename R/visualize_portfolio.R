#' Visualize important portfolio metrics
#'
#' This function visualizes the annual returns, volatility, sharpe ratio, maximum drawdown, CAGR, beta and alpha metrics on the given portfolio.
#' @param portfolio_data A data frame containing the different positions of the portfolio and their value for each year. It has to have at least 3 years
#' (e.g., data.frame( position = c("Stock A", "Stock B"),
#'                      year_0 = c(950, 1500),
#'                      year_1 = c(1000, 1200),
#'                      year_2 = c(1000, 1400) ))
#' @param risk_free_rate A percentage specifying the risk free rate
#' @param market_returns A numeric vector containing the returns of an comparison market in percent (e.g., c(2,4,7,3)). It has to have one value less than the portfolio_data parameter has years.
#' @return It does not return anything, but visualizes the calculated metrics. The calculated metrics are:
#' \item{annual_return_year_}{The annual return in percent for each position in the portfolio and the complete portfolio}
#' \item{volatility}{Volatility measures the range of returns of an investment}
#' \item{sharpe_ratio}{The Sharpe Ratio measures the risk-adjusted return of a portfolio}
#' \item{max_drawdown}{Maximum Drawdown measures the maximum loss from the peak to the trough of a portfolio over a specific period}
#' \item{cagr}{CAGR represents the average annual growth rate of an investment over a specific period}
#' \item{alpha}{Alpha measures the excess return of a portfolio compared to a benchmark index}
#' \item{beta}{Beta measures the sensitivity of a portfolio compared to the overall market. A beta greater than 1 indicates the portfolio is more volatile than the market}
#' @examples
#' portfolio_data <- data.frame(
#'  position = c("Stock A", "Stock B", "Stock C"),
#'  year_0 = c(950, 1500, 1350),
#'  year_1 = c(1000, 1200, 1500),
#'  year_2 = c(1100, 1500, 2400)
#'  )
#' risk_free_rate <- 2
#' market_returns <- c(3,5)
#'
#' visualize_portfolio(portfolio_data, risk_free_rate, market_returns)
#' @export
visualize_portfolio <- function(portfolio_data, risk_free_rate, market_returns) {
  #processed_portfolio <- portfolioAnalysisr::calculate_portfolio_metrics(portfolio_data, risk_free_rate, market_returns)
  print("hello")
  #show_portfolio_metrics <- portfolioAnalysisr::show_portfolio_metrics(processed_portfolio)
}
