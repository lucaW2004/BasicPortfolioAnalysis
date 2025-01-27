#' Visualize important portfolio metrics
#'
#' This function visualizes important portfolio metrics and should be used together with the calculate_portfolio_metrics function from this package
#' @param portfolio_data A data frame containing the different positions of the portfolio and the values for the metrics for each year.
#' It has to contain the values for at least three years for each position and it has to contain the values for the annual returns for each position.
#' On top of that it can contain the following metrics as columns:
#' year_ : obligatory, as already described
#' annual_return_year_ : obligatory, as already described
#' volatility : Volatility measures the range of returns of an investment
#' sharpe_ratio : The Sharpe Ratio measures the risk-adjusted return of a portfolio
#' max_drawdown : Maximum Drawdown measures the maximum loss from the peak to the trough of a portfolio over a specific period
#' cagr : CAGR represents the average annual growth rate of an investment over a specific period
#' alpha : Alpha measures the excess return of a portfolio compared to a benchmark index
#' beta : Beta measures the sensitivity of a portfolio compared to the overall market. A beta greater than 1 indicates the portfolio is more volatile than the market
#' It has to be formatted like the ouput of the calculate_portfolio_metrics function from this package
#' @return No values are being returned. Only the plots are shown
#' @examples
#' portfolio_data <- data.frame(
#'  position = c("Stock A", "Stock B", "Stock C"),
#'  year_0 = c(950, 1500, 1350),
#'  year_1 = c(1000, 1200, 1500),
#'  year_2 = c(1100, 1500, 2400)
#'  )
#' risk_free_rate <- 2
#' market_returns <- c(3,5)
#' portfolio_metrics <- calculate_portfolio_metrics(portfolio_data, risk_free_rate, market_returns)
#'
#' show_portfolio_metrics(portfolio_metrics)
#' @export
show_portfolio_metrics <- function(portfolio_data) {
  library(ggplot2)
  library(reshape2)
  library(ggpubr)
  # melt the data frames for the annual returns
  annual_returns_data <- melt(portfolio_data, id.vars = "position", measure.vars = grep("annual_return_year_", colnames(portfolio_data), value = TRUE))

  annual_value_positions <- melt(subset(portfolio_data, position != "Portfolio"), id.vars = "position", measure.vars = grep("^year_", colnames(portfolio_data), value = TRUE))

  annual_value_portfolio <- melt(subset(portfolio_data, position == "Portfolio"), id.vars = "position", measure.vars = grep("^year_", colnames(portfolio_data), value = TRUE))

  # plot value of distinct positions in portfolio
  p0 <- ggplot(annual_value_positions, aes(x = variable, y = value, color = position, group = position)) +
    geom_line() +
    geom_point() +
    scale_x_discrete(guide = guide_axis(n.dodge=2)) +
    labs(title = "Value of distinct positions", x = "Year", y = "Value") +
    theme_minimal()

  # plot value of whole portfolio
  p1 <- ggplot(annual_value_portfolio, aes(x = variable, y = value, group = position)) +
    geom_line() +
    geom_point() +
    scale_x_discrete(guide = guide_axis(n.dodge=2)) +
    labs(title = "Value of whole portfolio", x = "Year", y = "Value") +
    theme_minimal()

  # plot annual returns
  p2 <- ggplot(annual_returns_data, aes(x = variable, y = value, color = position, group = position)) +
    geom_line() +
    geom_point() +
    scale_x_discrete(guide = guide_axis(n.dodge=3)) +
    labs(title = "Annual Return", x = "Jahr", y = "Return (%)") +
    theme_minimal()

  # Define list of plots to be shown
  plot_list <- list(p0, p1, p2)

  # Plot der Volatilität
  if("volatility" %in% colnames(portfolio_data)){
    p3 <- ggplot(portfolio_data, aes(x = position, y = volatility, fill = position)) +
      geom_bar(stat = "identity") +
      labs(title = "Volatility", x = "Position", y = "Volatility (%)") +
      theme_minimal() +
      theme(legend.position = "none")

    plot_list[[length(plot_list)+1]] <- p3
  }

  # Plot Sharpe Ratio
  if("sharpe_ratio" %in% colnames(portfolio_data)){
    p4 <- ggplot(portfolio_data, aes(x = position, y = sharpe_ratio, fill = position)) +
      geom_bar(stat = "identity") +
      labs(title = "Sharpe Ratio", x = "Position", y = "Sharpe Ratio") +
      theme_minimal() +
      theme(legend.position = "none")

    plot_list[[length(plot_list)+1]] <- p4
  }

  # Plot Maximum Drawdown
  if("max_drawdown" %in% colnames(portfolio_data)){
    p5 <- ggplot(portfolio_data, aes(x = position, y = max_drawdown, fill = position)) +
      geom_bar(stat = "identity") +
      labs(title = "Maximum Drawdown", x = "Position", y = "Drawdown (%)") +
      theme_minimal() +
      theme(legend.position = "none")

    plot_list[[length(plot_list)+1]] <- p5
  }

  # Plot CAGR
  if("cagr" %in% colnames(portfolio_data)){
    p6 <- ggplot(portfolio_data, aes(x = position, y = cagr, fill = position)) +
      geom_bar(stat = "identity") +
      labs(title = "CAGR", x = "Position", y = "CAGR (%)") +
      theme_minimal() +
      theme(legend.position = "none")

    plot_list[[length(plot_list)+1]] <- p6
  }

  # Plot Beta
  if("beta" %in% colnames(portfolio_data)){
    p7 <- ggplot(portfolio_data, aes(x = position, y = beta, fill = position)) +
      geom_bar(stat = "identity") +
      labs(title = "Beta", x = "Position", y = "Beta") +
      theme_minimal() +
      theme(legend.position = "none")

    plot_list[[length(plot_list)+1]] <- p7
  }

  # Plot Alpha
  if("alpha" %in% colnames(portfolio_data)){
    p8 <- ggplot(portfolio_data, aes(x = position, y = alpha, fill = position)) +
      geom_bar(stat = "identity") +
      labs(title = "Alpha", x = "Position", y = "Alpha") +
      theme_minimal() +
      theme(legend.position = "none")

    plot_list[[length(plot_list)+1]] <- p8
  }

  # Show the plots
  f <- ggarrange(plotlist=plot_list, nrow=2, ncol=1)
  f
}
