# Load required packages
suppressPackageStartupMessages({
  library(quantmod)
  library(scales)
  library(RQuantLib)
  library(highcharter)
})

# Highcharter settings
hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- ","
options(highcharter.lang = hcoptslang)

# --- Function to Forecast Price using Volatility --- #
vol_forecast <- function(sym, start_date, end_date, forecast_days = 300, num_simulations = 1e6) {
  
  # Get historical prices
  full_stk <- getSymbols(sym, auto.assign = FALSE, from = start_date, to = Sys.Date() + 1)
  stk_prc <- full_stk[paste0(start_date, "/", end_date)]
  
  # Daily log returns and annualized volatility
  daily_returns <- dailyReturn(Cl(stk_prc), type = "log")
  hist_vol <- sd(daily_returns, na.rm = TRUE) * sqrt(252)
  
  last_price <- as.numeric(last(Cl(stk_prc)))
  dt <- 1 / 252
  drift <- 0  # can be modified to include mean return
  
  # Monte Carlo Simulation Matrix
  price_paths <- matrix(nrow = forecast_days, ncol = num_simulations)
  price_paths[1, ] <- last_price
  
  for (t in 2:forecast_days) {
    z <- rnorm(num_simulations)
    price_paths[t, ] <- price_paths[t - 1, ] * exp((drift - 0.5 * hist_vol^2) * dt + hist_vol * sqrt(dt) * z)
  }
  
  forecast_mean <- rowMeans(price_paths)
  forecast_upper <- apply(price_paths, 1, quantile, 0.975)
  forecast_lower <- apply(price_paths, 1, quantile, 0.025)
  
  # Generate future dates (trading days only)
  dates_all <- seq(as.Date(end_date) + 1, by = "day", length.out = forecast_days * 2)
  dates <- dates_all[isBusinessDay("UnitedStates/NYSE", dates_all)][1:forecast_days]
  
  forecast_df <- data.frame(Date = dates, Mean = forecast_mean, Upper = forecast_upper, Lower = forecast_lower)
  hist_df <- data.frame(Date = index(stk_prc), Price = as.numeric(Cl(stk_prc)))
  
  # Merge forecast to full series for plotting
  XTS <- merge(full_stk, xts(forecast_df[, c("Mean", "Upper", "Lower")], order.by = as.Date(forecast_df$Date)))
  
  # Plot using highcharter
  p <- highchart(type = "stock") %>%
    hc_add_series(round(XTS$Upper, 4), type = "line", color = "#E22F2F", name = "Upper") %>%
    hc_add_series(round(XTS$Lower, 4), type = "line", color = "#0BE600", name = "Lower") %>%
    hc_add_series(round(XTS$Mean, 4), type = "line", color = "grey", name = "Mean") %>%
    hc_add_series(round(XTS[, c(1:4)], 4), name = sym, upColor = "#0BE600", color = "#E22F2F") %>%
    hc_title(text = paste0(sym, " Stock Price Forecast | Days: ", forecast_days)) %>%
    hc_subtitle(text = paste0("Historical Volatility: ", percent(hist_vol, accuracy = 0.01))) %>%
    hc_yAxis(type = "logarithmic")
  
  # Output key values
  cat("------ Forecast Summary ------\n")
  cat("Symbol: ", sym, "\n")
  cat("Annualized Historical Volatility:", round(hist_vol, 4), "\n")
  cat("Last Observed Price:", round(last_price, 2), "\n")
  cat("Forecasted Price in", forecast_days, "days:", round(forecast_mean[forecast_days], 2), "\n")
  
  return(list(plot = p, forecast_df = forecast_df, hist_vol = hist_vol, last_price = last_price))
}

# --- Run Example --- #
result <- vol_forecast(sym = "SPY",
                       start_date = "2024-01-01",
                       end_date = "2025-02-15",
                       forecast_days = 90,
                       num_simulations = 10000)

# Show plot
print(result$plot)
