rm(list = ls())

setwd("~/Documents/ITA DC/data")

library(tidyverse)
library(TTR)

# data loading ----
df_prices = fst::read_fst("daily_close_prices.fst")
period = 20

tickers = names(df_prices)[-1]

# functions ----
runQuantile = function(x, n){
  quant = function(y) mean(y <= tail(y,1), na.rm = TRUE)
  
  out = rep(NA, n)
  for (i in (n+1):length(x)) {
    out = c(out, quant(x[1:i]))
  }
  
  out
}

genIndicatorFeature = function(y, n){
  # MACD, RSI, CTI, returns runSD, returns quantile
  roc = ROC(y, n, "continuous")
  
  macd = MACD(y, n, 2*n, round(.75*n))
  rsi = RSI(y, n)
  cti = c(rep(NA,n-1), CTI(y, n))
  run_sd = runSD(roc, n)
  run_quantile = runQuantile(roc, n)
  
  df = data.frame(
    runSD = run_sd,
    returns = roc,
    macd,
    RSI = rsi,
    CTI = cti,
    runQuantile = run_quantile
  )
  names(df) = paste0(names(df), "_", n)
  df
}

# Technical Indicators ----
df_full <- df_future <- NULL
for (ticker in tickers) {
  cat("Ticker: ");cat(ticker);cat("\n")
  
  y = df_prices[,ticker] |> zoo::na.locf0()
  df = cbind(
    genIndicatorFeature(y, round(period/2)),
    genIndicatorFeature(y, period),
    genIndicatorFeature(y, 2*period)
  )
  
  N = length(y)
  lagged_interval = (1+period):N
  
  roc = ROC(y, period, "continuous")
  run_sd = runSD(roc, period)
  
  df_full = rbind(df_full,
                  data.frame(
                    target = run_sd[lagged_interval],
                    ticker = ticker,
                    target_date = df_prices[lagged_interval,"ref_date"],
                    df[lagged_interval - period,]
                  ) |> na.omit())
  
  # df_future = rbind(df_future,
  #                   data.frame(
  #                     ticker = ticker,
  #                     predictors_date = df_prices[(N-period+1):N,"ref_date"],
  #                     df[(N-period+1):N,]
  #                   ))
}

fst::write_fst(df_full, "vol_lagged_dataframe.fst", compress = 100)
# fst::write_fst(df_future, "vol_future_dataframe.fst", compress = 100)