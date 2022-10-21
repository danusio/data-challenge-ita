rm(list = ls())

setwd("~/Documents/ITA DC/data")

library(tidyverse)
library(TTR)

df_prices = fst::read_fst("close_prices.fst")
df_returns = fst::read_fst("returns.fst")

load(file = "correlated_tickers.list")

tickers = names(df_prices |> select(-ref_date))

rsi = df_prices |> 
  select(-ref_date) |> 
  sapply(function(x) RSI(x,6)) |> 
  data.frame()

cti = df_prices |> 
  select(-ref_date) |> 
  sapply(function(x) c(rep(NA,5),CTI(x,6))) |> 
  data.frame()

tdi = df_prices |> 
  select(-ref_date) |> 
  sapply(function(x) TDI(x,6)[,"tdi"]) |> 
  data.frame()

di = df_prices |> 
  select(-ref_date) |> 
  sapply(function(x) TDI(x,6)[,"di"]) |> 
  data.frame()

macd = df_prices |> 
  select(-ref_date) |> 
  sapply(function(x) MACD(x,4,6,2)[,"macd"]) |> 
  data.frame()

signal = df_prices |> 
  select(-ref_date) |> 
  sapply(function(x) MACD(x,4,6,2)[,"signal"]) |> 
  data.frame()

# Fn
E = NULL
for (ticker in tickers) {
  sel_tickers = cor_tickers[[ticker]]
  lr_weights = cor_values[[ticker]]
  
  df_ticker = NULL
  for (tick in sel_tickers) {
    df_ticker = rbind(df_ticker,
                      cbind(
                        target = df_returns[,tick],
                        RSI = rsi[,tick],
                        CTI = cti[,tick],
                        MACD = macd[,tick],
                        signal = signal[,tick],
                        TDI = tdi[,tick],
                        DI = di[,tick],
                        weight = lr_weights[tick],
                        ref_date = df_returns$ref_date
                      ))
  }
  
  N = nrow(df_ticker)
  
  df_lagged = data.frame(
    target = df_ticker[2:N,"target"],
    df_ticker[1:(N-1),]
  ) |> na.omit()
  
  dates = unique(df_lagged$ref_date) |> sort()
  
  ini_test = dates[round(0.7*length(dates))]
  
  outpred = rep(NA,3)
  for (i in dates[dates>=ini_test]) {
    df_train = df_lagged |> filter(ref_date < i)
    model = lm(target ~ . - weight, df_train, weights = weight)
    
    df_test = df_lagged |> filter(weight == 1, ref_date == i)
    
    if(nrow(df_test) > 0){
      ypred = predict(model, df_test)
      names(ypred) = NULL
      
      outpred = rbind(outpred,
                      c(
                        actual = df_test$target,
                        predicted = ypred,
                        ref_date = i
                      ))
    }
  }
  
  outpred = data.frame(outpred |> na.omit())
  err = with(outpred, actual - predicted)
  EQM = mean(err^2)
  s2 = var(err)
  
  E = c(E, sqrt(EQM + s2))
  cat("Finished ticker: ");cat(ticker);cat("\n")
}

E |> summary() |> print()