rm(list = ls())
setwd("~/Documents/ITA DC/data")

library(tidyverse)
library(PerformanceAnalytics)

# functions ----
getBestTickers = function(df, n, lambda, ...){
  out = (df |> 
           mutate(
             utility = getUtil(.actual, actual_vol),
             pred_utility = getUtil(.pred_rf, pred_vol),
             weights = pmax(pred_utility, th)
           ) |> 
           arrange(desc(pred_utility)) |> 
           select(!contains(".pred")))[1:n,] |> 
    arrange(desc(utility)) |> 
    mutate(
      weights = weights/sum(weights),
      actual_weights = c(1-lambda, rep(lambda/(n()-1), n()-1))
    ) |> 
    mutate(actual_weights = actual_weights/sum(actual_weights)) |> 
    arrange(desc(weights)) |>
    mutate(
      rank = 1:n
    )
  
  out
}

getWeights = function(pred, actual){
  fobj = function(x){
    if (sum(x<0)>0) return(1e6)
    
    x = x/sum(x)
    
    out = mean(abs(x*pred - actual))
    
    out
  }
  
  ans = optim(rep(1, length(pred)), fobj)
  
  w = ans$par/sum(ans$par)
  
  w
}

getUtil = function(r, S) exp(r)^2 - S^2

# data loading and setup ----
df_returns = fst::read_fst("predictions_returns.fst")
df_vol = fst::read_fst("predictions_volatility.fst")

df_full_predictions = df_returns |> 
  select(c(ticker, target_date, .pred_rf, .actual)) |> 
  inner_join(
    df_vol |> 
      select(c(ticker, target_date, .pred_rf, .actual)) |> 
      rename(
        pred_vol = .pred_rf,
        actual_vol = .actual),
    by = c("ticker", "target_date")
  )

rm(df_returns, df_vol)

nperiods = 20
th = 0.0001

lambda_vec = .05
nstocks = 15

df_sp500 = fst::read_fst("daily_close_prices_sp500.fst") |> 
  mutate(return = TTR::ROC(price_close, nperiods, type = "continuous"))

# processing ----
overall_perf = NULL
for (n in nstocks) {
  
  perf_by_lambda = NULL
  for (lambda in lambda_vec) {
    df = df_full_predictions |> 
      group_by(target_date) |> 
      group_modify(getBestTickers, n = n, lambda = lambda) |> 
      na.omit()
    
    dates = unique(df$target_date) |> 
      sort()
    
    ndates = length(dates)
    
    performance <- R <- SP500 <- NULL
    W = 0
    for (i in 1:nperiods) {
      time_vec = dates[seq(i, ndates, by=nperiods)]
      
      if (length(time_vec)==38) time_vec = time_vec[-1]
      
      df1 = inner_join(df, data.frame(target_date = time_vec), by = "target_date")
      
      df2 = df1 |> filter(target_date == time_vec[1])
      
      r = with(df2, sum(weights* .actual))
     
      w_pred = with(df2, getWeights(weights, actual_weights))
      w = with(df2, w_pred*weights/sum(w_pred*weights))
      
      results = r
      weights = df2$weights
      for (d in time_vec[-1]) {
        weights = rbind(weights, w)
        
        df2 = df1 |> filter(target_date == d)
        
        r = with(df2, sum(w* .actual))
        results = c(results, r)
        
        w_pred = (1-lambda)*with(df2, getWeights(weights, actual_weights)) + lambda*w_pred
        w = with(df2, w_pred*weights/sum(w_pred*weights))
      }
      
      R = cbind(R, results)
      
      Er = mean(results, na.rm = TRUE)
      S2 = var(results, na.rm = TRUE)
      S2_neg = var(results[results<=0], na.rm = TRUE)
      
      performance = rbind(performance, c(Er = Er, S2 = S2, S2_neg = S2_neg))
      W = W+weights
      
      sp500 = (df_sp500 |> 
        inner_join(data.frame(ref_date = time_vec), by = "ref_date"))$return
      
      SP500 = cbind(SP500, sp500)
    }
    
    log_returns = data.frame(
      ref_date = time_vec,
      returns = rowMeans(R),
      returns_sp500 = rowMeans(SP500)
    ) |> 
      mutate(
        cum_returns = cumsum(returns),
        cum_returns_sp500 = cumsum(returns_sp500)
      )
    
    W = W/nperiods
    W = data.frame(W)
    names(W) = paste0("ticker_", 1:n)
    rownames(W) = NULL
    W$ref_date = time_vec
    perf_by_lambda = rbind(perf_by_lambda, 
                           colMeans(performance, na.rm = TRUE))
  }
  
  perf_by_lambda = cbind(perf_by_lambda, 
                         SR_annual = sqrt(12)*perf_by_lambda[,"Er"]/sqrt(perf_by_lambda[,"S2"]),
                         Sortino = perf_by_lambda[,"Er"]/sqrt(perf_by_lambda[,"S2_neg"]),
                         lambda = lambda_vec,
                         nstocks = n)
  
  overall_perf = rbind(overall_perf, perf_by_lambda)
}

overall_perf = overall_perf[order(overall_perf[,"SR_annual"], decreasing = TRUE),]

overall_perf = data.frame(overall_perf)

# visualization ----
overall_perf |> 
  print()

log_returns |> 
  select(returns, returns_sp500) |> 
  sapply(mean) |> 
  print()

log_returns |> 
  select(returns, returns_sp500) |> 
  sapply(sd) |> 
  print()

W |> 
  pivot_longer(-ref_date, names_to = "ticker", values_to = "weights") |> 
  mutate(ticker = str_remove_all(ticker, "ticker_") |> 
           as.numeric() |> 
           factor(levels = 1:n)) |> 
  ggplot(aes(x = ref_date)) +
  geom_col(aes(y = weights, color = ticker, fill = ticker)) +
  xlab("data") + ylab("pesos")


log_returns |> ggplot(aes(x = ref_date)) +
  geom_line(aes(y = cum_returns_sp500), color = "red") +
  geom_line(aes(y = cum_returns), color = "blue") +
  xlab("data") + ylab("retornos acumulados")

log_returns |> ggplot() + 
  geom_density(aes(x = returns_sp500), color = "red", fill = "red", alpha = .3) +
  geom_density(aes(x = returns), color = "blue", fill = "blue", alpha = .3) +
  xlab("retornos mensais") + ylab("densidade")

log_returns |> 
  mutate(`SP500 > 0?` = ifelse(returns_sp500>0, "sim", "não")) |> 
  ggplot(aes(x = returns_sp500, color = `SP500 > 0?`)) + 
  geom_point(aes(y = returns))+
  xlab("retornos SP500") + ylab("retornos estratégia")


y = with(log_returns, xts(returns, ref_date))
y_bench = with(log_returns, xts(returns_sp500, ref_date))

# metrics
draw = maxDrawdown(y, geometric = F)
draw_bench = maxDrawdown(y_bench, geometric = F)

exceed_returns = ActiveReturn(y, y_bench, scale = 12, geometric = FALSE)
adj_SR = AdjustedSharpeRatio(y, geometric=F)
adj_SR_bench = AdjustedSharpeRatio(y_bench, geometric=F)

SR = SharpeRatio(y, geometric=F, annualize = TRUE)
SR_bench = SharpeRatio(y_bench, geometric=F, annualize = TRUE)

sortino = SortinoRatio(y, geometric=F)
sortino_bench = SortinoRatio(y_bench, geometric=F)

appraisal = AppraisalRatio(y, y_bench)
jensen = CAPM.jensenAlpha(y, y_bench)

chart.VaRSensitivity(y, 
                     methods = c("ModifiedVaR", "HistoricalVaR", "ModifiedES","HistoricalES"),
                     main = "Medidas de Risco - Estratégia",
                     xlab = "risco", ylab = "confiança do intervalo")

chart.VaRSensitivity(y_bench, 
                     methods = c("ModifiedVaR", "HistoricalVaR", "ModifiedES","HistoricalES"),
                     main = "Medidas de Risco - S&P 500",
                     xlab = "risco", ylab = "confiança do intervalo")

