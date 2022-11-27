rm(list = ls())
setwd("~/Documents/ITA DC/data")

library(tidyverse)

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
      #  // pmax(utility,th)/sum(pmax(utility,th)) // pmax(.actual,th)/sum(pmax(.actual,th))
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
    
    out = mean(abs(x*pred - actual)) # sqrt(mean((x*pred - actual)^2))
    
    out
  }
  
  ans = optim(rep(1, length(pred)), fobj)
  
  w = ans$par/sum(ans$par)
  
  w
}

getUtil = function(r, S) exp(r) - S # exp(r)^2 - S^2

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

lambda_vec = c(.05, .1, .3, .5, .75) # exploration rate
nstocks = c(2,5,10,15,20)

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
    
    performance = NULL
    for (i in 1:nperiods) {
      time_vec = dates[seq(i, ndates, by=nperiods)]
      
      df1 = inner_join(df, data.frame(target_date = time_vec), by = "target_date")
      
      df2 = df1 |> filter(target_date == time_vec[1])
      
      r = with(df2, sum(weights* .actual))
      # w = with(df2, (1-lambda)*(actual_weights + weights)/2 + lambda * weights)
      w_pred = with(df2, getWeights(weights, actual_weights))
      w = with(df2, w_pred*weights/sum(w_pred*weights))
      
      results = r
      # weights = df2$weights
      for (d in time_vec[-1]) {
        # weights = rbind(weights, w)
        
        df2 = df1 |> filter(target_date == d)
        
        r = with(df2, sum(w* .actual))
        results = c(results, r)
        
        # w = with(df2, (1-lambda)*(actual_weights + w)/2 + lambda * w)
        # w_pred = sqrt(((1-lambda)*with(df2, getWeights(weights, actual_weights)))^2 + (lambda*w_pred)^2)
        w_pred = (1-lambda)*with(df2, getWeights(weights, actual_weights)) + lambda*w_pred
        w = with(df2, w_pred*weights/sum(w_pred*weights))
      }
      
      Er = mean(results, na.rm = TRUE)
      S2 = var(results, na.rm = TRUE)
      S2_neg = var(results[results<=0], na.rm = TRUE)
      
      performance = rbind(performance, c(Er = Er, S2 = S2, S2_neg = S2_neg))
      # weights = data.frame(weights)
      # names(weights) = paste0("ticker_", 1:n)
      # rownames(weights) = NULL
      # weights$ref_date = time_vec
    }
    
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

overall_perf |> 
  group_by(nstocks) |> 
  summarise(Er = mean(Er), 
            S = sqrt(mean(S2)), 
            SR_annual = mean(SR_annual)) |> 
  print()

overall_perf |> 
  group_by(lambda) |> 
  summarise(Er = mean(Er), 
            S = sqrt(mean(S2)), 
            SR_annual = mean(SR_annual)) |> 
  print()

overall_perf |> 
  group_by(lambda) |> 
  summarise(Er = mean(Er), 
            S = sqrt(mean(S2)), 
            Sortino = mean(Sortino, na.rm=T)) |> 
  print()

overall_perf |> arrange(desc(SR_annual)) |> head(5) |> print()
