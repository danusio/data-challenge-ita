rm(list = ls())

setwd("~/Documents/ITA DC/data")

library(tidyverse)

# data loading and setup ----
df_full_predictions = fst::read_fst("predictions_returns.fst")
df_count = df_full_predictions |> 
  group_by(target_date) |> 
  summarise(n = n())

df_full_predictions = inner_join(df_full_predictions, df_count, by="target_date") |> 
  filter(n>20) |> 
  select(-n)

# df = df_full_predictions |> 
#   filter(target_date == min(target_date))

nperiods = 20

getHighestReturns = function(df, n, ...){
  df1 = (df |> 
           arrange(desc(.pred_rf)) |> 
           mutate(
             weights = .pred_rf
           ) |> 
           select(!contains(".pred")))[1:n,] |> 
    mutate(
      weights = weights/sum(weights),
      weighted_return = weights * .actual
    )
  
  df1
}

assessStrategy = function(n){
  df_returns = df_full_predictions |> 
    group_by(target_date) |> 
    group_modify(getHighestReturns, n = n)
  
  result_strategy = df_returns |> 
    summarise(monthly_return = sum(weighted_return))
  
  N = nrow(result_strategy)
  
  cum_results = NULL
  for (i in 1:nperiods) {
    cum_results = rbind(cum_results,
                    c(
                      Er = mean(result_strategy$monthly_return[seq(i, N, by=nperiods)], na.rm = TRUE),
                      S2 = var(result_strategy$monthly_return[seq(i, N, by=nperiods)], na.rm = TRUE)
                      )
                    )
  }
  
  colMeans(cum_results)
}

nstocks = c(1,2,5,10,15,20)

results = sapply(nstocks, assessStrategy)

results = results |> t() |> as.data.frame()
results$SR_annual = with(results, Er/sqrt(S2))*sqrt(12)
results$n_stocks = nstocks

results |> print()
