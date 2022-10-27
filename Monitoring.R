rm(list = ls())

setwd("~/Documents/ITA DC/data")

library(yfR)
library(tidyverse)

evalFn = function(df, ...){
  err = with(df, .actual - .pred)
  EQM = mean(err^2)
  s2 = var(err)
  
  data.frame(E = sqrt(EQM + s2),
    R2 = with(df, cor(.actual, .pred)^2))
}

end_date = Sys.Date()
ini_date = as.Date("2022-09-26")

df_prices = yf_collection_get(
  "SP500", 
  first_date = ini_date,
  last_date = end_date,
  freq_data = "daily",
  type_return = "log",
  thresh_bad_data = 0,
  do_complete_data = TRUE
)

df_prices = df_prices |> 
  filter(ticker != "TRGP")

df_prices_1 = yf_get(
  c("BF-B", "BRK-B"), 
  first_date = ini_date,
  last_date = end_date,
  freq_data = "daily",
  type_return = "log",
  thresh_bad_data = 0,
  do_complete_data = TRUE
)

df_prices = rbind(df_prices, df_prices_1)
rm(df_prices_1)

df_returns = df_prices |> 
  group_by(ticker) |> 
  summarise(
    ref_date = ref_date,
    .actual = TTR::ROC(price_close, 20, type = "continuous" )
  ) |> 
  na.omit()

df_submission = read_csv("predicao.csv") |> 
  pivot_longer(
    A:ZTS,
    names_to = "ticker",
    values_to = ".pred"
  ) |> 
  rename(ref_date = Dia)

df_comp = inner_join(df_returns, 
                     df_submission, 
                     by = c("ticker", "ref_date"))

df_comp |> 
  group_by(ticker) |> 
  group_modify(~ evalFn(.x)) |> 
  ungroup() |> 
  select(-ticker) |> 
  colMeans() |> 
  print()
