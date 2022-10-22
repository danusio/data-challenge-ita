rm(list = ls())

setwd("~/Documents/ITA DC/data")

library(yfR)
library(tidyverse)

end_date = as.Date("2022-10-22")
ini_date = as.Date("2012-01-01")

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

nticks = df_prices$ticker |> unique() |> length()
nticks |> print()
df_prices$ref_date |> max() |> print()

if (nticks == 502){
  list_wide = yf_convert_to_wide(df_prices)
  
  df_close = list_wide$price_close
  
  fst::write_fst(df_close, "daily_close_prices.fst", compress = 100)
}
