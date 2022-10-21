rm(list = ls())

setwd("~/Documents/ITA DC/data")

library(yfR)

end_date = as.Date("2022-09-30")
ini_date = as.Date("2012-01-01")

df_prices = yf_collection_get(
  "SP500", 
  first_date = ini_date,
  last_date = end_date,
  freq_data = "monthly",
  type_return = "log",
  thresh_bad_data = 0,
  do_complete_data = TRUE
)

df_prices$ref_date = df_prices$ref_date |> 
  format("%Y-%m") |> 
  zoo::as.yearmon() |> 
  format("%Y-%m-%d") |> 
  as.Date()

list_wide = yf_convert_to_wide(df_prices)

df_returns = list_wide$ret_closing_prices
df_close = list_wide$price_close

fst::write_fst(df_returns, "returns.fst")
fst::write_fst(df_close, "close_prices.fst")
