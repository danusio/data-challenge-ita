rm(list = ls())

setwd("~/Documents/ITA DC/data")

library(yfR)

end_date = Sys.Date() - 1
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

list_wide = yf_convert_to_wide(df_prices)

df_close = list_wide$price_close

fst::write_fst(df_close, "daily_close_prices.fst", compress = 100)
