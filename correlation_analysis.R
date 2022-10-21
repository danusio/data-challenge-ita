rm(list = ls())

setwd("~/Documents/ITA DC/data")

library(tidyverse)

df_returns = fst::read_fst("returns.fst")

cor_mat = cor(df_returns |> select(-ref_date), use = "c")

tickers <- rownames(cor_mat) <- colnames(cor_mat)

cutoff = .85

cor_tickers <- cor_values <- list()
for (tick in tickers) {
  x = abs(cor_mat[tick,])
  cor_tickers[[tick]] = tickers[x >= cutoff]
  cor_values[[tick]] = x[x >= cutoff]
}

save(cor_tickers, cor_values, file = "correlated_tickers.list")
