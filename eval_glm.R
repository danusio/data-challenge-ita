rm(list = ls())

setwd("~/Documents/ITA DC/data")

library(tidyverse)
library(tidymodels)

# data loading and setup ----
df_full_prices = fst::read_fst("lagged_dataframe.fst")
tickers = unique(df_full_prices$ticker)

ini_test = as.Date("2022-09-01")

# functions ----
corFix = function(df){
  df1 = df
  # find high correlations
  df_train_pred = df1 |> select(-target)
  
  hcv = DescTools::FindCorr(df_train_pred |> cor(use = "c"), .85) # high correlated variables
  hcv_names = names(df_train_pred)[hcv]
  
  # PCA transformation
  pca_transf <- recipe(~ ., df1 |> select(all_of(hcv_names))) |>
    step_normalize(all_numeric_predictors())  |> 
    step_pca(all_numeric(), threshold = .95)
  
  list(estimates = prep(pca_transf, training = df1),
       HCV = hcv_names)
}

applyCorFix = function(df, list_fix){
  cbind(df, 
        bake(list_fix$estimates, df)) |> 
    select(-all_of(list_fix$HCV))
}

getRelevantFeatures = function(df, n = 10) c("target", 
                                             FSelector::rank.correlation(target ~ ., df) |> 
                                               arrange(desc(attr_importance)) |> 
                                               rownames() |> head(n))

evalFn = function(df){
  err = with(df, .actual - .pred)
  EQM = mean(err^2)
  s2 = var(err)
  
  sqrt(EQM + s2)
}

# simulation ----
model_setup = linear_reg(
  mode = "regression",
  engine = "glmnet",
  penalty = 0,
  mixture = 0
)

E = NULL
for (tick in tickers) {
  cat("Ticker: ");cat(tick);cat("\n")
  
  df_prices = df_full_prices |> filter(ticker == tick)
  
  df_train = df_prices |> filter(target_date < ini_test)
  df_test = df_prices |> filter(target_date >= ini_test)
  rm(df_prices)
  
  # initial preproc ----
  train_recipe = recipe(target ~ ., df_train) |> 
    step_rm(ticker) |> 
    step_date(target_date, features = c("dow", "month")) |>
    # step_BoxCox(all_numeric_predictors()) |> 
    step_dummy(all_nominal_predictors()) |> 
    step_zv(all_predictors()) |> 
    step_normalize(all_numeric_predictors())
  
  main_transf = train_recipe |> 
    prep(training = df_train)
  
  df_train_transf = bake(main_transf, df_train) |> 
    mutate(target_date = as.numeric(target_date))
  
  # correlation issue fix ----
  list_cor = corFix(df_train_transf)
  
  df_train_transf = applyCorFix(df_train_transf, list_cor)
  
  sel_feats = getRelevantFeatures(df_train_transf, 15)

  df_train_transf = df_train_transf[, sel_feats]
  
  # modeling ----
  model = model_setup |> 
    fit(target ~ ., df_train_transf)
  
  # prediction ----
  df_test_transf = applyCorFix(bake(main_transf, df_test) |> 
                                 mutate(target_date = as.numeric(target_date)), 
                               list_cor)
  
  target_pred = predict(model, df_test_transf)  
  target_pred$.actual = df_test_transf$target
  
  # evaluation ----
  E = c(E, evalFn(target_pred))
  
  rm(df_train, df_test, df_train_transf, df_test_transf)
  gc()
  
  cat(mean(E));cat("\n")
}
cat("\n")
summary(E) |> print()

# fst::write_fst(data.frame(ticker = tickers, E = E), "results_linear.fst", compress = 100)
