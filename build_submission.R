rm(list = ls())

setwd("~/Documents/ITA DC/data")

library(tidyverse)
library(tidymodels)
library(bizdays)

load_quantlib_calendars("UnitedStates/NYSE",
                        from = "2022-01-01", 
                        to = "2022-12-31"
)

pred_dates = seq(as.Date("2022-10-24"), as.Date("2022-11-18"), by = 1)
is_pred_bizday = is.bizday(pred_dates, "QuantLib/UnitedStates/NYSE")
pred_dates = pred_dates[is_pred_bizday]

# data loading and setup ----
df_full_train = fst::read_fst("lagged_dataframe.fst") # |> filter(ticker == "A")
df_full_test = fst::read_fst("future_dataframe.fst") # |> filter(ticker == "A")
tickers = unique(df_full_train$ticker) |> sort()

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

evalFn = function(df){
  err = with(df, .actual - .pred)
  EQM = mean(err^2)
  s2 = var(err)
  
  sqrt(EQM + s2)
}

predComb = function(df, w){
  w[1]*df$.pred_rf*df$.pred_glm + 
    w[2]*(exp(df$.pred_rf) - 1) + 
    w[3]*(exp(df$.pred_glm) - 1)
}

# Setup ----
model_setup_rf = rand_forest(
  mode = "regression",
  trees = 100,
  mtry = 4,
  min_n = 100
) 

model_setup_glm = linear_reg(
  mode = "regression",
  engine = "glmnet",
  penalty = 0,
  mixture = 0
)

df_submission = data.frame(Dia = pred_dates)
for (tick in tickers) {
  cat("\n\nTicker: ");cat(tick);cat("\n")
  
  df_train = df_full_train |> filter(ticker == tick)
  df_test = df_full_test |> 
    filter(ticker == tick) |> 
    rename(target_date = predictors_date) |> 
    mutate(target_date = pred_dates)
  
  # initial preproc ----
  train_recipe = recipe(target ~ ., df_train) |> 
    step_rm(ticker) |> 
    step_date(target_date, features = c("dow", "month")) |>
    step_dummy(all_nominal_predictors()) |> 
    step_zv(all_predictors())
  
  main_transf = train_recipe |> 
    prep(training = df_train)
  
  df_train_transf = bake(main_transf, df_train) |> 
    mutate(target_date = as.numeric(target_date))
  
  # correlation issue fix ----
  list_cor = corFix(df_train_transf)
  
  df_train_transf = applyCorFix(df_train_transf, list_cor)
  
  df_test_transf = applyCorFix(bake(main_transf, df_test) |> 
                                 mutate(target_date = as.numeric(target_date)), 
                               list_cor)
  
  # modeling ----
  model_rf = model_setup_rf |> 
    fit(target ~ ., df_train_transf)
  
  model_glm = model_setup_glm |> 
    fit(target ~ ., df_train_transf)
  
  # train prediction ----
  train_pred = bind_cols(
    predict(model_rf, df_train_transf) |> rename(.pred_rf = .pred),
    predict(model_glm, df_train_transf) |> rename(.pred_glm = .pred),
    tibble(.actual = df_train_transf$target)
  )
  
  test_pred = bind_cols(
    predict(model_rf, df_test_transf) |> rename(.pred_rf = .pred),
    predict(model_glm, df_test_transf) |> rename(.pred_glm = .pred)
  )
  
  # optimization ----
  fobj = function(w){
    if (sum(w < 0) > 0) return(1e6)
    
    w = w/sum(abs(w))
    
    out = predComb(train_pred, w)
    
    EQM = mean((train_pred$.actual - out)^2)
    s2 = var(train_pred$.actual - out)
    
    sqrt(EQM + s2)
  }
  
  ans = optim(rep(1/3,3), fobj)
  w_opt = ans$par/sum(abs(ans$par))
  print(w_opt |> round(3))
  
  ypred = predComb(test_pred, w_opt)
  
  df_submission[,tick] = ypred
}