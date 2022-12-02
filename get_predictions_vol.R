rm(list = ls())

setwd("~/Documents/ITA DC/data")

library(tidyverse)
library(tidymodels)

# data loading and setup ----
df_full_prices = fst::read_fst("vol_lagged_dataframe.fst")
tickers = fst::read_fst("predictions_returns.fst")$ticker |> unique()

ntickers = length(tickers)

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

# simulation ----
model_setup_rf = rand_forest(
  mode = "regression",
  trees = 100
) 

# model_setup_svm = svm_linear(
#   mode = "regression",
#   engine = "kernlab"
# ) 

model_setup_glm = linear_reg(
  mode = "regression",
  engine = "glmnet",
  penalty = 0,
  mixture = 0
)

t0 = Sys.time()
iteration = 0

df_full_predictions = NULL
for (tick in tickers) {
  cat("\n\nTicker: ");cat(tick);cat("\n")
  
  df_prices = df_full_prices |> filter(ticker == tick)
  ini_get_ensemble_data = df_prices$target_date[round(0.7*nrow(df_prices))]
  
  min_date_pos = sum(df_prices$target_date < ini_get_ensemble_data)
  int_ensemble = seq(min_date_pos+1, nrow(df_prices)-20, by = 20)
  nIter = length(int_ensemble)
  
  num_iter = 1:nIter
  names(num_iter) = int_ensemble
  
  for (ini_test in int_ensemble) {
    cat("Iteration ");cat(num_iter[as.character(ini_test)]);cat(" from ");cat(nIter);cat(", ")
    df_train = df_prices[1:(ini_test-1),]
    df_test = df_prices[ini_test:(ini_test+19),]
    
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
    
    # modeling ----
    model_rf = model_setup_rf |> 
      set_args(mtry = round(sqrt(ncol(df_train_transf) - 1)), # round((ncol(df_train_transf) - 1)/6)
               min_n = round(nrow(df_train_transf)*.01)) |> 
      fit(target ~ ., df_train_transf)
    
    # model_svm = model_setup_svm |> 
    #   fit(target ~ ., df_train_transf)
    
    model_glm = model_setup_glm |>
      fit(target ~ ., df_train_transf)
    
    # prediction ----
    df_test_transf = applyCorFix(bake(main_transf, df_test) |> 
                                   mutate(target_date = as.numeric(target_date)), 
                                 list_cor)
    
    target_pred = predict(model_glm, df_test_transf)
    names(target_pred) = paste0(names(target_pred), "_glm")
    
    # target_pred$.pred_svm = predict(model_svm, df_test_transf)$.pred
    target_pred$.pred_rf = predict(model_rf, df_test_transf)$.pred
    
    target_pred$.actual = df_test_transf$target
    target_pred$target_date = df_test$target_date
    target_pred$ticker = tick
    
    rm(df_train, df_test, df_train_transf, df_test_transf)
    
    df_full_predictions = rbind(df_full_predictions, target_pred)
  }
  
  rm(df_prices)
  
  dt = Sys.time() - t0
  iteration = iteration+1
  avg_dt = dt/iteration
  
  end_time = Sys.time() + (ntickers - iteration)*avg_dt
  cat("\n End time: "); cat(end_time |> as.character()); cat("\n")
  
}

fst::write_fst(df_full_predictions, "predictions_volatility.fst", compress = 100)
cat("\nData saved!\n")