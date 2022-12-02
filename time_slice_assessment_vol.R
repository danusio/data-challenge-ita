rm(list = ls())

setwd("~/Documents/ITA DC/data")

library(tidyverse)
library(tidymodels)

# data loading and setup ----
df_full_prices = fst::read_fst("vol_lagged_dataframe.fst")
tickers = unique(df_full_prices$ticker)

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
  # s2 = var(err)

  # sqrt(EQM + s2)
  sqrt(EQM)
  # with(df, cor(.actual, .pred)^2)
}

getIndex = function(resamp) {
  min(assessment(resamp)$index)
}

createTransformedDf = function(resamp){
  df_train = analysis(resamp)
  df_test = assessment(resamp)
  
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
  
  list(train = applyCorFix(df_train_transf, list_cor),
       test = applyCorFix(bake(main_transf, df_test) |> 
                            mutate(target_date = as.numeric(target_date)), 
                          list_cor))
}

fitModel = function(transfDf, model_setup, ...){
  df_train = transfDf$train
  
  # fitting ----
  model_setup |> 
    fit(target ~ ., df_train)
}

getPerformance = function(model, transfDf){
  df_train = transfDf$train
  pred_train = predict(model, df_train)
  pred_train$.actual = df_train$target
  
  df_test = transfDf$test
  pred_test = predict(model, df_test)
  pred_test$.actual = df_test$target
  
  c(
    train = evalFn(pred_train),
    test = evalFn(pred_test)
  )
}

assessmentPipeline = function(DF, params){
  # params:
  ## frac, model_setup
  
  df = DF
  N = nrow(df)
  
  # df$index = 1:N
  
  time_splits = rolling_origin(
    data = df, 
    initial = round(params$frac*N), 
    assess = 20, 
    skip = 19
  )
  
  # start_index = map(time_splits$splits, getIndex)
  # time_splits$start_index <- do.call("c", start_index)
  
  time_splits$transformed = map(time_splits$splits, createTransformedDf)
  time_splits$model = map(time_splits$transformed, fitModel, model_setup = params$model_setup)
  
  map2_dfr(time_splits$model, time_splits$transformed, getPerformance)
}

printBounds = function(x){
  mu = x |> mean()
  sig = x |> sd()
  
  print(mu + 1.96*sig/sqrt(length(x))*c(lower_bound = -1, ev = 0, upper_bound = 1))
}

# Setup ----
model_setup_rf = rand_forest(
  mode = "regression",
  trees = 100,
  mtry = 4,
  min_n = 100
) 

model_setup_svm = svm_linear(
  mode = "regression",
  engine = "kernlab"
) 

model_setup_lm = linear_reg(
  mode = "regression",
  engine = "glmnet",
  penalty = 0,
  mixture = 0
)

model_setup_xgb = boost_tree(
  mode = "regression",
  engine = "xgboost",
  mtry = 4,
  trees = 100,
  min_n = 100,
  tree_depth = 3,
  learn_rate = .1,
)

# Assessment ----
set.seed(1)
sample_tickers = sample(tickers, 50)

set.seed(NULL)
rf_results <- svm_results <- lm_results <- xgb_results <- NULL
for (tick in sample_tickers) {
  cat(tick);cat("\n\n")
  
  df = df_full_prices |> filter(ticker == tick)
  
  rf_assess = assessmentPipeline(df, list(
    frac = 0.9,
    model_setup = model_setup_rf
  ))
  
  svm_assess = assessmentPipeline(df, list(
    frac = 0.9,
    model_setup = model_setup_svm
  ))
  
  lm_assess = assessmentPipeline(df, list(
    frac = 0.9,
    model_setup = model_setup_lm
  ))
  
  xgb_assess = assessmentPipeline(df, list(
    frac = 0.9,
    model_setup = model_setup_xgb
  ))
  
  rf_results = rbind(rf_results, rf_assess)
  svm_results = rbind(svm_results, svm_assess)
  lm_results = rbind(lm_results, lm_assess)
  xgb_results = rbind(xgb_results, xgb_assess)
  
  print(
    rbind(
      rf = colMeans(rf_results),
      svm = colMeans(svm_results),
      lm = colMeans(lm_results),
      xgb = colMeans(xgb_results)
    )
  )
}

cat("\nRF:\n")
printBounds(rf_results$test)
cat("\nSVM:\n")
printBounds(svm_results$test)
cat("\nGLM:\n")
printBounds(lm_results$test)
cat("\nXGBoost:\n")
printBounds(xgb_results$test)
