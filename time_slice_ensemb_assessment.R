rm(list = ls())

setwd("~/Documents/ITA DC/data")

library(tidyverse)
library(tidymodels)

# data loading and setup ----
df_full_predictions = rbind(
  fst::read_fst("predictions_for_ensemble.fst"),
  fst::read_fst("predictions_for_ensemble_1.fst"))

tickers = unique(df_full_predictions$ticker)

# functions ----
evalFn = function(df){
  err = with(df, .actual - .pred)
  EQM = mean(err^2)
  s2 = var(err)

  c(E = sqrt(EQM + s2),
    R2 = with(df, cor(.actual, .pred)^2))
}

predComb = function(df, w){
  # w[1]*(exp(df$.pred_svm) - 1) + 
  #   w[2]*(exp(df$.pred_rf) - 1) + 
  #   w[3]*(exp(df$.pred_glm) - 1)
  
  w[1]*df$.pred_rf*df$.pred_glm + 
    w[2]*(exp(df$.pred_rf) - 1) + 
    w[3]*(exp(df$.pred_glm) - 1)
}

getPerformance = function(resamp){
  df_train = analysis(resamp)
  df_test = assessment(resamp)
  
  # optimization ----
  fobj = function(w){
    # if ((sum(w < -1) + sum(w > 1)) > 0) return(1e6)
    if (sum(w < 0) > 0) return(1e6)
    
    w = w/sum(abs(w))
    
    out = predComb(df_train, w)
    
    EQM = mean((df_train$.actual - out)^2)
    s2 = var(df_train$.actual - out)
    R2 = cor(df_train$.actual, out)^2
    
    sqrt(EQM + s2)
  }
  
  ans = optim(rep(1/3,3), fobj)
  w_opt = ans$par/sum(abs(ans$par))
 
  ypred = predComb(df_test, w_opt)
  
  optimized = evalFn(data.frame(
    .pred = ypred,
    .actual = df_test$.actual
  )) 
  
  names(optimized) = paste0(names(optimized), "_optim")
  
  rf = evalFn(data.frame(
    .pred = df_test$.pred_rf,
    .actual = df_test$.actual
  ))
  
  names(rf) = paste0(names(rf), "_rf")
  
  svm = evalFn(data.frame(
    .pred = df_test$.pred_svm,
    .actual = df_test$.actual
  ))
  
  names(svm) = paste0(names(svm), "_svm")
  
  GLM = evalFn(data.frame(
    .pred = df_test$.pred_glm,
    .actual = df_test$.actual
  ))
  
  names(GLM) = paste0(names(GLM), "_glm")
  
  c(optimized, rf, svm, GLM)
}

assessmentPipeline = function(df, params){
  # params:
  ## frac
  
  N = nrow(df)
  
  time_splits = rolling_origin(
    data = df, 
    initial = round(params$frac*N), 
    assess = 20, 
    skip = 19
  )
  
  map_dfr(time_splits$splits, getPerformance)
}

printBounds = function(x){
  mu = x |> mean()
  sig = x |> sd()
  
  print(mu + 1.96*sig/sqrt(length(x))*c(lower_bound = -1, ev = 0, upper_bound = 1))
}

# Setup ----

# Assessment ----
# set.seed(1)
# sample_tickers = sample(tickers, 50)
# 
# set.seed(NULL)
results = NULL
for (tick in tickers) {
  cat(tick);cat(", ")
  
  df = df_full_predictions |> filter(ticker == tick)
  
  if (nrow(df) > 100){
    all_assess = assessmentPipeline(df, list(frac = 0.7))
    
    results = rbind(results, all_assess)
  }
}
cat("end loop. \n\n")
print(colMeans(results))

cat("\nRF:\n")
printBounds(results$E_rf)
cat("\nSVM:\n")
printBounds(results$E_svm)
cat("\nGLM:\n")
printBounds(results$E_glm)
cat("\nOptimized:\n")
printBounds(results$E_optim)
