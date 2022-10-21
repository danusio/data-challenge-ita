rm(list = ls())

setwd("~/Documents/ITA DC/data")

library(tidyverse)
library(tidymodels)

evalFn = function(df){
  err = with(df, .actual - .pred)
  EQM = mean(err^2)
  s2 = var(err)
  
  sqrt(EQM + s2)
}

df_full = fst::read_fst("predictions_for_ensemble_2.fst")
df_full$.pred_glm = NULL
tickers = unique(df_full$ticker)

model_ml_setup = linear_reg(
  mode = "regression",
  engine = "glmnet",
  penalty = 0.01,
  mixture = 0.01
)

E <- W <- NULL
for (tick in tickers) {
  cat("Ticker: ");cat(tick);cat("\n")
  
  df = df_full |> filter(ticker == tick)
  N = nrow(df)
  
  if (N>=40){
    ini_test = round(N/2)
    int_test = seq(ini_test, N-20, by = 20)
    
    result_optim <- result_ml <- NULL
    for (i in int_test) {
      df_train = df[1:(i-1),]
      df_test = df[i:(i+19),]
      
      # optimization ----
      fobj = function(w){
        if (sum(w<0)>0) return(1e6)
        
        w = w/sum(abs(w))
        
        s1 = sign(cor(
          exp(df_train$.pred_svm) - 1, df_train$.actual
        ))
        
        s2 = sign(cor(
          exp(df_train$.pred_rf) - 1, df_train$.actual
        ))
        
        s3 = sign(cor(
          df_train$.pred_svm * df_train$.pred_rf, df_train$.actual
        ))
        
        out = s1*w[1]*(exp(df_train$.pred_svm) - 1) + 
          s2*w[2]*(exp(df_train$.pred_rf) - 1) + 
          s3*w[3]*df_train$.pred_svm * df_train$.pred_rf
        
        EQM = mean((df_train$.actual - out)^2)
        s2 = var(df_train$.actual - out)
        
        sqrt(EQM + s2)
      }
      
      ans = optim(c(0.4, 0.5, 0.6), fobj)
      w_opt = ans$par/sum(abs(ans$par))
      W = rbind(W, w_opt)
      
      ypred = w_opt[1]*df_test[,1] +
        w_opt[2]*df_test[,2]
      
      result_optim = rbind(result_optim,
                           cbind(.pred = ypred,
                                 .actual = df_test$.actual))
      
      # ML ----
      # model =  model_ml_setup |> 
      #   fit(.actual ~ ., df_train[,1:4])
      # 
      # ypred = predict(model, df_test)
      # ypred$.actual = df_test$.actual
      # 
      # result_ml = rbind(result_ml, ypred)
    }
    
    result_optim = data.frame(result_optim)
    # result_ml = data.frame(result_ml)
    result_rf = df[ini_test:N,c(".pred_rf", ".actual")] |> rename(.pred = .pred_rf)
    
    E = rbind(E,
              c(optim = evalFn(result_optim), 
                # ML = evalFn(result_ml),
                RF = evalFn(result_rf)))
    
    print(colMeans(E));cat("\n")
  }
}

cat("\n")
E |> summary() |> print()
W |> summary() |> print()

