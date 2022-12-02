rm(list = ls())
setwd("~/Documents/ITA DC/data")

library(tidyverse)
library(PerformanceAnalytics)

# functions ----
getBestTickers = function(df, n, lambda, ...){
  out = (df |> 
           mutate(
             utility = getUtil(.actual, actual_vol),
             pred_utility = getUtil(.pred_rf, pred_vol),
             weights = pmax(pred_utility, th)
           ) |> 
           arrange(desc(pred_utility)) |> 
           select(!contains(".pred")))[1:n,] |> 
    arrange(desc(utility)) |> 
    mutate(
      weights = weights/sum(weights),
      actual_weights = c(1-lambda, rep(lambda/(n()-1), n()-1))
    ) |> 
    mutate(actual_weights = actual_weights/sum(actual_weights)) |> 
    arrange(desc(weights)) |>
    mutate(
      rank = 1:n
    )
  
  out
}

getWeights = function(pred, actual){
  fobj = function(x){
    if (sum(x<0)>0) return(1e6)
    
    x = x/sum(x)
    
    out = mean(abs(x*pred - actual))
    
    out
  }
  
  ans = optim(rep(1, length(pred)), fobj)
  
  w = ans$par/sum(ans$par)
  
  w
}

getUtil = function(r, S) exp(r)^2 - S^2

getHighestReturns = function(df, n, ...){
  df1 = (df |> 
           arrange(desc(.pred_rf)) |> 
           mutate(
             weights = .pred_rf
           ) |> 
           select(!contains(".pred")))[1:n,] |> 
    mutate(
      weights = weights/sum(weights),
      weighted_return = weights * .actual
    )
  
  df1
}

# data loading and setup ----
df_returns = fst::read_fst("predictions_returns.fst")
df_vol = fst::read_fst("predictions_volatility.fst")

df_full_predictions = df_returns |> 
  select(c(ticker, target_date, .pred_rf, .actual)) |> 
  inner_join(
    df_vol |> 
      select(c(ticker, target_date, .pred_rf, .actual)) |> 
      rename(
        pred_vol = .pred_rf,
        actual_vol = .actual),
    by = c("ticker", "target_date")
  )

rm(df_returns, df_vol)

nperiods = 20
th = 0.0001

lambda_vec = .05
nstocks = 15

df_sp500 = fst::read_fst("daily_close_prices_sp500.fst") |> 
  mutate(return = TTR::ROC(price_close, nperiods, type = "continuous"))

# processing ----
# overall_perf = NULL
for (n in nstocks) {
  df_naive = df_full_predictions |> 
    group_by(target_date) |> 
    group_modify(getHighestReturns, n = n)
  
  dates = unique(df_naive$target_date) |> 
    sort()
  
  ndates = length(dates)
  max_inst = floor((ndates-nperiods)/nperiods)
  
  r_naive = 0
  for (i in 1:nperiods) {
    time_vec = dates[seq(i, ndates, by=nperiods)]
    
    df1 = inner_join(df_naive, data.frame(target_date = time_vec), by = "target_date")
    
    df2 = df1 |> 
      summarise(return = sum(weighted_return))
    
    r_naive = r_naive + df2$return[1:max_inst]
  }
  
  r_naive = r_naive/nperiods
  
  # perf_by_lambda = NULL
  for (lambda in lambda_vec) {
    df = df_full_predictions |> 
      group_by(target_date) |> 
      group_modify(getBestTickers, n = n, lambda = lambda) |> 
      na.omit()
    
    dates = unique(df$target_date) |> 
      sort()
    
    ndates = length(dates)
    
    performance <- R <- SP500 <- NULL
    W = 0
    for (i in 1:nperiods) {
      time_vec = dates[seq(i, ndates, by=nperiods)]
      
      if (length(time_vec)==38) time_vec = time_vec[-1]
      
      df1 = inner_join(df, data.frame(target_date = time_vec), by = "target_date")
      
      df2 = df1 |> filter(target_date == time_vec[1])
      
      r = with(df2, sum(weights* .actual))
      
      w_pred = with(df2, getWeights(weights, actual_weights))
      w = with(df2, w_pred*weights/sum(w_pred*weights))
      
      results = r
      weights = df2$weights
      for (d in time_vec[-1]) {
        weights = rbind(weights, w)
        
        df2 = df1 |> filter(target_date == d)
        
        r = with(df2, sum(w* .actual))
        results = c(results, r)
        
        w_pred = (1-lambda)*with(df2, getWeights(weights, actual_weights)) + lambda*w_pred
        w = with(df2, w_pred*weights/sum(w_pred*weights))
      }
      
      R = cbind(R, results)
      
      Er = mean(results, na.rm = TRUE)
      S2 = var(results, na.rm = TRUE)
      S2_neg = var(results[results<=0], na.rm = TRUE)
      
      performance = rbind(performance, c(Er = Er, S2 = S2, S2_neg = S2_neg))
      W = W+weights
      
      sp500 = (df_sp500 |> 
                 inner_join(data.frame(ref_date = time_vec), by = "ref_date"))$return
      
      SP500 = cbind(SP500, sp500)
    }
    
    log_returns = data.frame(
      ref_date = dates[seq(i, ndates, by=nperiods)],
      returns = rowMeans(R),
      returns_naive = r_naive,
      returns_sp500 = rowMeans(SP500)
    ) |> 
      mutate(
        cum_returns = cumsum(returns),
        cum_returns_naive = cumsum(r_naive),
        cum_returns_sp500 = cumsum(returns_sp500)
      )
    
    W = W/nperiods
    W = data.frame(W)
    names(W) = paste0("ticker_", 1:n)
    rownames(W) = NULL
    W$ref_date = time_vec
    # perf_by_lambda = rbind(perf_by_lambda, 
    #                        colMeans(performance, na.rm = TRUE))
  }
  
  # perf_by_lambda = cbind(perf_by_lambda, 
  #                        SR_annual = sqrt(12)*perf_by_lambda[,"Er"]/sqrt(perf_by_lambda[,"S2"]),
  #                        Sortino = perf_by_lambda[,"Er"]/sqrt(perf_by_lambda[,"S2_neg"]),
  #                        lambda = lambda_vec,
  #                        nstocks = n)
  # 
  # overall_perf = rbind(overall_perf, perf_by_lambda)
}

# overall_perf = overall_perf[order(overall_perf[,"SR_annual"], decreasing = TRUE),]
# 
# overall_perf = data.frame(overall_perf)

# visualization ----
# overall_perf |> 
#   print()

log_returns |> 
  select(returns, returns_naive, returns_sp500) |> 
  sapply(mean) |> 
  print()

log_returns |> 
  select(returns, returns_naive, returns_sp500) |> 
  sapply(sd) |> 
  print()

W |> 
  pivot_longer(-ref_date, names_to = "ticker", values_to = "weights") |> 
  mutate(ticker = str_remove_all(ticker, "ticker_") |> 
           as.numeric() |> 
           factor(levels = 1:n)) |> 
  ggplot(aes(x = ref_date)) +
  geom_col(aes(y = weights, color = ticker, fill = ticker)) +
  xlab("data") + ylab("pesos")

clr = c(
  "Proposta" = "blue",
  "Naive" = "purple",
  "SP500" = "red"
)

log_returns |> ggplot(aes(x = ref_date)) +
  geom_line(aes(y = cum_returns_sp500, color = "SP500")) +
  geom_line(aes(y = cum_returns_naive, color = "Naive")) +
  geom_line(aes(y = cum_returns, color = "Proposta")) +
  scale_color_manual(values = clr) +
  guides(color = guide_legend(title = "Estratégia")) +
  xlab("data") + ylab("retornos acumulados")

log_returns |> ggplot() + 
  geom_density(aes(x = returns_sp500, color = "SP500", fill = "SP500"), alpha = .3) +
  geom_density(aes(x = returns_naive, color = "Naive", fill = "Naive"), alpha = .3) +
  geom_density(aes(x = returns, color = "Proposta", fill = "Proposta"), alpha = .3) +
  scale_color_manual(values = clr) + scale_fill_manual(values = clr) +
  guides(color = guide_legend(title = "Estratégia"), 
         fill = guide_legend(title = "Estratégia")) +
  xlab("retornos mensais") + ylab("densidade")

log_returns |> 
  mutate(`> SP500?` = ifelse(returns > returns_sp500, "sim", "não")) |> 
  ggplot(aes(x = returns_sp500, color = `> SP500?`)) + 
  geom_point(aes(y = returns))+
  xlab("retornos SP500") + ylab("retornos proposta")

log_returns |> 
  mutate(`> naive?` = ifelse(returns > returns_naive, "sim", "não")) |> 
  ggplot(aes(x = returns_naive, color = `> naive?`)) + 
  geom_point(aes(y = returns))+
  xlab("retornos naïve") + ylab("retornos proposta")

log_returns |> 
  ggplot(aes(x = returns_sp500)) +
  geom_point(aes(y = returns)) +
  geom_smooth(aes(y = returns), method = "lm") +
  xlab("retornos SP500") + ylab("retornos proposta")

log_returns |> 
  ggplot(aes(x = returns_sp500)) +
  geom_point(aes(y = returns_naive)) +
  geom_smooth(aes(y = returns_naive), method = "lm", color = "red") +
  xlab("retornos SP500") + ylab("retornos naïve")

# financial metrics
y = with(log_returns, xts(returns, ref_date))
y_naive = with(log_returns, xts(returns_naive, ref_date))
y_bench = with(log_returns, xts(returns_sp500, ref_date))

basicMetrics = function(y) c(Er = mean(y), S = sd(y))

M0 = data.frame(
  cbind(
    proposta = basicMetrics(y),
    naive = basicMetrics(y_naive),
    SP500 = basicMetrics(y_bench)
  )
)

arrayVar = function(q, y) VaR(y, q, method = "modified")

q = seq(.85, .99, by = .01)
Q  = data.frame(
  q = q,
  VaR_proposal = sapply(q, arrayVar, y = y),
  VaR_naive = sapply(q, arrayVar, y = y_naive),
  VaR_bench = sapply(q, arrayVar, y = y_bench)
)

Q |> ggplot(aes(x = q)) +
  geom_line(aes(y = VaR_bench, color = "SP500")) +
  geom_line(aes(y = VaR_naive, color = "Naive")) +
  geom_line(aes(y = VaR_proposal, color = "Proposta")) +
  scale_color_manual(values = clr) +
  guides(color = guide_legend(title = "Estratégia")) +
  xlab("confiança") + ylab("value at risk") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) + 
  labs(title = "VaR")

arrayES = function(q, y) ES(y, q, method = "gaussian")

E  = data.frame(
  q = q,
  ES_proposal = sapply(q, arrayES, y = y),
  ES_naive = sapply(q, arrayES, y = y_naive),
  ES_bench = sapply(q, arrayES, y = y_bench)
)

E |> ggplot(aes(x = q)) +
  geom_line(aes(y = ES_bench, color = "SP500")) +
  geom_line(aes(y = ES_naive, color = "Naive")) +
  geom_line(aes(y = ES_proposal, color = "Proposta")) +
  scale_color_manual(values = clr) +
  guides(color = guide_legend(title = "Estratégia")) +
  xlab("confiança") + ylab("expected shortfall") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) + 
  labs(title = "ES")


draw = maxDrawdown(y, geometric = F)
draw_naive = maxDrawdown(y_naive, geometric = F)
draw_bench = maxDrawdown(y_bench, geometric = F)

adj_SR = AdjustedSharpeRatio(y, geometric=F)
adj_SR_naive = AdjustedSharpeRatio(y_naive, geometric=F)
adj_SR_bench = AdjustedSharpeRatio(y_bench, geometric=F)

SR = SharpeRatio(y, geometric=F, annualize = TRUE)
SR_naive = SharpeRatio(y_naive, geometric=F, annualize = TRUE)
SR_bench = SharpeRatio(y_bench, geometric=F, annualize = TRUE)

sortino = SortinoRatio(y, geometric=F)
sortino_naive = SortinoRatio(y_naive, geometric=F)
sortino_bench = SortinoRatio(y_bench, geometric=F)

M1 = data.frame(
  proposta = c(draw, adj_SR, SR, sortino),
  naive = c(draw_naive, adj_SR_naive, SR_naive, sortino_naive),
  SP500 = c(draw_bench, adj_SR_bench, SR_bench, sortino_bench)
)

rownames(M1) = c("max drawdown", "Sharpe ajustado", "Sharpe stdev", "Sharpe VaR", "Sharpe ES", "Sortino")

exceed_returns = ActiveReturn(y, y_bench, scale = 12, geometric = FALSE)
exceed_returns_naive = ActiveReturn(y_naive, y_bench, scale = 12, geometric = FALSE)

appraisal = AppraisalRatio(y, y_bench)
jensen = CAPM.jensenAlpha(y, y_bench)

appraisal_naive = AppraisalRatio(y_naive, y_bench)
jensen_naive = CAPM.jensenAlpha(y_naive, y_bench)

M2 = data.frame(
  proposta = c(exceed_returns, appraisal, jensen),
  naive = c(exceed_returns_naive, appraisal_naive, jensen_naive)
)

rownames(M2) = c("excesso de retorno", "appraisal", "Jensen")

betaMeas = function(y, y_bench) c(
  Beta = CAPM.beta(y, y_bench),
  Beta_bull = CAPM.beta.bull(y, y_bench),
  Beta_bear = CAPM.beta.bear(y, y_bench),
  timing = TimingRatio(y, y_bench),
  cor = cor(y, y_bench)
)

M3 = data.frame(
  cbind(
    proposta = betaMeas(y, y_bench),
    naive = betaMeas(y_naive, y_bench)
  )
)



# modeling metrics

df_me = df_full_predictions |> 
  group_by(ticker) |> 
  summarise(
    me_returns = mean((.pred_rf - .actual)),
    me_vol = mean((pred_vol - actual_vol))
  )

df_me |> 
  mutate(
    classes = case_when(
      (me_returns < 0 & me_vol > 0) ~ "A",
      (me_returns < 0 & me_vol < 0) ~ "B",
      (me_returns > 0 & me_vol < 0) ~ "C",
      TRUE ~ "D"
    ) |> factor(levels = c("A", "B", "C", "D"), ordered = TRUE)
  ) |> 
  ggplot(aes(x = me_vol)) +
  geom_point(aes(y = me_returns, color = classes))
