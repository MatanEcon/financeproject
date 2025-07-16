

returns<- qDT(read.csv(paste0(output, 'stock_indices_data.csv')))


returns[, year:=year(date)]

metrics <- data.table()

for(i in min(returns$year):2024){
  print(paste('year', i))
  subgroup <- returns[year==i]
  subgroup[, lag_returns := shift(returns, type = 'lag'), by = 'iso3']
  subgroup[, run := ifelse((returns>0 & lag_returns>0) |
                             (returns<=0 & lag_returns<=0), 1,0), 
           by ='iso3']
  
  subgroup[, N_s := sum((run*shift(run) + (1-run)*(1-shift(run))), na.rm = T), 
           by = 'iso3']
  subgroup[, N_r := .N - N_s, 
           by = 'iso3']
  # Calculate μ and σ per group
  stats <- subgroup[, .(
    mu = mean(returns, na.rm = TRUE),
    sigma = sd(returns, na.rm = TRUE),
    n = .N,
    N_s = max(N_s),
    N_r = max(N_r)
  ), by = .(iso3,year)]
  # Observed Cowles-Jones ratio
  stats[, cj_ratio := N_s / N_r]
  
  metrics <- rbind(metrics, stats)
  
}

#remove countrie-years with a very low amount of observations
metrics <- metrics[n>=150]
saveRDS(metrics, paste0(output, 'test_metrics.rds'))

rm(metrics, stats, subgroup, returns)
