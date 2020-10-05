library(tidyverse)

# config -----
N    = c(50,100,500,1000,2000,10000)
LIFT = c(seq(.01, .1, .01), .2, .5, 1) + 1
MU   = c(1,2,5,10,20,50,100)
SD   = c(1,2,5,10)

f_sim <- function(n_sim, N, lift, sd, mu){
  res <- list()
  for(i in 1:n_sim){
    
    res$values$n1a = rnorm(N * 0.50, sd = sd, mean = mu)
    res$values$n2a = rnorm(N * 0.50, sd = sd, mean = mu*lift)
    
    res$values$n1b = rnorm(N * 0.75, sd = sd, mean = mu)
    res$values$n2b = rnorm(N * 0.25, sd = sd, mean = mu*lift)
    
    res$values$n1c = rnorm(N * 0.90, sd = sd, mean = mu)
    res$values$n2c = rnorm(N * 0.10, sd = sd, mean = mu*lift)
    
    res$values$n1d = rnorm(N * 0.99, sd = sd, mean = mu)
    res$values$n2d = rnorm(max(N * 0.01, 5), mean = mu*lift)
    
    res$ttest$p.value$exp_5050[i] = t.test(res$values$n1a, res$values$n2a)$p.value
    res$ttest$p.value$exp_7525[i] = t.test(res$values$n1b, res$values$n2b)$p.value
    res$ttest$p.value$exp_9010[i] = t.test(res$values$n1c, res$values$n2c)$p.value
    res$ttest$p.value$exp_9901[i] = t.test(res$values$n1d, res$values$n2d)$p.value
    
    res$mw$p.value$exp_5050[i] = wilcox.test(res$values$n1a, res$values$n2a)$p.value
    res$mw$p.value$exp_7525[i] = wilcox.test(res$values$n1b, res$values$n2b)$p.value
    res$mw$p.value$exp_9010[i] = wilcox.test(res$values$n1c, res$values$n2c)$p.value
    res$mw$p.value$exp_9901[i] = wilcox.test(res$values$n1d, res$values$n2d)$p.value
  }
  
  res$tbl <- data.frame(
    weights = c('5050','7525','9010','9901'),
    n_sim = n_sim,
    mu = mu,
    N = N,
    lift = (lift-1)*100,
    sd = sd,
    power_ttest = c(
      mean(res$ttest$p.value$exp_5050 < .05),
      mean(res$ttest$p.value$exp_7525 < .05),
      mean(res$ttest$p.value$exp_9010 < .05),
      mean(res$ttest$p.value$exp_9901 < .05)
    ),
    power_mw = c(
      mean(res$mw$p.value$exp_5050 < .05),
      mean(res$mw$p.value$exp_7525 < .05),
      mean(res$mw$p.value$exp_9010 < .05),
      mean(res$mw$p.value$exp_9901 < .05)
    )
  )
  return(res$tbl)
}

f_report <- function(n_sim = 1000, N_list, mu_list, sd_list, lift_list){
  i = 1
  sims <- list()
  for(N in 1:length(N_list)){
    for(mu in 1:length(mu_list)){
      for(sd in 1:length(sd_list)){
        for(lift in 1:length(lift_list)){
          print(paste(N_list[N], mu_list[mu], sd_list[sd], lift_list[lift], collapse = " , "))
          sims$tbls[[i]] <- f_sim(
            n_sim     = n_sim,
            N         = N_list[N],
            lift      = lift_list[lift],
            mu        = mu_list[mu],
            sd        = sd_list[sd]
          )
          i = i + 1
        }
      }
    } 
  }
  return(sims)
}

# run -----
res <- f_report(
  n_sim = 1000,
  N_list = N,
  mu_list = MU,
  sd_list = SD,
  lift_list = LIFT
)

res <- do.call(bind_rows, res)
