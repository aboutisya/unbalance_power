# config -----
N    = c(50,100,500,1000,2000,10000)
LIFT = c(seq(.01, .1, .01), .2, .5, 1) + 1
MU   = c(1,2,5,10,20,50,100)
SD   = c(1,2,5,10)

f_sim <- function(n_sim, N, lift, sd, mu){
  exp_split <- c(0.5, 0.75, 0.9, 0.99)
  res <- sapply(1:n_sim, function(x) {
    lapply(exp_split, function(y) {
      list(
        "t.test" = t.test(rnorm(N * y, mean = mu, sd = sd), rnorm(N * (1 - y), mean = mu * lift, sd = sd))$p.value,
        "wilcox" = wilcox.test(rnorm(N * y, mean = mu, sd = sd), rnorm(N * (1 - y), mean = mu * lift, sd = sd))$p.value
      )
    })
  })
  return(
    data.frame(
      weights = c('5050','7525','9010','9901'),
      n_sim = n_sim,
      mu = mu,
      N = N,
      lift = (lift - 1) * 100,
      sd = sd,
      power_ttest = sapply(1:length(exp_split), function(x) {
        t <- unlist(res[x, ])
        mean(t[which(names(t) == 't.test')] < .05)
      }),
      power_mw = sapply(1:length(exp_split), function(x) {
        t <- unlist(res[x, ])
        mean(t[which(names(t) == 'wilcox')] < .05)
      })
    )
  )
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
  N_list = N,
  mu_list = MU,
  sd_list = SD,
  lift_list = LIFT
)

res <- do.call(bind_rows, res)

