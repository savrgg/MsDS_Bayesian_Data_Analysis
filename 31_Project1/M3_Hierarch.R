# 0) load data and libraries --------------------------
source("~/Repositories/BayesianModels/Stan/Project1/01_LoadDataset.R")

names(full_d)
niter = 150000
nchains <- 2

# 1) pooled 2 pars ----------------------------------------
hierarch_2pars <- stan('3_hierarch_2pars.stan', 
                     data = full_d,
                     iter = niter, 
                     chains = nchains,
                     cores = 2,
                     thin = 10,
                     verbose = TRUE)
save(hierarch_2pars, file = "3_hierarch_2pars.Rdata")
tidy(hierarch_2pars)
stan_trace(hierarch_2pars, pars = "beta_1", inc_warmup = FALSE) +
  ylim(c(2.5,10))
stan_trace(hierarch_2pars, pars = "beta_2", inc_warmup = FALSE) +
  ylim(c(.4,1))
stan_ac(hierarch_2pars, pars = c("beta_1", "beta_2"), inc_warmup = FALSE)

# 2) pooled 3 pars ----------------------------------------
hierarch_3pars <- stan('3_hierarch_3pars.stan', 
                     data = full_d,
                     iter = niter, 
                     chains = nchains,
                     cores = 2,
                     thin = 10,
                     verbose = TRUE)
save(hierarch_3pars, file = "3_hierarch_3pars.Rdata")
tidy(hierarch_3pars)
stan_trace(hierarch_3pars, pars = c("beta_1", "beta_2", "beta_3"), inc_warmup = FALSE)
stan_trace(hierarch_3pars, pars = "beta_1", inc_warmup = FALSE) +
  ylim(c(0,90))
stan_trace(hierarch_3pars, pars = "beta_2", inc_warmup = FALSE) +
  ylim(c(0,3))
stan_trace(hierarch_3pars, pars = "beta_3", inc_warmup = FALSE) +
  ylim(c(0,1))
stan_ac(hierarch_3pars, pars = c("beta_1", "beta_2", "beta_3"), inc_warmup = FALSE)
