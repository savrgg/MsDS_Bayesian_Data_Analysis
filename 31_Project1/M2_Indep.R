# 0) load data and libraries --------------------------
source("~/Repositories/BayesianModels/Stan/Project1/01_LoadDataset.R")

names(full_d)
niter = 150000
nchains <- 2

# 1) pooled 2 pars ----------------------------------------
indep_2pars <- stan('2_indep_2pars.stan', 
                     data = full_d,
                     iter = niter, 
                     chains = nchains,
                     cores = 2,
                     thin = 10,
                     verbose = TRUE)
save(indep_2pars, file = "2_indep_2pars.Rdata")
tidy(indep_2pars)
stan_trace(indep_2pars, pars = "beta_1", inc_warmup = FALSE) +
  ylim(c(2.5,10))
stan_trace(indep_2pars, pars = "beta_2", inc_warmup = FALSE) +
  ylim(c(.4,1))
stan_ac(indep_2pars, pars = c("beta_1", "beta_2"), inc_warmup = FALSE)

# 2) pooled 3 pars ----------------------------------------
indep_3pars <- stan('2_indep_3pars.stan', 
                     data = full_d,
                     iter = niter, 
                     chains = nchains,
                     cores = 2,
                     thin = 10,
                     verbose = TRUE)
save(indep_3pars, file = "2_indep_3pars.Rdata")
tidy(indep_3pars)
stan_trace(indep_3pars, pars = "beta_1", inc_warmup = FALSE) +
  ylim(c(0,110))
stan_trace(indep_3pars, pars = "beta_2", inc_warmup = FALSE) +
  ylim(c(0,3.5))
stan_trace(indep_3pars, pars = "beta_3", inc_warmup = FALSE) +
  ylim(c(0,3))

  
