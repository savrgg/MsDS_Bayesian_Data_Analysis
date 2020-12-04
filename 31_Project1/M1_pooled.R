# 0) load data and libraries --------------------------
source("~/Repositories/BayesianModels/Stan/Project1/01_LoadDataset.R")

names(full_d)
niter = 150000
nchains <- 2

# 1) pooled 2 pars ----------------------------------------
pooled_2pars <- stan('1_pooled_2pars.stan', 
                   data = full_d,
                   iter = niter, 
                   chains = nchains,
                   cores = 2,
                   thin = 10,
                   verbose = TRUE)
save(pooled_2pars, file = "1_pooled_2pars.Rdata")
tidy(pooled_2pars)
stan_trace(pooled_2pars, pars = c("beta_1", "beta_2"), inc_warmup = FALSE)
stan_ac(pooled_2pars, pars = c("beta_1", "beta_2"), inc_warmup = FALSE)

# 2) pooled 3 pars ----------------------------------------
pooled_3pars <- stan('1_pooled_3pars.stan', 
                 data = full_d,
                 iter = niter, 
                 chains = nchains,
                 cores = 2,
                 thin = 10,
                 verbose = TRUE)
save(pooled_3pars, file = "1_pooled_3pars.Rdata")
tidy(pooled_3pars)
stan_trace(pooled_3pars, pars = c("beta_1", "beta_2", "beta_3"), inc_warmup = FALSE)
stan_ac(pooled_3pars, pars = c("beta_1", "beta_2", "beta_3"), inc_warmup = FALSE)
