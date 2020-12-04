# 0) load libraries -----------------------------------
library(tidyverse)
library(broom)
library(rstan)
library(stringr)

niter = 40000
nchains <- 2

full_d <- list(npupil = 229,         
               p = 9,         
               School = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,
                        2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,3,3,
                        3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
                        3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,
                        4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,5,5,5,
                        5,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,6,6,6,6,
                        6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,
                        7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,
                        8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
                        8,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9),
               Goals=c(0,1,1,1,1,1,1,0,0,0,1,1,1,0,1,0,1,1,1,1,1,0,0,1,
                       0,0,1,1,1,0,1,0,0,1,0,0,1,1,1,1,1,1,1,0,1,1,1,0,
                       1,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,1,0,0,0,1,1,1,0,
                       0,1,1,0,1,0,0,0,1,1,0,1,0,0,0,1,1,1,0,1,0,1,1,0,
                       0,0,1,1,0,0,0,1,1,0,1,1,0,1,1,1,0,1,1,0,0,0,1,1,
                       1,1,1,0,0,1,1,1,0,1,1,1,1,1,1,0,0,0,0,0,1,0,0,0,
                       1,0,1,0,0,1,0,0,1,0,1,0,1,0,1,0,0,0,1,1,1,0,1,1,
                       1,1,0,0,0,1,1,0,0,0,0,1,1,1,1,1,1,1,1,1,0,0,0,1,
                       1,1,0,1,1,0,1,1,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,
                       1,1,1,0,1,1,1,0,0,1,0,1,1))    

fit_pooled <- stan('Stan/model_pooled.stan', 
                   data = full_d,
                   iter = niter, 
                   chains = nchains,
                   verbose = TRUE)
fit_indepe <- stan('Stan/model_independent.stan', 
                   data = full_d,
                   iter = niter, 
                   chains = nchains, 
                   verbose = TRUE)
fit_hierar <- stan('Stan/model_hierarchical.stan', 
                   data = full_d,
                   iter = niter, 
                   chains = nchains, 
                   thin = 30,
                   verbose = TRUE)
fit_hierar_2 <- stan('Stan/model_hierarchical_2.stan', 
                   data = full_d,
                   iter = niter, 
                   chains = nchains, 
                   thin = 30,
                   verbose = TRUE)

rbind(tidy(fit_pooled) %>% mutate(model = "pooled"),
      tidy(fit_indepe) %>% mutate(model = "indepe"),
      tidy(fit_hierar) %>% mutate(model = "hierar gamma(.1,.1)"),
      tidy(fit_hierar_2) %>% mutate(model = "hierar gamma(1,1)")) %>% 
  filter(str_detect(term, "theta")) %>% 
  ggplot(aes(x = term, y = estimate, 
             group = model, color = model)) +
  geom_errorbar(aes(ymin = estimate-2*std.error, 
                    ymax = estimate+2*std.error), alpha = .5)+
  geom_point() +
  geom_hline(aes(yintercept = 0.6106351), color = "dodgerblue4")


