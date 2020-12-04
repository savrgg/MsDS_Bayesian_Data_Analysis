# 0) load libraries -----------------------------------
library(tidyverse)
library(broom)
library(rstan)
library(stringr)

niter = 10000
nchains <- 2

full_d <- list(Ndoses = 6, Nplates = 3,
     y = structure(.Data = c(15,21,29,16,18,21,16,26,33,27,41,60,33,38,41,20,27,42),
                   .Dim = c(6, 3)),
     x = c(0, 10, 33, 100, 333, 1000))

fit_pooled <- stan('model_orig.stan', 
                   data = full_d,
                   iter = niter, 
                   chains = nchains,
                   verbose = TRUE)


# 
# fit_indepe <- stan('Stan/model_independent.stan', 
#                    data = full_d,
#                    iter = niter, 
#                    chains = nchains, 
#                    verbose = TRUE)
# fit_hierar <- stan('Stan/model_hierarchical.stan', 
#                    data = full_d,
#                    iter = niter, 
#                    chains = nchains, 
#                    thin = 30,
#                    verbose = TRUE)
# fit_hierar_2 <- stan('Stan/model_hierarchical_2.stan', 
#                      data = full_d,
#                      iter = niter, 
#                      chains = nchains, 
#                      thin = 30,
#                      verbose = TRUE)
# 
# rbind(tidy(fit_pooled) %>% mutate(model = "pooled"),
#       tidy(fit_indepe) %>% mutate(model = "indepe"),
#       tidy(fit_hierar) %>% mutate(model = "hierar gamma(.1,.1)"),
#       tidy(fit_hierar_2) %>% mutate(model = "hierar gamma(1,1)")) %>% 
#   filter(str_detect(term, "theta")) %>% 
#   ggplot(aes(x = term, y = estimate, 
#              group = model, color = model)) +
#   geom_errorbar(aes(ymin = estimate-2*std.error, 
#                     ymax = estimate+2*std.error), alpha = .5)+
#   geom_point() +
#   geom_hline(aes(yintercept = 0.6106351), color = "dodgerblue4")
# 

