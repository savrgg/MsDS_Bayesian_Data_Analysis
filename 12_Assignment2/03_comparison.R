# comparison

# mu[1]
mu_1 <- data.frame(high_var = modelo_high$sims.list$mu[,1],
                   low_var = modelo_low$sims.list$mu[,1],
                   non_inf = modelo_noninf$sims.list$mu[,1]) %>% 
  mutate(par = "mu_1")
# mu[2]
mu_2 <- data.frame(high_var = modelo_high$sims.list$mu[,2],
                   low_var = modelo_low$sims.list$mu[,2],
                   non_inf = modelo_noninf$sims.list$mu[,2]) %>% 
  mutate(par = "mu_2")
# sigma[1]
sigma_1 <- data.frame(high_var = modelo_high$sims.list$sigma[,1],
                      low_var = modelo_low$sims.list$sigma[,1],
                      non_inf = modelo_noninf$sims.list$sigma[,1]) %>% 
  mutate(par = "sigma1")

# sigma[2]
sigma_2 <- data.frame(high_var = modelo_high$sims.list$sigma[,2],
                      low_var = modelo_low$sims.list$sigma[,2],
                      non_inf = modelo_noninf$sims.list$sigma[,2]) %>% 
  mutate(par = "sigma2")
# delta
delta <- data.frame(high_var = modelo_high$sims.list$delta,
                    low_var = modelo_low$sims.list$delta,
                    non_inf = modelo_noninf$sims.list$delta) %>% 
  mutate(par = "delta")

pars_df <- rbind(mu_1, mu_2, sigma_1, sigma_2, delta)
p1 <- pars_df %>% gather(variable, value, high_var:non_inf) %>%
  ggplot(aes(x = value, color = variable)) +
  geom_density() +
  facet_wrap(~par, scale = "free", ncol = 2)

ggsave(plot = p1, 
       filename = "../12_Assignment2/imgs/comparison.png", 
       width = 16, height = 14, units = "cm")


