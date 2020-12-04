library(tidyverse)
library(gridExtra)

# 1) Gamma distributions ------------------------------
n <- seq(0,1000, by = .01)
gamma1 <- dgamma(n,
                 shape = .001,
                 rate = .001)
plot2 <- data.frame(gamma1, n) %>% 
  ggplot(aes(x = n, y = gamma1)) +
  geom_line() +
  labs(title = "Gamma distribution", subtitle="scale = 0.001, rate = .001",
       x = "domain", y = "probability")

n <- seq(-10000,10000, by = 10)
norm1 <- dnorm(n, mean = 0, sd = 1000000)
plot1 <- data.frame(norm1, n) %>% 
  ggplot(aes(x = n, y = norm1)) +
  geom_line() + ylim(c(0,1)) +
  labs(title = "Normal distribution", subtitle="mean = 0, sd = 1e+06",
       x = "domain", y = "probability")

ggsave(plot = grid.arrange(plot1, plot2, ncol =2), 
       filename = "../12_Assignment2/imgs/00_prior_norm.png", 
       width = 16, height = 8, units = "cm")


