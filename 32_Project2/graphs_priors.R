library(tidyverse)

data.frame(y = dnorm(seq(-100,100,by = .1), mean = 0, sd = sqrt(1000)),
           x = seq(-100,100,by = .1)) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_line() + ylim(c(0,1)) +
  labs(title = "Normal non-informative distribution for each mu[i]",
       x = "mu", 
       y = "f(x)") +
  theme_bw()