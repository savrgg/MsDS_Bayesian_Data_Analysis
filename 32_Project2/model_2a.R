library(tidyverse)

xx <- read.csv("model1_hist1.csv", header = F)
xx %>% ggplot(aes(x = V2)) +
  geom_histogram(bins = 10) +
  labs(title = "Histogram model 1 - Posterior predictive check", y = "Frequency", x = "posterior predicted p-values")

xx <- read.csv("model2_hist1.csv", header = T) %>% 
  mutate(group = cut(mean, breaks = (0:10)/10, include.lowest = T))
xx %>% ggplot(aes(x = group)) +
  geom_bar() +
  labs(title = "Histogram model 2 - Posterior predictive check", y = "Frequency", x = "posterior predicted p-values")

xx <- read.csv("model2_hist2.csv", header = T) %>% 
  mutate(group = cut(mean, breaks = (0:10)/10, include.lowest = T))
xx %>% ggplot(aes(x = group)) +
  geom_bar() +
  labs(title = "Histogram model 2 - Mixed predictive check", y = "Frequency", x = "Mixed predicted p-values")


xx <- read.csv("model3_hist1.csv", header = T) %>% 
  mutate(group = cut(mean, breaks = (0:10)/10, include.lowest = T))
xx %>% ggplot(aes(x = group)) +
  geom_bar() +
  labs(title = "Histogram model 3 - Posterior predictive check", y = "Frequency", x = "posterior predicted p-values")

xx <- read.csv("model3_hist2.csv", header = T) %>% 
  mutate(group = cut(mean, breaks = (0:10)/10, include.lowest = T))
xx %>% ggplot(aes(x = group)) +
  geom_bar() +
  labs(title = "Histogram model 3 - Mixed predictive check", y = "Frequency", x = "Mixed predicted p-values")
