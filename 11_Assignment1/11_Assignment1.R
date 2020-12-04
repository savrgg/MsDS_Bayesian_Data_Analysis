library(tidyverse)

# 0) functions for plot -------------------------------
posterior_plot <- function(prior, likelihood, posterior, title, filename){
  post_df <- data.frame(theta = n, 
                        likelihood = likelihood,
                        prior = prior,
                        posterior = posterior)
  plot <- post_df %>% 
      gather(Distributions, value, likelihood:posterior) %>% 
      ggplot(aes(x = theta, y = value, group = Distributions, color = Distributions))+
      geom_line() +
      labs(title = title) +
      theme_bw()
  ggsave(filename = filename, 
         plot = plot, width = 5, height = 3)
}
# 1) priors comparison --------------------------------
dir.create("~/Repositories/UoE_2_BDA/11_Assignment1/")
n = seq(.01, .99, .01)
beta_noninf = dbeta(n, 1/2, 1/2)
uniform_noninf = dunif(n, 0, 1)
beta_inf = dbeta(n, 21, 10)
priors <- data.frame(theta_1 = n,
                      beta_noninf, 
                      uniform_noninf, 
                      beta_inf)

(gg_priors <- priors %>% 
  gather(Distributions, value, beta_noninf:beta_inf) %>% 
  ggplot(aes(x = theta_1, y = value, group = Distributions, color = Distributions)) +
  geom_line() +
  labs(title = "Prior distributions for the problem") +
  theme_bw())

ggsave(filename = "~/Repositories/UoE_2_BDA/11_Assignment1/priors_dist.png", 
       plot = gg_priors, width = 5, height = 3)

# 2) Posterior for parameter \theta_1 -----------------
theta1_likelihood = dbeta(n, 51, 60)

# uniform prior
theta1_c1_posterior = dbeta(n, 51, 60)

posterior_plot(prior = uniform_noninf,
               likelihood = theta1_likelihood, 
               posterior = theta1_c1_posterior,
               title = "Posterior of theta_1, given uniform prior",
               filename = "~/Repositories/UoE_2_BDA/11_Assignment1/theta1_c1_posterior.png")

# Jeffrey's prior
theta1_c2_posterior = dbeta(n, 50.5, 59.5)

posterior_plot(prior = beta_noninf,
               likelihood = theta1_likelihood, 
               posterior = theta1_c2_posterior,
               title = "Posterior of theta_1, given Jeffrey's prior",
               filename = "~/Repositories/UoE_2_BDA/11_Assignment1/theta1_c2_posterior.png")

# Informative prior
theta1_c3_posterior = dbeta(n, 72, 70)

posterior_plot(prior = beta_inf,
               likelihood = theta1_likelihood, 
               posterior = theta1_c3_posterior,
               title = "Posterior of theta_1, given informative prior",
               filename = "~/Repositories/UoE_2_BDA/11_Assignment1/theta1_c3_posterior.png")
# 3) Posterior for parameter \theta_2 -----------------
theta2_likelihood = dbeta(n, 91, 31)

# uniform prior
theta2_c1_posterior = dbeta(n, 91, 31)

posterior_plot(prior = uniform_noninf,
               likelihood = theta2_likelihood, 
               posterior = theta2_c1_posterior,
               title = "Posterior of theta_2, given uniform prior",
               filename = "~/Repositories/UoE_2_BDA/11_Assignment1/theta2_c1_posterior.png")

# Jeffrey's prior
theta2_c2_posterior = dbeta(n, 90.5, 30.5)

posterior_plot(prior = beta_noninf,
               likelihood = theta2_likelihood, 
               posterior = theta2_c2_posterior,
               title = "Posterior of theta_2, given Jeffrey's prior",
               filename = "~/Repositories/UoE_2_BDA/11_Assignment1/theta2_c2_posterior.png")

# 4) summaries ----------------------------------------
# Selected priors: Jeffrey priors

theta1_c2_posterior = dbeta(n, 50.5, 59.5)
theta1_c2_mean = 50.5/(50.5+59.5)
theta1_c2_std = sqrt(50.5*59.5)/(((50.5+59.5)^2)*(50.5+59.5+1))
theta1_c2_median = qbeta(.5, 50.5, 59.5)
theta1_c2_CI = qbeta(c(.1, .9), 50.5, 59.5)

theta2_c2_posterior = dbeta(n, 90.5, 30.5)
theta2_c2_mean = 90.5/(90.5+30.5)
theta2_c2_std = sqrt(90.5*30.5)/(((90.5+30.5)^2)*(90.5+30.5+1))
theta2_c2_median = qbeta(.5, 90.5, 30.5)
theta2_c2_CI = qbeta(c(.1, .9), 90.5, 30.5)

CI_gg <- data.frame(distribution = c("Boys", "Girls"),
           mean = c(theta1_c2_mean, theta2_c2_mean),
           std = c(theta1_c2_std, theta2_c2_std),
           median = c(theta1_c2_median, theta2_c2_median),
           CI_low = c(theta1_c2_CI[1], theta2_c2_CI[1]),
           CI_high = c(theta1_c2_CI[2], theta2_c2_CI[2])) %>% 
  ggplot(aes(x = distribution)) +
  geom_point(aes(y = mean, color = distribution)) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high, 
                    width = .1, color = distribution)) +
  labs(title = "Mean and credibility intervals") +
  theme_bw()
  
ggsave(filename = "~/Repositories/UoE_2_BDA/11_Assignment1/CI_intervals.png", 
       plot = CI_gg, width = 5, height = 3)

# 5) Posterior distribution comparisons ---------------
diff_gg <- data.frame(theta = n,
           theta1_c2_posterior,
           theta2_c2_posterior) %>% 
  gather(Poblation, value, theta1_c2_posterior:theta2_c2_posterior) %>% 
  ggplot(aes(x = theta, y = value, group = Poblation, color = Poblation)) +
  geom_line() +
  labs(title = "Diff. in distributions with Jeffrey's prior") +
  theme_bw()
ggsave(filename = "~/Repositories/UoE_2_BDA/11_Assignment1/diff.png", 
       plot = diff_gg, width = 5, height = 3)