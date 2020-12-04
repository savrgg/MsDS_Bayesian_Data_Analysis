#Load the OpenBUGS Package - make sure XQuartz is running
library(tidyverse)
library(R2OpenBUGS)

#define the model
nummodel <- function(){
  # Model 1) priori non informative
    # Model  
    # distribution of the sample means
    barx ~ dnorm(mu[1], tau.mean[1])
    bary ~ dnorm(mu[2], tau.mean[2])
    # distribution of the sample variances
    s2x  ~ dgamma(n1, tau1.2)   		
    s2y  ~ dgamma(n2, tau2.2)   		
    # Priors 
    for (j in 1 : 2){
      mu[j] ~ dnorm(0, 0.000001)    
      # standard deviation of error distribution
      sigma[j] <- sqrt(1 /  tau[j])}
    tau[1] ~ dgamma(12.34321, 1.111)
    tau[2] ~ dgamma(1000, 10)
    # introduce the difference between the means (cases-controls)
    delta <- mu[1] - mu[2]		  
    # introduce the precisions of the means 
    tau.mean[1] <-  tau[1]*n*N
    tau.mean[2] <-  tau[2]*m*N
    # introduce the scale of the gamma distribution (chi-square) 
    tau1.2 <- n*N* tau[1]/2
    tau2.2 <-  m*N*tau[2]/2
    # introduce the shape of the gamma distribution (chi-square) 
    n1 <-  (n*N-1)/2
    n2 <-  (m*N-1)/2
}

# write the model code out to a file
write.model(nummodel, "nummodel.txt")
model.file1 = paste(getwd(),"nummodel.txt", sep="/")
## and let's take a look:
file.show("nummodel.txt")

#prepare the data for input into OpenBUGS
n = 12   
m=10
N = 50
barx = 4.03
bary = 2.59
s2x = 0.0841
s2y = 0.0121
data <- list ("n", "m", "N", "barx", "bary", "s2x", "s2y")

#initialization of variables
inits <- function(){
  list(chain1 <- c("mu[1]=10", "mu[2] = 0'", "tau[1] = 0.1", "tau[2] = 0.5"),
       chain2 <- c("mu[1]=-10", "mu[2] = -10", "tau[1] = 0.2", "tau[2] = 0.2"))}

#set the WINE working directory and the directory to OpenBUGS - change the OpenBUGS.exe location as necessary
WINE="/usr/local/bin/wine"
WINEPATH="/usr/local/bin/winepath"
OpenBUGS.pgm="/Users/salvadorgarcia/.wine/drive_c/Program Files/OpenBUGS/OpenBUGS323/OpenBUGS.exe"

#these are the parameters to save
parameters = c("mu", "tau", "delta", "sigma")

#run the model
schools.sim <- bugs(data, inits, 
                    model.file = model.file1,
                    parameters=parameters,
                    n.chains = 2, 
                    n.iter = 11000, 
                    OpenBUGS.pgm=OpenBUGS.pgm,
                    WINE=WINE, 
                    WINEPATH=WINEPATH,
                    useWINE=T,
                    n.burnin = 1500)

schools.sim$summary %>% round(4) %>% write.table(pipe("pbcopy"))
modelo_low <- schools.sim
p1 <- schools.sim$sims.list$delta %>%
  data.frame() %>% setNames(c("Delta")) %>% 
  ggplot(aes(x= Delta))+
  geom_density() +
  labs(title = "Posterior distribution of delta", subtitle = "delta = mu_1 - mu_2")

p2 <- schools.sim$sims.list$mu[,1] %>%
  data.frame() %>% setNames(c("mu_1")) %>% 
  ggplot(aes(x= mu_1))+
  geom_density() +
  labs(title = "Posterior distribution of mu_1", subtitle = "Case group")

p3 <- schools.sim$sims.list$mu[,2] %>%
  data.frame() %>% setNames(c("mu_2")) %>% 
  ggplot(aes(x= mu_2))+
  geom_density() +
  labs(title = "Posterior distribution of mu_2", subtitle = "Control group")

p4 <- schools.sim$sims.list$sigma[,1] %>%
  data.frame() %>% setNames(c("sigma_1")) %>% 
  ggplot(aes(x= sigma_1))+
  geom_density() +
  labs(title = "Posterior distribution of sigma_1", subtitle = "Case group")

p5 <- schools.sim$sims.list$sigma[,2] %>%
  data.frame() %>% setNames(c("sigma_2")) %>% 
  ggplot(aes(x= sigma_2))+
  geom_density() +
  labs(title = "Posterior distribution of sigma_2", subtitle = "Control group")

ggsave(plot = grid.arrange(p2,p3,p4,p5,p1, ncol = 2), 
       filename = "../12_Assignment2/imgs/01_posterior_low.png", 
       width = 16, height = 14, units = "cm")



