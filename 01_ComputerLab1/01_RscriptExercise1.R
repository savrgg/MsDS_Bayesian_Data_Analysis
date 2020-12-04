#------------
# Data
#------------

# number of cases

n=12

# number of controls

m=10

# number of genes 

N=50

# sample mean and s.d. for cases

barx = 4.03 
sx = 0.29

# sample mean and s.d. for controls

bary = 2.59
sy = 0.11

#----------------------------------------------
# likelihood
#----------------------------------------------

# cases, mu1 and sigma1 

# barX \sim N(\mu_1, \sigma_1^2/(nN))
# Sx^2/\sigma_1^2 \sim \chisq^2_{Nn-1}

#  controls, mu2 and sigma2

# barY \sim N(\mu_2, \sigma_2^2/(mN))
# Sy^2/\sigma_2^2 \sim \chisq^2_{Nm-1}

# plot the likelihood

z =seq(0.1, 7,0.05)

# take 
sigma1= 0.3/sqrt(n*N)
sigma2= 0.1/sqrt(m*N)

# for mu1 (cases)
 plot(z, dnorm(barx, z, sigma1), type='l', xlab="mu1, cases", ylab="", main="Likelihood")

# for mu2 (controls)
 plot(z, dnorm(bary, z, sigma2), type='l', xlab="mu2, controls", ylab="", main="Likelihood")

# (can adjust the x axis)

#----------------------------------------------
#----------------------------------------------
# priors for mu2
#----------------------------------------------
#----------------------------------------------


# prior 1: p(mu)=1 (Jeffreys prior)

 plot(z, rep(1,length(z)), type='l', xlab="mu2", ylab="")
 title("Uniform prior (improper)")

# prior 2:  mu \sim N(0,A^2), $A=10^3$

A= 1000

 plot(z, dnorm(z,0,A), type='l', xlab="mu2", ylab="")
 title("Proper approximation of uniform prior, N(0,A^2)")

# prior 3:  mu \sim N(mu0,sigma0^2) 

mu0 =2.38
sigma0 =0.04

 plot(z, dnorm(z,mu0,sigma0), type='l', xlab="mu2", ylab="")
 title("Informative conjugate prior")

#-------------------------------------------------------
# posterior for mu2 (and likelihood and the prior)
#-------------------------------------------------------

#------------------
# prior 1: p(mu)=1
#------------------

#posterior
 plot(z, dnorm( z, bary, sigma2), type='l', xlab="mu2, controls", ylab="")
#likelihood
 lines(z, dnorm(bary, z, sigma2), col=4)
#prior
 lines(z, rep(1,length(z)), col=2)

 title("Uniform prior (improper)")

#-------------------------------------------
# prior 2:  mu \sim N(0,A^2), $A=10^3$
#-------------------------------------------

A= 100

tauL = 1/sigma2^2
tauP = 1/A^2

sigmaP2=1/sqrt(tauL+tauP)
muP2 = tauL*bary/(tauL+tauP)

#posterior
 plot(z, dnorm(z, muP2, sigmaP2), type='l', xlab="mu2, controls", ylab="")
#likelihood
 lines(z, dnorm(bary, z, sigma2), col=4)
#prior
 lines(z, dnorm(z,0,A), col=2)

 title("Proper approximation of uniform prior, N(0,A^2)")

#------------------------------------
# prior 3:  mu \sim N(mu0,sigma0^2) 
#------------------------------------

tauL = 1/sigma2^2
tauP = 1/sigma0^2

sigmaP3=1/sqrt(tauL+tauP)
muP3 = (tauL*bary+tauP*mu0)/(tauL+tauP)

# z = seq(2.2,2.7, 0.001)

z = seq(2.57,2.61, 0.001)


#posterior
 plot(z, dnorm(z, muP3, sigmaP3), type='l', xlab="mu2, controls", ylab="")
#likelihood
 lines(z, dnorm(bary, z, sigma2), col=4)
#prior
 lines(z, dnorm(z,mu0,sigma0), col=2)

title("Informative conjugate prior")


c(muP3, sigmaP3 )  


#-------------------------------------------------------------
# posterior summaries 

# mean and median coincide for normal distribution 

#------------
# flat prior
#------------

# mean, median 

bary

# standard deviation

sigma2

# 5% and 95% quantiles

qnorm(c(0.05,0.95), bary, sigma2)

# summarise in a table

rownames0 = c("Uniform", "Proper noninf", "Informative") 
colnames0 = c("Mean", "Median", "S.d.", "5perc", "95perc")

post.summary = array(dim=c(3,5), dimnames=list(rownames0, colnames0) )

post.summary[1,]= c(bary,bary,sigma2,qnorm(c(0.05,0.95), bary, sigma2))
post.summary[2,]= c(muP2,muP2,sigmaP2,qnorm(c(0.05,0.95), muP2,sigmaP2))
post.summary[3,]= c(muP3,muP3,sigmaP3,qnorm(c(0.05,0.95), muP3,sigmaP3))
 
post.summary

round(post.summary, 3)

#writes in the working directory: getwd()
write.csv(post.summary, file = "PosteriorSummaryExercise1_controls.csv")


#--------------------------------------------------------------
#--------------------------------------------------------------


#----------------------------------------------
#----------------------------------------------
# priors for mu1
#----------------------------------------------
#----------------------------------------------


z =seq(0.1, 7,0.05)

z =seq(3.5, 4.5,0.01)


# prior 1: p(mu)=1 (Jeffreys prior)

 plot(z, rep(1,length(z)), type='l', xlab="mu1", ylab="")
 title("Uniform prior (improper)")

# prior 2:  mu \sim N(0,A^2), $A=10^3$

A= 1000

 plot(z, dnorm(z,0,A), type='l', xlab="mu1", ylab="")
 title("Proper approximation of uniform prior, N(0,A^2)")

#-------------------------------------------------------
# posterior for mu1 (and likelihood and the prior)
#-------------------------------------------------------

#------------------
# prior 1: p(mu)=1
#------------------

#posterior
 plot(z, dnorm( z, barx, sigma1), type='l', xlab="mu1, cases", ylab="")

#likelihood
 lines(z, dnorm(barx, z, sigma1), col=4)
#prior
 lines(z, rep(1,length(z)), col=2)

 title("Uniform prior (improper)")

#-------------------------------------------
# prior 2:  mu \sim N(0,A^2), $A=10^3$
#-------------------------------------------

#A= 1000

tauL4 = 1/sigma1^2
tauP4 = 1/A^2

sigmaP4=1/sqrt(tauL4+tauP4)
muP4 = tauL4*barx/(tauL4+tauP4)

#posterior
 plot(z, dnorm(z, muP4, sigmaP4), type='l', xlab="mu1, cases", ylab="")
#likelihood
 lines(z, dnorm(barx, z, sigma1), col=4)
#prior
 lines(z, dnorm(z,0,A), col=2)

 title("Proper approximation of uniform prior, N(0,A^2)")

#-------------------------------------------------------------
# posterior summaries 

# mean and median coincide for normal distribution 

#------------
# flat prior
#------------

# summarise in a table

rownames1 = c("Uniform", "Proper noninf") 
colnames1 = c("Mean", "Median", "S.d.", "5perc", "95perc")

post.summary.case = array(dim=c(2,5), dimnames=list(rownames1, colnames1) )

post.summary.case[1,]= c(barx,barx,sigma1,qnorm(c(0.05,0.95), barx, sigma1))
post.summary.case[2,]= c(muP4,muP4,sigmaP4,qnorm(c(0.05,0.95), muP4,sigmaP4))

post.summary.case

round(post.summary.case, 3)

#writes in the working directory: getwd()
write.csv(post.summary.case, file = "PosteriorSummaryExercise1_case.csv")

#--------------------------------------------------------------
#--------------------------------------------------------------


#--------------------------------------------------------------
#--------------------------------------------------------------
# comparison of the distribution of mu1 and mu2
#--------------------------------------------------------------
#--------------------------------------------------------------

# Use no-informative prior (either flat or proper approximation, 
# since the results are identical, to the considered precision)

# posterior summaries 

post.summary.uniform = array(dim=c(2,5), dimnames=list(c("mu1", "mu2"), colnames1) )

post.summary.uniform[1,] = post.summary.case[1,]
post.summary.uniform[2,] = post.summary[1,]

post.summary.uniform

# the 95% credible intervals do not overlap, 
# so the available evidence suggest that that the gene concentrations (e^mu1 and e^mu2)
# differ in cases and controls, i.e. in those with and without pancreatic cancer. 

#-------------------------------------------
# posterior distribution of d= mu1- mu2

# is N(barx-bay, sigma1^2 + sigma2^2)

w=seq(0.8,1.8,0.001)

# plot 
 plot(w, dnorm(w, barx-bary, sqrt(sigma1^2 + sigma2^2)), type='l', xlab="mu1-mu2 ", ylab="")
 title("Posterior distribution of mu1- mu2")

# posterior summaries

post.summary.diff = array(dim=c(1,5), dimnames=list(c("mu1-mu2"), colnames1) )
post.summary.diff[1,] = c(barx-bary,barx-bary,sqrt(sigma1^2 + sigma2^2),qnorm(c(0.05,0.95), barx-bary,sqrt(sigma1^2 + sigma2^2)))
post.summary.diff

# 95% credible interval does not contain 0, so the same conclusion

# find P(mu1-mu2 >0 | data)

pnorm(0, barx-bary,sqrt(sigma1^2 + sigma2^2))

# With probability close to 1, the posterior distribution of mu1-mu2 is positive, 
# so the observed data provided strong evidence that there is a difference in gene expression 
# between those with and without pancreatic cancer. 

