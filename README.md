```
setwd("C:\Users\18829.DESKTOP-PG2BS5Q\Desktop\TEXTBOOK\start--5")
library(runjags)
library(coda)

testjags()

# The model
## Beta Binomial rate model 
modelstring = " 
model{
# Observed Counts
k ~ dbin(theta,n)
# Prior Distribution for Rate Theta
theta ~ dbeta(1,1)
}"

##JAGS uses dbin (instead of dbinom in R)
##dbeta takes two arguments instead of three (in R)

writeLines(modelstring, con="rate_text.txt")

## data
k <- 5 #number of successes
n <- 10 #number of trials

##MCMC
##In total, we tossed 10 kittens out of the window, and 5 landed on their feet.

## ship the data to JAGS, have to be packed in a list:
data = list( 
  k = k,
  n = n )

## Meta data
# bookkeeping
# number of chains
#Each chain is an independent MCMC run of the same model with the same data. 
#In our example, we will run two chains, so we set nchains = 2.
nchains <-  2   

# how many samples should be used for adaptation.
# In our example, we will use 500 adaptation samples, hence nadapt = 500.
nadapt <-  500  

#nburn sets the number of burn-in samples that you want to discard at the beginning of a sampling run. 
#As you know, the start of an MCMC run may be way off target, and we need a number of samples to get to the candy. 
#In our example, we will discard 1000 samples: nburn = 1000
nburn <-  1000  #number of burn-in samples 

# niter specifies the total number of samples we want to draw from the posterior of the parameters, 
#in this case θ. 
#we want to obtain 20,000 samples in total from the posterior of θ.
#each chain should get 10,000 samples: niter = 10000.
niter <-  10000 # total number of samples from posterior

# which parameters will be monitored.
#we are interested in the parameter θonly, we set parameters <- c(“theta”).
# parameters to be monitored 
parameters <- c("theta") 

# Initial/starting values
#starting value to start the markov chain.
#we have only one unobserved value /theta, and we can provide starting values for each chain.
myinits <- list(
  list(theta = 0.6), #chain 1 starting value
  list(theta = 0.9) #chain 2 starting value
)

# Run the model
#It asks JAGS to sample from the posterior, record the value of the unobserved variable θ for each sample.
out <- run.jags( model = "rate_simple.txt" , 
                 monitor = parameters ,
                 data = data ,  
                 inits = myinits , 
                 n.chains = nchains ,  
                 adapt = nadapt ,
                 burnin = nburn ,  
                 sample = niter )

# The sampled values of θ are stored in the variable out. Have a peak:
head(out$mcmc,3)

# Construct a density estimate, and compute values of interest. 
# Collect posterior samples across all chains:
theta <- combine.mcmc(out$mcmc)[,"theta"]
length(theta)

# Output
#Based on these sampled values, 
#you can compute the posterior mean, median, mode and credible intervals:

m <- mean(theta)       # mean of theta
md <- median(theta)    # median of theta
mode <- density(theta)$x[which.max(density(theta)$y)]   #mode of theta
s2 <- var(theta)    # the variance of the posterior 
q95 <- quantile(theta,c(.025,.975))   # 95% credible interval for theta

names(out)

# out
#out$summary
#out$summaries
#summary statistics of the posterior distribution can be obtained 
summary(out)

#The summary also contains the effective sample size (ESS), 
#which is the equivalent number of values if they were sampled independently of each other.

## histogram
#plots the posterior distribution of the rate θ, 
#approximated by a histogram of the samples:
par(cex.main = 1.5, 
    mar = c(5, 6, 4, 5) + 0.1, 
    mgp = c(3.5, 1, 0), 
    cex.lab = 1.5, 
    font.lab = 2, 
    cex.axis = 1.3, bty = "n", las=1)

Nbreaks <- 80
y       <- hist(theta, Nbreaks, plot=F)

plot(c(y$breaks, max(y$breaks)), c(0,y$density,0), type="S", lwd=2, lty=1,
     xlim=c(0,1), ylim=c(0,5), xlab="Rate", ylab="Posterior Density")


segments(c(y$breaks, max(y$breaks)), rep(0,89),
         c(y$breaks, max(y$breaks)),
         c(0,y$density,0), col=rgb(0,0,1, alpha=.2))

title(main =expression(paste("density plot of ", theta)))


##Or you could use a density plot or the coda package’s densplot:
par(mfrow=c(1,2))
plot(density(theta))
densplot(theta)

## Convergence
#The Rhat statistic provides information about the convergence of the sampling procedure, 
##not about the posterior distribution.
out$psrf

#PSRF estimates the potential decrease in the between-chains variability 
#with respect to the within-chain variability.
#If it is large, more samples are expected to either decrease between-chain-variability 
#or increase within-chain-variability 
#because the simulations have not yet explored the full posterior distribution.

# When PSRF < 1.2 for all model parameters, convergence has been reached. 
#Otherwise, longer chains or other means for improving the convergence may be needed.

thetak <- out$mcmc[,"theta"]
traceplot(thetak, 
          main = expression(paste("traceplot for ", theta)),
          ylab = expression(theta),
          smooth = TRUE)

# 图：Plot density plots, traceplots and autocorrelation plots all at once by using plot(out):
plot(out)
```
