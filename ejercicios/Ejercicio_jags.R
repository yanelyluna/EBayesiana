library(rjags)
library(tseries)
library(nortest)
library(lmtest)
library(rriskDistributions)
library(datarium)
data("marketing")

x <- marketing$youtube
y <- marketing$sales
n <- length(x)

data <- list(
  y=y,
  x=x,
  n=n,
  zeros=c(0,0), #media
  diagonal=diag(0.001,2) #varianza
)

inits <- function(){list(
  "Beta"=rnorm(2,0,1),
  "Tau"=rgamma(1,1,1) #Precisión
)}

param <- c("Beta", "Sigma2")

fit <- jags.model("rlm_jags.bug",data,inits,n.chains = 3)

update(fit,1000)

sample <- coda.samples(fit, param,n.iter = 4000,thin = 1)

summary(sample)

# Iterations = 1001:5000
# Thinning interval = 1 
# Number of chains = 3 
# Sample size per chain = 4000 
# 
# 1. Empirical mean and standard deviation for each variable,
# plus standard error of the mean:
#   
#   Mean     SD  Naive SE Time-series SE
# Beta[1]  8.43116 0.5486 5.008e-03      4.987e-03
# Beta[2]  0.04758 0.0027 2.465e-05      2.465e-05
# Sigma2  15.44171 1.5713 1.434e-02      1.418e-02
# 
# 2. Quantiles for each variable:
#   
#   2.5%      25%      50%      75%    97.5%
# Beta[1]  7.34288  8.05705  8.43781  8.80035  9.50030
# Beta[2]  0.04225  0.04577  0.04758  0.04937  0.05291
# Sigma2  12.68150 14.34722 15.33223 16.41920 18.81341
plot(sample)
