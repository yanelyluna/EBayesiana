########
library(polycor)
library(corrplot)
library(car)

datos <- read.csv("datos/Datoslog.csv")
str(datos)

xtabs(~admit+rank, data=datos)
hist(datos$gpa)
table(datos$admit)

aux_cor <- hetcor(datos)
aux_cor2 <- aux_cor$correlations
corrplot(aux_cor2)

#---- Regresión logística
modelo1 <- glm(admit~gre+gpa+factor(rank),data = datos,
               family = binomial(link = "logit"))
summary(modelo1)

confint(modelo1)

vif(modelo1) #Menor a 10, no hay problemas de colinealidad

prediccion_evaluacion <- ifelse(modelo1$fitted.values>=0.5,1,0)
table(datos$admit,prediccion_evaluacion)

(244+35)/400 #0.6975
1-(244+35)/400 #Error de clasificación 30.25%

##############
library(rjags)
attach(datos)

data <- list(
  y=admit,
  x1=gre,
  x2=gpa,
  x3=rank,
  n=length(admit)
)

param <- c("alpha","Beta1","Beta2","Beta3")
inits <-  function() {list(
  "alpha"=rnorm(1),
  "Beta1"=rnorm(1),
  "Beta2"=rnorm(1),
  "Beta3"=rnorm(1)
)
  
}

modelo=" model {
  for(i in 1:n){
    y[i]~dbern(p[i])
    p[i] <- 1/(1.0001+exp(-(alpha+Beta1*x1[i]+Beta2*x2[i]+Beta3*x3[i])))
  }
  
  alpha ~ dnorm(0.0,1.0E-2)
  Beta1 ~ dnorm(0.0,1.0E-2)
  Beta2 ~ dnorm(0.0,1.0E-2)
  Beta3 ~ dnorm(0.0,1.0E-2)
}

"
fit <- jags.model(textConnection(modelo),data,inits,n.chains=3)

update(fit,1000)

sample <- coda.samples(fit,param,n.iter = 4000,thin = 1)

dev.new()
plot(sample)

gelman.plot(sample)

summary(sample)

# Iterations = 2001:6000
# Thinning interval = 1 
# Number of chains = 3 
# Sample size per chain = 4000 
# 
# 1. Empirical mean and standard deviation for each variable,
# plus standard error of the mean:
#   
#   Mean       SD  Naive SE Time-series SE
# Beta1  0.003537 0.001084 9.896e-06      0.0000849
# Beta2  0.663338 0.320744 2.928e-03      0.0436962
# Beta3 -0.472965 0.125317 1.144e-03      0.0047645
# alpha -3.920014 1.089134 9.942e-03      0.1428954
# 
# 2. Quantiles for each variable:
#   
#   2.5%      25%       50%       75%    97.5%
# Beta1  0.001325  0.00282  0.003565  0.004297  0.00551
# Beta2  0.045997  0.45696  0.669794  0.879656  1.29328
# Beta3 -0.720391 -0.55708 -0.471628 -0.387218 -0.22814
# alpha -5.876124 -4.71063 -3.984566 -3.126821 -1.82145

head(sample)
x=cbind(rep(1.0,400),gre,gpa,rank)
aux_cadenas =do.call(rbind,sample)
coeficientes =colMeans(aux_cadenas)

param_acomodados =c(coeficientes[4],coeficientes[1:3])

param_acomodados

y_hat <- drop(x%*%param_acomodados)

probas = 1/(1+exp(-y_hat))
head(probas)

matriz_jags <- ifelse(probas>=0.5,1,0)
head(matriz_jags)

table(datos$admit,matriz_jags)
(242+39)/400 #0.7025
1-(242+39)/400 #Error de clasificación 0.2975

plot(ecdf(probas))
