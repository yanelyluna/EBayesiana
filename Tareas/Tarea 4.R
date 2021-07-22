########## TAREA 4 #######################
## EJERCICIO 1
###### Librerías #########
#install.packages("mcsm")
library(mcsm)
library(VGAM)
library(ggplot2)
library(gridExtra)
library(dplyr)
##### Datos ####
data("challenger")
str(challenger)
# Modelo de regresión logística
modelo_reglog <- glm(oring~temp,family = "binomial",data = challenger)
summary(modelo_reglog)
# Estimadores máximo verosímiles de alpha y beta
alpha_mle <- as.numeric(coefficients(modelo_reglog)[1]) #15.0429
beta_mle <- as.numeric(coefficients(modelo_reglog)[2]) #-0.2321627

#### Algoritmo de Metropolis-Hastings ######
# Distribución objetivo
f <- function(alpha,beta){
  x <- challenger$temp
  y <- challenger$oring
  
  p <- exp(alpha+x*beta)/(1+exp(alpha+x*beta))
  
  return(prod((p^y)*(1-p)^(1-y)))
}

# Algoritmo
N_sim <- 5000 # número de simulaciones
set.seed(139)
# Valores iniciales
a <- rep(rexp(1,1/alpha_mle),N_sim) #candidata para alpha
b <- rep(rlaplace(1,beta_mle,0.01), N_sim) #candidata para beta

for(i in 2:N_sim){
  # candidatas
  a_c <- rexp(1,1/alpha_mle)
  b_c <- rlaplace(1,location=beta_mle,scale = 0.01)
  # rho
  rho <- min(f(a_c,b_c)*dexp(a[i-1],1/alpha_mle)*dlaplace(b[i-1],beta_mle,scale = 0.01)/
    (f(a[i-1],b[i-1])*dexp(a_c,1/alpha_mle)*dlaplace(b_c,beta_mle,scale = 0.01)),1)
  # decisión
  a[i] <- a[i-1] + (a_c - a[i-1])*(runif(1)<rho)
  b[i] <- b[i-1] + (b_c - b[i-1])*(runif(1)<rho)
  
}
# Revisar convergencia
par(mfrow=c(1,2))
plot(a,type="l",col="blue",main="Simulaciones para alpha")
plot(b, type="l",col="green",main="Simulaciones para beta")

data.frame(est_mv = c(alpha_mle,beta_mle),est_MH = c(mean(a),mean(b)))

# Histogramas
g1 <- ggplot(data.frame(alpha=a,beta=b),aes(x=alpha)) +
  geom_histogram(bins = 15,fill="lightblue") +
  theme_light()
g2 <- ggplot(data.frame(alpha=a,beta=b),aes(x=beta)) +
  geom_histogram(bins = 15,fill="lightgreen") +
  theme_light()
grid.arrange(g1,g2,ncol=2)
##### Cálculo de p(i) usando las simulaciones #######
x <- challenger$temp
y <- challenger$oring
#p <- matrix(NA,nrow = N_sim,ncol = length(x))
p <- data.frame()
for (i in 1:N_sim) {
  p[i,1:length(x)] <- exp(a[i] + b[i]*x)/(1+exp(a[i] + b[i]*x))
}
# Gráfica del ajuste del modelo de regresión logística (rojo)
#  y algunas aproximaciones de la simulación
plot(x=x,y=y)
lines(x= x, y=modelo_reglog$fitted.values,col="red")
lines(x=x,y=p[700,],col="blue")
lines(x=x,y=p[800,],col="pink")
lines(x=x,y=p[300,],col="violet")

# 200 valores diferentes 
ab_dist <- distinct(data.frame(a=a,b=b))
p_dist <- c()
for (i in 1:length(ab_dist$a)) {
  p_dist[(23*i-22):(i*23)] <- exp(ab_dist$a[i] + ab_dist$b[i]*x)/
                            (1+exp(ab_dist$a[i] + ab_dist$b[i]*x))
}

p_graph <- data.frame(x=rep(x,200),p=p_dist,group=rep(1:200,each=23))

####### Gráfica #######

ggplot(p_graph,aes(x=x,y=p,group=group)) + geom_line(color="yellow") + theme_bw() +
  geom_line(aes(x=x,y=rep(modelo_reglog$fitted.values,200),group=1),color="purple",size=2) +
  geom_point(aes(x=x,y=rep(challenger$oring,200),group=1),color="blue")
  
##### Estimaciones de p(i) ######
p_est <- matrix(NA,nrow = N_sim,ncol = 3)
for (i in 1:N_sim) {
  p_est[i,1] <- exp(a[i] + b[i]*60)/(1+exp(a[i] + b[i]*60))
  p_est[i,2] <- exp(a[i] + b[i]*50)/(1+exp(a[i] + b[i]*50))
  p_est[i,3] <- exp(a[i] + b[i]*40)/(1+exp(a[i] + b[i]*40))
}

estimaciones <- data.frame(temp = c(60,50,40),
                           GLM = sapply(c(60,50,40),FUN=function(t) {predict(modelo_reglog,data.frame(temp=t),type = "response")}),
                           M_H = c(mean(p_est[,1]),mean(p_est[,2]),mean(p_est[,3])),
                           std_err = c(sd(p_est[,1]),sd(p_est[,2]),sd(p_est[,3])))
estimaciones



########### Usando JAGS #######
p = modelo_reglog$fitted.values   ### logistic
library(rjags)
n=length(x)
data <- list(
  y = y ,
  x = x,
  n = n 
)

param <- c("Beta")


### Logit

inits <- function(){	list(
  "Beta" = rnorm(2,0,0.1) 
)	}

fit <- jags.model("Tareas/RegLog_E1T4.bug", data, inits,  n.chains=3)

update(fit,2000)

sample <- coda.samples(fit, param, n.iter=2000, thin=1)

plot(sample)
summary(sample)

# Iterations = 3001:5000
# Thinning interval = 1 
# Number of chains = 3 
# Sample size per chain = 2000 
# 
# 1. Empirical mean and standard deviation for each variable,
# plus standard error of the mean:
#   
#   Mean     SD Naive SE Time-series SE
# Beta[1] -0.2313 0.1152 0.001488        0.03531
# Beta[2] 14.9092 7.8454 0.101283        2.40186
# 
# 2. Quantiles for each variable:
#   
#   2.5%     25%     50%     75%    97.5%
# Beta[1] -0.4682 -0.3158 -0.2022 -0.1405 -0.06443
# Beta[2]  3.5103  8.7777 12.9498 20.6592 31.07928
