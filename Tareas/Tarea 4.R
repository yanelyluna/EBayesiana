########## TAREA 4 #######################
## EJERCICIO 1
#install.packages("mcsm")
library(mcsm)
library(VGAM)
library(ggplot2)
library(gridExtra)
library(dplyr)
# Datos
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
# Candidatas


# Algoritmo
N_sim <- 5000 # número de simulaciones
# Valores iniciales
a <- rep(rexp(1,1/alpha_mle),N_sim)
b <- rep(rlaplace(1,beta_mle,0.01), N_sim)

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
plot(a,type="l",col="lightblu")
plot(b, type="l",col="lightgreen")

# Histogramas
g1 <- ggplot(data.frame(alpha=a,beta=b),aes(x=alpha)) +
  geom_histogram(bins = 15,fill="lightblue") +
  theme_light()
g2 <- ggplot(data.frame(alpha=a,beta=b),aes(x=beta)) +
  geom_histogram(bins = 15,fill="lightgreen") +
  theme_light()
grid.arrange(g1,g2,ncol=2)
# Cálculo de p(i) usando las simulaciones 
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

# 197 valores diferentes 
p_dist <- distinct(data.frame(a=a,b=b))

g3 <- ggplot(data.frame(x=x,y=y), aes(x=x,y=y)) + geom_point(color="blue") +
  geom_line(aes(x=x,y=modelo_reglog$fitted.values),color="red") 
for (i in 1:length(p_dist$V1)) {
  g3 <- g3 + geom_line(aes(x=x,y=p_dist[i,]),color="pink")
}

g3 + theme_bw()


p_est <- matrix(NA,nrow = N_sim,ncol = 3)
for (i in 1:N_sim) {
  p_est[i,1] <- exp(a[i] + b[i]*60)/(1+exp(a[i] + b[i]*60))
  p_est[i,2] <- exp(a[i] + b[i]*50)/(1+exp(a[i] + b[i]*50))
  p_est[i,3] <- exp(a[i] + b[i]*40)/(1+exp(a[i] + b[i]*40))
}
mean(p_est[,1])
mean(p_est[,2])
mean(p_est[,3])
predict(modelo_reglog,data.frame(temp=60),type = "response")
predict(modelo_reglog,data.frame(temp=50),type = "response")
predict(modelo_reglog,data.frame(temp=30),type = "response")
