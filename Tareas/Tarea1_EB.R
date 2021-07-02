######### ESTADíSTICA BAYESIANA ########
#------------ TAREA 1 -------------
set.seed(2)
library(ggplot2)
library(tidyr)
# X_i ~ Poisson(theta)
n <- 15
theta <- 7
X <- rpois(n,theta)

#----PRIORI: Gamma(alpha0,beta0)----
# Hiperparámetros
alpha0 <- 25
beta0 <- 4

#-----POSTERIOR: Gamma(alpha1, beta1)----
alpha1 <- alpha0 + sum(X)
beta1 <- beta0 + n

#Valores de theta para graficar.
theta.seq <- seq(0,max(X)+sd(X),length.out=100)
#----Gráficas----
plot(theta.seq,dgamma(theta.seq,alpha1,beta1),xlab="Theta",ylab="Densidad",
     main="Distribución posterior",type="l",col="darkgreen",lwd=2) #posterior
lines(theta.seq,dgamma(theta.seq,sum(X)+1,n),col="orange",lty=4,lwd=2) #verosimilitud
lines(theta.seq,dgamma(theta.seq,alpha0,beta0),col="blue",lty=1,lwd=2) #prior
legend("topright", legend=c("Prior","Verosimilitud","Posterior"), lty=c(2,4,1), col=c("blue","orange","darkgreen"), lwd=2)
#------ con ggplot2 ----
pr_pos <- data.frame(theta = seq(0,max(X)+sd(X),length.out=100),
                     vero = dgamma(theta.seq,sum(X)+1,n),
                     prior = dgamma(theta.seq,alpha0,beta0),
                     posterior = dgamma(theta.seq,alpha1,beta1))
pr_pos_gather <- pr_pos %>% gather(key="densidad",value="valor",-theta)
ggplot(pr_pos_gather, aes(x=theta,y=valor,color=densidad)) +
  geom_line() +
  ggtitle("Densidad") +
  theme_bw()
#-----------------
#----PREDICTIVA INICIAL----
r0 <- alpha0 
p0 <- beta0/(beta0+1) 

#----PREDICTIVA FINAL----
r1 <- alpha1 #alpha0 + sum(X)
p1 <- beta1/(beta1+1) #(beta0 + n)/(beta0 + n + 1)

#----Gráficas----
z <- seq(0,max(X)+sd(X),by=1)

plot(z,dnbinom(z, r0, p0), main="Distribución predictiva",ylab="Densidad",lwd=2,col="blue") #predictiva inical
points(z, dnbinom(z,r1,p1), lwd=2, col="darkgreen",type="h")   #predictiva final
lines(z,dpois(z,theta),col="orange",lty=4,lwd=2) #modelo paramétrico
legend("topright", legend=c("Predictiva Prior","Predictiva Posterior","Modelo parametrico"), lty=c(1,3,4), col=c("blue","black","orange"),cex=0.8)  
