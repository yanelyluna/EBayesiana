##Ejercicio 7.5 Casella
#Dada una normal objetivo Np(0,(1-rho)*I+rho*I)
#### Inciso a) #### 
#Elabora un muestreo de Gibbs usando las 
#distribuciones condicionales dadas en el 
#ejemplo 7.4. Corre el código de R para p=5 
#y rho=0.25, y verifica gráficamente que 
#todas las marginales son N(0,1). 

n<-5000
inits<-c(0.5,0.5,0.5,0.5,0.5) 
p<-5 
rho<-0.25
x<-array(0,dim=c(p,n)) 
x[1:5,1]<-inits 
r<-((p-1)*rho)/(1+(p-2)*rho) 
v<-(1+(p-2)*rho-(p-1)*rho^2)/(1+(p-2)*rho) 
#pbar <- create_progress_bar('text')
#pbar$init(n)
for(i in 2:n){ 
  x[1,i]<-rnorm(1,r*mean(c(x[2,i-1],x[3,i-1],x[4,i-1],x[5,i-1])),sqrt(v)) 
  x[2,i]<-rnorm(1,r*mean(c(x[1,i],x[3,i-1],x[4,i-1],x[5,i-1])),sqrt(v)) 
  x[3,i]<-rnorm(1,r*mean(c(x[1,i],x[2,i],x[4,i-1],x[5,i-1])),sqrt(v)) 
  x[4,i]<-rnorm(1,r*mean(c(x[1,i],x[2,i],x[3,i],x[5,i-1])),sqrt(v)) 
  x[5,i]<-rnorm(1,r*mean(c(x[1,i],x[2,i],x[3,i],x[4,i])),sqrt(v)) 
  #pbar$step()
  }
par(mfrow=c(1,1))
plot(x[1,],type="l",main="Simulación de la cadena 1",col.main="purple",col="violet",xlab="iteraciones",ylab="")
plot(x[2,],type="l",main="Simulación de la cadena 2",col.main="purple",col="plum3",xlab="iteraciones",ylab="")
plot(x[3,],type="l",main="Simulación de la cadena 3",col.main="purple",col="slategrey",xlab="iteraciones",ylab="")
plot(x[4,],type="l",main="Simulación de la cadena 4",col.main="purple",col="mediumturquoise",xlab="iteraciones",ylab="")
plot(x[5,],type="l",main="Simulación de la cadena 5",col.main="purple",col="mediumorchid",xlab="iteraciones",ylab="")
##Ahora veamos los histogramas
aux<-rnorm(50000,0,1)
par(mfrow=c(2,3))
hist(x[1,],main="Densidad  1",col="violet",col.main="purple",freq=FALSE, border = "white", ylab = "", xlab="")
lines(density(aux), col="blue",lty = 2, lwd=3)
hist(x[2,],main="Densidad  2",col="green",col.main="purple",freq=FALSE, border = "white", ylab = "", xlab="")
lines(density(aux), col="blue",lty = 2, lwd=3)
hist(x[3,],main="Densidad  3",col="pink",col.main="purple",freq=FALSE, border = "white", ylab = "", xlab="")
lines(density(aux), col="blue",lty = 2, lwd=3)
hist(x[4,],main="Densidad  4",col="yellow",col.main="purple",freq=FALSE, border = "white", ylab = "", xlab="")
lines(density(aux), col="blue",lty = 2, lwd=3)
hist(x[5,],main="Densidad  5",col="orange",col.main="purple",freq=FALSE, border = "white", ylab = "", xlab="")
lines(density(aux), col="blue",lty = 2, lwd=3)
hist(aux,main="N(0,1)",col="salmon",col.main="purple",freq=FALSE, border = "white", ylab = "", xlab="")
lines(density(aux), col="blue",lty = 2, lwd=3)

par(mfrow=c(1,1))
#### Inciso b) #### 
#Compara el algoritmo usando T=500 iteraciones
#con rmnorm en términos de ejecución. 
library(MASS) 
par(mfrow=c(1,1))
mu<-c(0,0,0,0,0) 
sigma<-matrix(c(1,0.25,0.25,0.25,0.25, 0.25,1,0.25,0.25,0.25, 0.25,0.25,1,0.25,0.25, 0.25,0.25,0.25,1,0.25, 0.25,0.25,0.25,0.25,1),nrow=5,ncol=5) 
z<-mvrnorm(500,mu,sigma)
hist(z,main="MASS",col="salmon",col.main="purple",freq=FALSE, border = "white", ylab = "", xlab="")
lines(density(aux), col="blue",lty = 2, lwd=3)
