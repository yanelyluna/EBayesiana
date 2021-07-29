#EJERCICIO 6.9


#### Simular la media de una Gamma(4.2,6.3) usando una candidata Gamma(4,7) #####
N_sim <- 1000

# Simular un valor de la candidata
x <- rgamma(N_sim,4,7)

# auxiliar media
prom <- x

#Algoritmo Matropolis-Hastings

for (i in 2:N_sim) {
  y <- rgamma(1,4,7) #candidata
  alpha <- min(dgamma(y,4.3,6.2)*dgamma(x[i-1],4,7)/
                 (dgamma(y,4,7)*dgamma(x[i-1],4.3,6.2)),1)
  x[i] <- x[i-1] + (y-x[i-1])*(runif(1)<alpha)
  prom[i] <- mean(x[1:i])
  
}
prom
# Revisar la convergencia
plot(x, type="l",col="blue")
plot(prom,type="l")
# Valor real
4.3/6.2
# Valor estimado con la simulación
mean(prom)
hist(x)

### Simular la media de una Gamma(4.2,6.3) usando una candidata Gamma(5,6) ####
N_sim <- 1000

# Simular un valor de la candidata
x <- rgamma(N_sim,5,6)

# auxiliar media
prom <- x

#Algoritmo Metropolis-Hastings

for (i in 2:N_sim) {
  y <- rgamma(1,5,6) #candidata
  alpha <- min(dgamma(y,4.3,6.2)*dgamma(x[i-1],5,6)/
                 (dgamma(y,5,6)*dgamma(x[i-1],4.3,6.2)),1)
  x[i] <- x[i-1] + (y-x[i-1])*(runif(1)<alpha)
  prom[i] <- mean(x[1:i])
  
}
prom
# Revisar la convergencia
plot(x, type="l",col="blue")
plot(prom,type="l")
# Valor real
4.3/6.2
# Valor estimado con la simulación
mean(prom)
hist(x)
