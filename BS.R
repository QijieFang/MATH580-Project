##580
##Fang Qijie
##35301506

##First difine the Black-Scholes function
Black_Scholes <- function(S, ro, c, sigma, t){
 
  firstpart <- S * pnorm((log(S/c) + (ro + sigma^2 / 2)*t) / (sigma*sqrt(t)))
  secondpart <- c * exp(-ro*t)*pnorm((log(S/c) + (ro - sigma^2 / 2)*t) / (sigma*sqrt(t)))
  P <- firstpart - secondpart
  return(P)
}

par(mfrow = c(2,2))
##The original parameter are S = 1, ro = 0.03, c = 1, sigma^2 = 0.02, t = 10
P1 <- rep(0,10)
for (i in 1:10){
  P1[i] <- Black_Scholes(1, 0.03, 1, sqrt(0.02), i)
}
plot(P1, type = "l", xlab = "time", ylab = "Price with constant parameter")


P <- matrix(NA,10,20)

##Change interest rate (rho)

 for (i in 1:10){
   for (j in 1:20){
     ro <- 0.01 + (j * 0.002)
     P[i,j] <- Black_Scholes(1,ro, 1, sqrt(0.02), i)
   }
 }
 
 plot(range(c(1:10)),range(P),type = "n",xlab = "time", ylab = "Price with different interest rate")
 
 for (i in 1:dim(P)[2]){
   lines(c(1:10),P[,i], lwd = (i/10)^2) #lines get thicker with increasing rho
 }
 
##Change strike Price (c)

P <- matrix(NA,10,20)
for (i in 1:10){
  for (j in 1:20){
    c <- 0 + (j * 0.05)
    P[i,j] <- Black_Scholes(1,0.03, c, sqrt(0.02), i)
  }
}

plot(range(c(1:10)),range(P),type = "n", xlab = "time", ylab = "Price with different strike price")

for (i in 1:dim(P)[2]){
  lines(c(1:10),P[,i], lwd = (i/10)^2) #lines get thicker with increasing c
}

##Change volatility (sigma)

P <- matrix(NA,10,20)
 for (i in 1:10){
   for (j in 1:20){
     sigma <- 0 + (j * 0.01)
     P[i,j] <- Black_Scholes(1,0.03, 1, sigma, i)
   }
 }

plot(range(c(1:10)),range(P),type = "n", xlab = "time", ylab = "Price with different volatility")

 for (i in 1:dim(P)[2]){
    lines(c(1:10),P[,i], lwd = (i/10)^2) #lines get thicker with increasing sigma
  }

