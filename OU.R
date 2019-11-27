##MATH580 Project part2
##Qijie Fang
##35301506


##This is the function from workshop, by Kanchan
rOU=function(n,N,Delta,theta,sigma){
  times=(0:n)*Delta ##vector of t_0,t_1,..,t_n
  X=matrix(0,nrow=N,ncol=n+1)
  for(i in 1:n){
    x=X[,i]#current value
    m=x*exp(-theta*Delta) #mean of new value
    v=sigma^2*(1-exp(-2*theta*Delta))/(2*theta) ##variance of new value
    X[,i+1]=rnorm(N,m,sqrt(v)) ##simulate new value
  }
  return(list(X=X,times=times))
}

##Define parameters

n <- 1000
N <- 10
Delta <- 0.01
theta <- 0.5
sigma <- 0.02

#calculate X by OU-function by parameter given
X <- rOU(n,N,Delta,theta,sigma)

R0 <- 0.1
miu <- 0.05

#calculate Rt with R0, miu and X
Q0 <- matrix(NA,nrow = N, ncol = n + 1)
Rt <- matrix(NA, nrow = N, ncol = n + 1)
for(i in 1:N){
  Rt[i,] <- exp(-theta * X$times) * R0 + (1 - exp(-theta * X$times)) * miu + X$X[i,]
}



#initiate Plot
plot(range(X$times),range(Rt), type = "n", xlab = "time", ylab = "Rt")

#plot the lines for Rt 
for(i in 1:N){
  lines(X$times,Rt[i,],col = i)
}




par(mfrow = c(1,2))
#Plot the means and variances
plot(X$times,colMeans(Rt), type = "l", xlab = "time", ylab = "mean")
plot(X$times, apply(Rt,2,var), type = "l",col = "red", xlab = "time", ylab = "variance")



par(mfrow = c(1,3))
##plot Q0
for (i in 1:N){
  Q0[i,] <- exp(-Delta * cumsum(Rt[i,]))
}
#initiate Plot for Q0
plot(range(X$times),range(Q0), type = "n", xlab = "time", ylab = "Bond price")
for(i in 1:N){
  lines(X$times,Q0[i,],col = i)
}


## find distribution of Q
N <- 1000
t = 5
Delta <- 0.01
n <- t/Delta
X <- rOU(n, N, Delta, theta, sigma)
R0 <- 0.1
miu <- 0.05
Rt <- matrix(NA, nrow = N, ncol = n + 1)
for(i in 1:N){
  Rt[i,] <- exp(-theta * X$times) * R0 + (1 - exp(-theta * X$times)) * miu + X$X[i,]
}
Q <- rep(NA,N)
for(i in 1:N){
  Q[i] <- exp(-Delta * cumsum(Rt[i,2:n+1]))
}

##histogram of Q and log(Q)
hist(Q[1:N], breaks = 50, ylab = "Frequency of bond price")
hist(log(Q[1:N]), breaks = 50)




