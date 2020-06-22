
# load oceanic whitetip acceleration data (log(ODBA) values)
lODBA<-read.csv("http://www.wiwi.uni-bielefeld.de/lehrbereiche/statoekoinf/stat/Lehre/shark.csv")$x
length(lODBA)
par(mfrow=c(1,2))
hist(lODBA,breaks=100)
plot(lODBA[1:5000],type="l")

# function that calculates minus the log-likelihood of an N-state HMM with Gaussian state-dep. distributions
mllk<-function(theta.star,x,N){
  Gamma <- diag(N)
  Gamma[!Gamma] <- exp(theta.star[1:((N-1)*N)])
  Gamma <- Gamma/rowSums(Gamma)
  delta <- solve(t(diag(N)-Gamma+1),rep(1,N))
  mu <- theta.star[(N-1)*N+1:N]
  sigma <- exp(theta.star[(N-1)*N+(N+1):(2*N)])
  allprobs <- matrix(1,length(x),N)
  ind <- which(!is.na(x))
  for (j in 1:N){
    allprobs[ind,j] <- dnorm(x[ind],mu[j],sigma[j])
  }
  foo <- delta%*%diag(allprobs[1,])
  l <- log(sum(foo))
  phi <- foo/sum(foo)
  for (t in 2:length(x)){
    foo <- phi%*%Gamma%*%diag(allprobs[t,])
    l <- l+log(sum(foo))
    phi <- foo/sum(foo)
  }
  return(-l)
}

# fit the model to the acceleration data
N=3
theta.star <- c(c(-2,-2,-2,-2,-2,-2),c(-4.2,-3.3,-2.5),log(c(2,2,2)))
mod3<-nlm(mllk,theta.star,x=lODBA,N=N,print.level=2)
theta.star<-mod3$estimate
Gamma <- diag(N)
Gamma[!Gamma] <- exp(theta.star[1:((N-1)*N)])
Gamma <- Gamma/rowSums(Gamma)
delta <- solve(t(diag(N)-Gamma+1),rep(1,N))
mu <- theta.star[(N-1)*N+1:N]
sigma <- exp(theta.star[(N-1)*N+(N+1):(2*N)])
Gamma
delta
mu
sigma

# plot fitted model
hist(lODBA,breaks=300,prob=TRUE)
curve(delta[1]*dnorm(x,mu[1],sigma[1]),from=-5,to=-1,add=TRUE,col=cb[1],lwd=3)
curve(delta[2]*dnorm(x,mu[2],sigma[2]),from=-5,to=-1,add=TRUE,col=cb[2],lwd=3)
curve(delta[3]*dnorm(x,mu[3],sigma[3]),from=-5,to=-1,add=TRUE,col=cb[3],lwd=3)
curve(delta[1]*dnorm(x,mu[1],sigma[1])+
      delta[2]*dnorm(x,mu[2],sigma[2])+
      delta[3]*dnorm(x,mu[3],sigma[3]),from=-5,to=-1,add=TRUE,col=1,lwd=3,lty=2)

# decode the hidden states under the fitted model
viterbi<-function(x,mu,sigma,gamma,delta){
  n <- length(x)
  allprobs <- matrix(1,n,N)
  ind <- which(!is.na(x))
  for (j in 1:N){
    allprobs[ind,j] <- dnorm(x[ind],mu[j],sigma[j])
  }
  xi <- matrix(0,n,N)
  foo <- delta*allprobs[1,]
  xi[1,] <- foo/sum(foo)
  for (t in 2:n){
    foo <- apply(xi[t-1,]*gamma,2,max)*allprobs[t,]
    xi[t,] <- foo/sum(foo)
  }
  iv <- numeric(n)
  iv[n] <- which.max(xi[n,])
  for (t in (n-1):1){
    iv[t] <- which.max(gamma[,iv[t+1]]*xi[t,])
  }
  iv
}

vit<-viterbi(lODBA,mu,sigma,Gamma,delta)
plot(lODBA[1:5000],pch=19,col=cb[vit])

