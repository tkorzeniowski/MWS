# zadanie 1
rzut1 <- c(0,1,0,0,1,1,1,1,0,1,0,1,1,0,0,1,0,1,1,1)
rzut2 <- c(1,0,0,1,1,1,0,1,1,0,1,1,1,0,1,0,1,0,1,0)


BetaDist <- function(alfa, beta, data){ # nowy rozkład Beta
  alfa_post <- alfa + sum(data)
  beta_post <- beta + length(data) - sum(data)
  
  est_p <- alfa_post / (alfa_post + beta_post) # estymator p
  
  return(list('alfa' = alfa_post, 'beta' = beta_post, 'p' = est_p))
  
}

alfa <- 2 # a priori
beta <- 2

bDist1 <- BetaDist(alfa, beta, rzut1)
bDist2 <- BetaDist(alfa, beta, c(rzut1, rzut2))

beta_x <- seq(from=0, to=1, length=100) # oś x
beta0 <- dbeta(beta_x, shape1 = alfa, shape2 = beta) # a priori
beta1 <- dbeta(beta_x, shape1 = bDist1$alfa, shape2 = bDist1$beta) # po 20 rzutach
beta2 <- dbeta(beta_x, shape1 = bDist2$alfa, shape2 = bDist2$beta) # po 40 rzutach

plot(beta_x, beta2, type = 'l', col='blue', xlab = 'p', ylab = 'f(p)')
lines(beta_x, beta1, type='l', col='red')
lines(beta_x, beta0, type='l', col='black')
legend("topleft", legend = c("a priori","a posteriori (20 rzutów)","a posteriori (40 rzutów)"),col=c("black","red","blue"),lty=1)




# zadanie 2
n <- 20
t <- 5.1

Licznik <- function(lambda){
  ((lambda^n) * exp(-lambda*n*t)) * dgamma(lambda, shape = alfa, scale = beta) # Ln * fprior
}

f_post <- function(lambda){
  Licznik(lambda)/integrate(Licznik,lower=0, upper=Inf)$value
}

est_lambda <- function(p) {p*f_post(p)} # estymator bayesowski

lambda <- seq(from=0, to=30, length=10000)

######### a)
EX <- 0.5
VX <- 1
beta <- VX/EX
alfa <- EX/beta

f_p <- f_post(lambda)

plot(lambda, dgamma(lambda,alfa,scale = beta),type="l",xlim=c(0,1), ylim=c(0,10),xlab = "lambda", ylab="f(lambda)") # a priori
lines(lambda, f_p ,type="l", col="blue")#a posteriori
legend("topright", legend = c('a priori','a posteriori'),col=c('black','blue'),lty=1)


lambda_e <- integrate(est_lambda ,lower=0,upper=Inf)$value # estymator lambdy

######### b)
EX <- 10
VX <- 20
beta <- VX/EX
alfa <- EX/beta

f_p <- f_post(lambda)

#plot(lambda, dgamma(lambda,alfa,scale = beta),type="l",xlim = c(0, 1),ylim=c(0,10),xlab = "lambda", ylab="f(lambda)") # a priori
#lines(lambda, f_p ,type='l', col='blue')#a posteriori

plot(lambda, dgamma(lambda,alfa,scale = beta),type="l",xlab = "lambda", ylab="f(lambda)") # a priori
plot(lambda, f_p, xlim = c(0,1), type='l', col='blue', xlab = "lambda", ylab="f(lambda)") # a posteriori

lambda_e <- integrate(est_lambda ,lower=0,upper=Inf)$value # estymator lambdy




# zadanie 3

uszkodzen <- 3 # indeks 265753
dane <- matrix(0, 100, 1)
dane[1:uszkodzen] <- 1 # losowe 3 urządzenia uszkodzone, kolejność nie ma znaczenia

######### a)
alfa <- 1
beta <- 1
bDist1 <- BetaDist(alfa, beta, dane)
beta_x <- seq(from=0, to=1, length=1000) # oś x
beta01 <- dbeta(beta_x, shape1 = alfa, shape2 = beta) # a priori
######### b)
alfa <- 0.5
beta <- 5
bDist2 <- BetaDist(alfa, beta, dane)
beta02 <- dbeta(beta_x, shape1 = alfa, shape2 = beta) # a priori



beta1 <- dbeta(beta_x, shape1 = bDist1$alfa, shape2 = bDist1$beta)
beta2 <- dbeta(beta_x, shape1 = bDist2$alfa, shape2 = bDist2$beta)

plot(beta_x, beta2, type = 'l', col='blue', xlab = 'theta', ylab = 'f(teta)', xlim = c(0,0.5))
lines(beta_x, beta1, type='l', col='red', xlim=c(0, 0.7))
lines(beta_x, beta01, type='l', col='black', xlim=c(0, 0.7))
lines(beta_x, beta02, type='l', col='green', xlim=c(0, 0.7))
legend("topright", legend = c('a priori Beta(1,1)', 'a priori Beta(0.5,5)','a posteriori Beta(4,98)','a posteriori Beta(3.5,102)'),col=c('black', 'green','red','blue'),lty=1)
