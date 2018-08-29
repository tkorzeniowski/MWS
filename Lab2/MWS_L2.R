# MWS LAB 2
#setwd()
# zadanie 1
######### a)
skrety <- scan("skrety.txt")
lamb = mean(skrety) # lambda

min <- min(skrety) # minimalna liczba skrętów
max <- max(skrety) # maksymalna liczba skretów
lWystapienSkretow <- matrix(0, max+1, 1) # liczba wystąpień każdej liczby skrętów
for(i in c(min:max)){ # zamiast table(), żeby wykres miał ładną oś y
  lWystapienSkretow[i+1] <- length(which(skrety == i))
}

skretyPois <- sum(lWystapienSkretow) * dpois( c(min:max), lambda = lamb ) # spodziewane liczby skrętów w prawo

plot(c(min:max), skretyPois, type="l", xlab = "Liczba skrętów", ylab = "Liczba wystąpień", col="red")
lines(c(min:max), lWystapienSkretow)


######### b)
# bootstrap nieparametryczny
i <- 1000
m <- matrix(0, i, 1)
for(i in 1:i){
  proba <- matrix(0, length(skrety), 1) # i-ta proba losowa
  for(j in 1: length(skrety)){
    proba[j] <- sample(skrety, 1, replace = TRUE) # losowanie z kapelusza
  }
  m[i] <- mean(proba) # srednia i-tej proby
}

lamb_sd <- sd(m) # odchylenie standardowe estymatora lambdy

hist(m, freq = TRUE, main = "Histogram estymatorów lambdy", xlab = "Estymator lambda", ylab = "Częstość występowania", breaks = 100)
abline(v=lamb, col = "red", lty = 2, lwd = 2)



# zadanie 2
fotony <- scan("fotony.txt")
######### a)

h <- hist(fotony, freq = TRUE, main = "Histogram fotonów", xlab = "Odstępy między fotonami", ylab = "Częstość występowania", breaks = 200)

######### b)

# metoda momentow
m1 <- mean(fotony) # pierwszy moment
m2 <- mean(fotony^2); # drugi moment

alfa_mm <- (m1^2) / (m2 - m1^2) # estymator alfy
beta_mm <- (m2 - m1^2) / m1 # estymator bety

gamma_x <- seq(from=min(fotony), to=max(fotony), length=length(fotony)) # równe odstępy na osi x 
# wyznaczanie wartość gęstości dla każdego odstępu między fotonami (length)  * szerokość histrogramu
gamma_mm <- dgamma(gamma_x, shape = alfa_mm, scale = beta_mm) * length(fotony) * (h$mids[2]-h$mids[1])


# metoda największej wiarygodności
sredniaFotonow <- mean(log(fotony)) - log(mean(fotony)) # lewa strona równania
f <- function (x){
  digamma(x) - log(x) - sredniaFotonow # wyznaczanie estymatora alfa
}
alfa_nw <- uniroot(f, interval = c(1e-14, 2))$root # wartość estymatora 
beta_nw <- mean(fotony)/alfa_nw

gamma_nw <- dgamma(gamma_x, shape = alfa_nw, scale = beta_nw) * length(fotony) * (h$mids[2]-h$mids[1])


######### c)

hist(fotony, freq = TRUE, main = "Histogram fotonów", xlab = "Odstępy między fotonami", ylab = "Częstość występowania", breaks = 200)
lines(gamma_x, gamma_mm, type='l', col='red') # metoda momentóW
lines(gamma_x, gamma_nw, type='l', col='blue') # metoda największej wiarygodności


######### d)
#bootstrap parametryczny
i <- 1000 # liczba iteracji

mm_alfa <- matrix(0, i, 1)
mm_beta <- matrix(0, i, 1)

nw_alfa <- matrix(0, i, 1)
nw_beta <- matrix(0, i, 1)

n<- length(fotony)

for(i in 1:i){
  # metoda momentów
  gamma_mm <-  rgamma(n, shape = alfa_mm, scale = beta_mm)
  m1 = mean(gamma_mm)
  m2 = mean(gamma_mm^2)
  mm_alfa[i]= (m1^2) / (m2 - m1^2)
  mm_beta[i]= (m2 - m1^2) / m1
  
  #metoda największej wiarygodności
  gamma_nw <- rgamma(n, shape = alfa_nw, scale = beta_nw)
  sredniaFotonow <- mean(log(gamma_nw)) - log(mean(gamma_nw))
  f <- function (x){
    digamma(x) - log(x) - sredniaFotonow # wyznaczanie estymatora alfa
  }
  
  nw_alfa[i] <- uniroot(f, interval = c(1e-14, 2))$root
  nw_beta[i] <- mean(gamma_nw) / nw_alfa[i]
  
}
mm_alfa_sd <- sd(mm_alfa)
mm_beta_sd <- sd(mm_beta)
nw_alfa_sd <- sd(nw_alfa)
nw_beta_sd <- sd(nw_beta)

# przedziały ufności
pu <- function(x, alfa=0.05){
  x_sort <- sort(x)
  n <- ceiling(alfa*length(x)/2) # liczba wyników do odrzucenia
  pu <- c(x_sort[floor(n)], x_sort[floor(length(x)-n+1)])
}

mm_alfa_pu <- pu(mm_alfa)
mm_beta_pu <- pu(mm_beta)
nw_alfa_pu <- pu(nw_alfa)
nw_beta_pu <- pu(nw_beta)



# zadanie 3
normy <- scan("norm.txt")

n <- length(normy)
srednia <- mean(normy) # jednocześnie estymator mi
est_sd <- (sum((normy-srednia)^2) / (n-1)) # estymator odchylenia standardowego

gamma=c(0.9, 0.95, 0.99) # poziomy ufności
alfa <- 1 - gamma

# przedziały ufności dla średniej mi
a_mi <- matrix(0, length(gamma), 1)
b_mi <- matrix(0, length(gamma), 1)

for(i in 1:length(gamma)){ 
  a_mi[i] <- srednia - est_sd/sqrt(n) * qt(1-alfa[i]/2,n-1) # rozkład Studenta
  b_mi[i] <- srednia + est_sd/sqrt(n) * qt(1-alfa[i]/2,n-1)
}


# przedziały ufności dla odchylenia standardowego
a_sd <- matrix(0, 3, 1)
b_sd <- matrix(0, 3, 1)

for(i in 1:length(gamma)){
  a_sd[i] <- ((n-1) * est_sd^2) / qchisq(1-alfa[i]/2, n-1) # rozkład X^2
  b_sd[i] <- ((n-1) * est_sd^2) / qchisq(alfa[i]/2, n-1)
}



