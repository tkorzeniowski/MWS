setwd("D:/Studia/Mgr_1/MWS/Laboratorium/L1/notowania")
# zadanie 1
dane1.1 <- read.table("JSW.mst", header = TRUE, sep=",")
dane1.2 <- read.table("CCC.mst", header = TRUE, sep=",")
dane1.3 <- read.table("ALIOR.mst", header = TRUE, sep=",")

procZm1 <- matrix(0, nrow(dane1.1)-1, 1)
for (i in 1:(nrow(dane1.1)-1)){
  procZm1[i] <- dane1.1$X.OPEN.[i+1]/dane1.1$X.OPEN.[i] - 1
}
hist(procZm1, freq = FALSE, main = "Histogram firmy", xlab = "Procentowa zmiana cen otwarcia", ylab = "Gęstość rozkładu")
curve(dnorm(x, mean=mean(procZm1), sd=sd(procZm1)), add=TRUE, col = "red")


procZm2 <- matrix(0, nrow(dane1.2)-1, 1)
for (i in 1:(nrow(dane1.2)-1)){
  procZm2[i] <- dane1.2$X.OPEN.[i+1]/dane1.2$X.OPEN.[i] - 1
}
hist(procZm2, freq = FALSE, main = "Histogram firmy", xlab = "Procentowa zmiana cen otwarcia", ylab = "Gęstość rozkładu")
curve(dnorm(x, mean=mean(procZm2), sd=sd(procZm2)), add=TRUE, col = "red")


procZm3 <- matrix(0, nrow(dane1.3)-1, 1)
for (i in 1:(nrow(dane1.3)-1)){
  procZm3[i] <- dane1.3$X.OPEN.[i+1]/dane1.3$X.OPEN.[i] - 1
}
hist(procZm3, freq = FALSE, main = "Histogram firmy", xlab = "Procentowa zmiana cen otwarcia", ylab = "Gęstość rozkładu")
curve(dnorm(x, mean=mean(procZm3), sd=sd(procZm3)), add=TRUE, col = "red")



n <- max(length(procZm1), length(procZm2), length(procZm3))
length(procZm1) = length(procZm2) = length(procZm3) <-n

procZm <- cbind(procZm1, procZm2, procZm3)

boxplot(procZm, names = c("JWS", "CCC", "ALIOR"))
grid(NA, 16, lwd = 1) 



setwd("D:/Studia/Mgr_1/MWS/Laboratorium/L1")
# zadanie 2
dane2 <- read.csv("katastrofy.csv")
# znajdowanie zakresu lat
d <- substring(dane2$Date, 7, 10)
start<-min(d) # rok poczatkowy
stop<-max(d) # rok koncowy

# zbieranie statystyk
d<-data.frame(Year= numeric(0), Fatalities= integer(0), Accidents= integer(0))
j<-1
for(i in c(start:stop)){
  d_temp <- dane2[substring(dane2$Date, 7, 10)==i, 11]
  d[j,1] <- i; # rok
  d[j,2] <- sum(d_temp) # suma liczby ofiar
  if(!is.finite(d[j,2])){ # gdy suma == NA
    d[j,2]<-0
  }
  if(is.null(length(d_temp))){ # gdy nie ma wypadkow
    d[j,3]<-0
  } else{
    d[j,3] <- length(d_temp) # liczba wypadkow
  }
  j<-j+1
}
######### a)
plot(d$Year, d$Fatalities, type="l") # liczba ofiar
plot(d$Year, d$Accidents, type="l") # liczba wypadkow

######### b)
# przyblizanie funkcji wielomianem 3-go stopnia
fit <- lm(d$Fatalities ~ poly(d$Year,3,raw=TRUE)) # liczba ofiar
xx <- seq(as.integer(start),as.integer(stop), length=nrow(d))
plot(d$Year, d$Fatalities, type="l", xlab="Rok", ylab="Liczba ofiar")
lines(xx, predict(fit, data.frame(x=xx)), col="red")

fit <- lm(d$Accidents ~ poly(d$Year,3,raw=TRUE)) # liczba wypadkow
xx <- seq(as.integer(start),as.integer(stop), length=nrow(d))
plot(d$Year, d$Accidents, type="l", xlab="Rok", ylab="Liczba wypadkow")
lines(xx, predict(fit, data.frame(x=xx)), col="red")


#https://davetang.org/muse/2013/05/09/on-curve-fitting/