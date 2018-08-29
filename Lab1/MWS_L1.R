# MWS LAB 1
#setwd()
# zadanie 1
######### a)
dane1.1 <- read.table("KGHM.mst", header = TRUE, sep=",")
dane1.2 <- read.table("PKOBP.mst", header = TRUE, sep=",")
dane1.3 <- read.table("ALIOR.mst", header = TRUE, sep=",")

getProcZm <- function(dane, firma){
  procZm <- matrix(0, length(dane)-1, 1)
  for (i in 1:(length(dane)-1)){
    procZm[i] <- (dane[i+1] - dane[i])/dane[i] * 100
  }
  hist(procZm, freq = FALSE, main = paste("Histogram", firma,  sep=" "), xlab = "Procentowa zmiana ceny otwarcia", ylab = "Gęstość rozkładu", breaks = 50)
  curve(dnorm(x, mean=mean(procZm), sd=sd(procZm)), add=TRUE, col = "red")
  procZm
} 

procZm1 <- getProcZm(dane1.1$X.OPEN., "KGHM") # zmiany procentowe KGHM
######### b)
procZm2 <- getProcZm(dane1.2$X.OPEN., "PKOBP")
procZm3 <- getProcZm(dane1.3$X.OPEN., "ALIOR")

# ustalenie jednej dlugosci macierzy do wykresu pudelkowego
n <- max(length(procZm1), length(procZm2), length(procZm3))
length(procZm1) = length(procZm2) = length(procZm3) <-n

procZm <- cbind(procZm1, procZm2, procZm3) # zmiany procentowe wszystkich firm

boxplot(procZm, names = c("KGHM", "PKOBP", "ALIOR")) # wykres pudelkowy
grid(NA, 32, lwd = 1) 




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
  d_temp <- dane2[substring(dane2$Date, 7, 10)==i, 11] # wypadki w i-tym roku
  
  if(length(d_temp)>0){ # gdy byly wypadki w danym roku, inaczej nie ma czego liczyc
    d[j,1] <- i; # rok
    d[j,2] <- sum(d_temp, na.rm = TRUE) # suma liczby ofiar z pominieciem wypadkow, gdzie liczba ta nie jest znana
    d[j,3] <- length(d_temp) # liczba wypadkow w roku
    j<-j+1
  } 
}
######### a)
plot(d$Year, d$Fatalities, type="l", xlab="Rok", ylab="Liczba ofiar") # liczba ofiar
plot(d$Year, d$Accidents, type="l", xlab="Rok", ylab="Liczba wypadkow") # liczba wypadkow

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
