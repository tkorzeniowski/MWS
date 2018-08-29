# zadanie 1
samobojstw <- c(1867, 1789, 1944, 2094, 2097, 1981, 1887, 2024, 1928, 2032, 1978, 1859)
dni <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

samobojstwDziennie <- sum(samobojstw)/sum(dni) # średnia liczba samobójstw każdego dnia roku
samobojstwMiesiecznie <- as.matrix(samobojstwDziennie*dni) # średnia liczba samobójstw w przeliczeniu na miesiące

plot(1:12, samobojstw, xlab = 'Miesiąc',  ylab = 'Liczba samobójstw')
segments(1:12, 0, 1:12, samobojstw)

# określanie sezonowości samobójstw
sam <- samobojstw/dni # średnia liczba samobójstw przypadająca na każdy dzień w danym miesiącu
plot(1:12, sam, xlab = 'Miesiąc',  ylab = 'Średnia liczba samobójstw dziennie')
segments(1:12, 0, 1:12, sam)
abline(samobojstwDziennie, 0, col='red')

r <- 12
T <- 0
for(i in 1:r) {
  T <- T + (samobojstw[i]-samobojstwMiesiecznie[i])^2/(samobojstwMiesiecznie[i]);
}

alfa <- 0.05
c <- qchisq(1 - alfa, r - 1) # funkcja kwantylowa

# automatyczny test chi^2
chisq.test(samobojstw, p = (dni/sum(dni)))




# zadanie 2
# a)
dane <- read.table('tempciala.txt',header =TRUE, sep =',',  encoding='UTF-8')
colnames(dane) <- c('temperatura', 'plec', 'tetno')

m <- dane[which(dane[,2]=='1'),-2]
k <- dane[which(dane[,2]=='2'),-2]

sredniaTempM <- mean(m$temperatura) # estymator n.w. średniej dla mężczyzn
warTempM <- sd(m$temperatura)^2
sredniaTempK <- mean(k$temperatura) # estymator n.w. średniej dla kobiet
warTempK <- sd(k$temperatura)^2

est_warTempM <- (sum((m$temperatura - sredniaTempM)^2) / (nrow(m)-1)) # estymator n.w. wariancji dla mężczyzn
est_warTempK <- (sum((k$temperatura - sredniaTempK)^2) / (nrow(k)-1)) # estymator n.w. wariancji dla kobiet

# ogólny wygląd wykresu kwantyl-kwantyl
rozk <- rnorm(1000, 0, 1)
qqnorm(rozk)
qqline(rozk, col='red')
# wykresy kwantyl-kwantyl dla danych
kwantyM <- quantile(m$temperatura, probs = seq(0,1,0.015))
qqplot(kwantyM, qnorm(seq(0,1,0.015), mean = sredniaTempM, sd=sqrt(est_warTempM)), xlab='Kwantyle z próby', ylab = 'Kwantyle z estymowanego rozkładu normalnego')
abline(0,1, col='red')

kwantyK <- quantile(k$temperatura, probs = seq(0,1,0.015))
qqplot(kwantyK, qnorm(seq(0,1,0.015), mean = sredniaTempK, sd=sqrt(est_warTempK)), xlab='Kwantyle z próby', ylab = 'Kwantyle z estymowanego rozkładu normalnego')
abline(0,1, col='red')


# b)
# test t-Studenta
alfa <- 0.05
# mężczyźni
TS_M <- (sredniaTempM - mi0)/sqrt(warTempM)*sqrt(nrow(m))
c_M = qnorm(1 - alfa/2)

# kobiety
TS_K <- (sredniaTempK - mi0)/sqrt(warTempK)*sqrt(nrow(k))
c_K = qnorm(1 - alfa/2)

# automatyczny test t-Studenta
t.test(m$temperatura, mu=36.6)
t.test(k$temperatura, mu=36.6) 