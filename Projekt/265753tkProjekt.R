# Tomasz Korzeniowski, 265753
#setwd()
#poprawki MR
# znajdowanie punktow zmian
getChangepoints <- function(data, alfa=0.01){
  if(nrow(data)<ncol(data)){ # kazda kolumna odpowiada wymiarowi danych
    data<-t(data)
  }
  
  N <- nrow(data) # liczba obserwacji
  K <- ncol(data) # liczba wymiarow
  R <- matrix(0, N, K) # znalezione punkty zmian
  R[c(1,N), c(1:K)] <- 1
  
  P <- matrix(0, N, K) # p-wartosci dla kazdego testowanego podzialu
  
  dlug_okna = 20;
  for (i in 2:(N-1)) {  # losowy punkt 
    if((i!=1) & (i!=N) ){
      
      for (j in 1:K){ # kazdy wymiar niezaleznie
        
        #i_ <- i-1 # indeks poprzedniego punktu zmiany
        #ip <- i+1 # indeks nastepnego punktu zmiany
        
        f <- which(R[,j] >0) # znajdowanie podzialu na probki
        i_ <- f[max(which(f < i))]
        ip <- f[min(which(f > i))]
        
        if (ip > i + dlug_okna) ip <- i + dlug_okna;
        #if (i_ < i - dlug_okna) i_ <- i - dlug_okna;
        i_ <- i - dlug_okna;
        
        if(i_<1) i_<-1
        if(ip>N) ip<-N
        
        s1 <- data[(i_+1):i, j] # podziel probki
        s2 <- data[(i+1):ip, j]
        
        # test Wilcoxona
        wt<-wilcox.test(as.matrix(s1), as.matrix(s2), exact = FALSE, correct = FALSE)
        if(!is.finite(wt$p.value)){ 
          P[i,j] <- pwilcox(wt$statistic, ncol(as.matrix(s1)), ncol(as.matrix(s2)))
        }else{
          P[i,j] <- wt$p.value
        }
        
        if(P[i,j]<=alfa){  # odrzucamy hipoteze zerowa
          R[i,j]<-1
        }else{
          R[i,j]<-0 # nie ma podstaw do odrzucenia H0
        }
        
      } 
      
    }
    
  }
  R
}



# wyniki dla danych wygenerowanych
N <- 200
X <- matrix(0, 200, 1)

for(i in 1:50){
  X[i]<-10 +((-1)^i)*runif(1, .5, 1.5)
  X[50+i]<-30 +((-1)^i)*runif(1, 1.0, 3.0)
  X[100+i]<-10 +((-1)^i)*runif(1, 1.0, 2.0)
  X[150+i]<-50 +((-1)^i)*runif(1, 1.0, 5.0)
}

r <- getChangepoints(X)

plot(c(1:N), X, type = 'l', xlab='czas', ylab='mierzona wielkość')
abline(v=c(which(r==1)), col="red", lty=3, lwd=1)


# wyniki dla danych rzeczywistych
data <- read.table('dane.txt')
colnames(data) <- c('odleglosc', 'jasnosc')

# przykladowy przebieg danych
plot(c(1:1001), data[6900:7900,1], type='l', xlab='czas', ylab='odległość')
plot(c(1:1001), data[6900:7900,2], type='l', xlab='czas', ylab='jasność')

data<-data[1:1000, ] # demonstracja wynikow na mniejszym przedziale

R <- getChangepoints(data) # wyniki


N <- nrow(data) # liczba obserwacji
plot(c(1:N), data[, 1], type = 'l', xlab='czas', ylab='odległość')
abline(v=c(which(R[,1]==1)), col="red", lty=3, lwd=1)

plot(c(1:N), data[,2], type = 'l', xlab='czas', ylab='jasność')
abline(v=c(which(R[,2]==1)), col="red", lty=3, lwd=1)
