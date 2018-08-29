# zadanie 1
los1 <- scan('los1.txt')
los2 <- scan('los2.txt')

# a)
mi1 <- mean(los1)
mi2 <- mean(los2)
mi <- mi1-mi2

# b)
sx2=sum((los1-mi1)^2)/(length(los1)-1)
sy2=sum((los2-mi2)^2)/(length(los2)-1)
s2 = ((length(los1)-1)*sx2 + (length(los2)-1)*sy2)/(length(los1)+length(los2)-2)

# c)
s=sqrt(s2*((1/length(los1))+(1/length(los2))))

# d) - dwustronna
# e)
T <- (mi1 - mi2) / s
alfa <- 0.1
c <- qt((1-alfa/2),length(los1)+length(los2)-2)

test95 <- t.test(los1,los2,var.equal=TRUE)


# f)
test90 <- t.test(los1,los2,var.equal=TRUE,conf.level = 0.9)
test90$p.value


# zadanie 2
lozyska <- read.table('lozyska.txt',header =TRUE, sep =',',  encoding='UTF-8')

# a)
t.test(lozyska[,1], lozyska[,2])

# b)
wilcox.test(lozyska[,1], lozyska[,2])

# c) Wilcoxona

# d)
# zakładają rozkład normalny
pstwo <- 0
n <- nrow(lozyska)
for(i in c(1:n)){
  for(j in c(1:n)){
    if(lozyska[i,1]>lozyska[j,2]){
      pstwo <- pstwo + 1
    }
  }
}
pstwo <- pstwo / (n*n)

# nie zakładając rozkładu - bootstrap nieparametryczny
i <- 1000
m <- matrix(0, i, 1)
for(i in 1:i){
  proba1 <- sample(lozyska[,1], 10000, replace = TRUE) # losowanie z kapelusza
  proba2 <- sample(lozyska[,2], 10000, replace = TRUE)
  
  m[i] <- mean(as.numeric(proba1>proba2))
}
pstwo_bootstrap <- mean(m) # prawdopodobieństwo

# zadnaie 3
# dane uzupełnione o punkt (0, 0)
v <- c(0.0, 33.3, 33.3, 49.1, 65.2, 78.5, 93.0)
y <- c(0.0, 4.7, 4.1, 10.3, 22.3, 34.4, 44.4)
  
lm1 <- lm(y ~ v - 1)
lm2 <- lm(sqrt(y) ~ v - 1)

y1 <- lm1$coefficients*v
y2 <- lm2$coefficients*v

matplot(v, y, pch="o", type="p", col="black")
matplot(v, y1, type="l", col="blue", add=TRUE)
matplot(v, y2^2, type="l", col="red", add=TRUE)
legend("topleft",1, c("dane","f(v) = y","f(v) = sqrt(y)"), col=c("black","blue","red"), pch=c("o","",""),lty=c(0,1,1))


# współczynnik determinacji
R21 <- 1-sum((y-y1)^2)/sum((y-mean(y))^2)
R22 <- 1- sum((sqrt(y)-y2)^2)/sum((sqrt(y)-mean(sqrt(y)))^2)
