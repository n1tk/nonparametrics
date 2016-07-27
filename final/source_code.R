
#a
data <- read.csv("/Users/sbuciuma/Desktop/Final_NP/Pima_tr.csv") 
head(data)
#test for significant association between our variables using computed spearman correlation 
spearman.test <- cor.test(data$bmi, data$skin, method = "spearman", exact = T, conf.level = 0.90)
spearman.test

#test for significant association between our variables using computed Kendall tau 

kendall.test <- cor.test(data$bmi, data$skin, method = "kendall", exact = T, conf.level = 0.90)
kendall.test

#pearson

pearson.test <- cor.test(data$bmi, data$skin, method = "pearson", exact = T, conf.level = 0.90)
pearson.test


#OLS model
install.packages("ms")
ols <- lm(data$bmi ~ data$skin)
summary(ols)

#BCA method
beta.funcs <- function(indata,indices){
  tempdata <- indata[indices,]
  tempfit <- lm(bmi ~ skin,data=tempdata)
  tempcoef <- coef(tempfit)
  return(c(b1=tempcoef[2],b2=tempcoef[3],
           b1Minusb2=(tempcoef[2]-tempcoef[3])))  
}
library(boot)

boot.datos <- boot(data,beta.funcs,R=10000)
boot.datos 
### Check if FH has nonzero slope
boot.ci(boot.datos,index=1)
boot.ci(boot.datos)
## check if MH has nonzero slope
boot.ci(boot.datos,conf = 0.99,index=2)

### not required, but this tests if beta_FH=beta_MH because
### it evaluates to beta_FH-beta_MH
#boot.ci(boot.datos,index=3)


## 95% CI's for beta1
boot.ci(boot.datos,index=1)  

## 95% CI's for beta2
boot.ci(boot.datos,index=2)

## 95% CI's for beta1+beta2
boot.ci(boot.datos,index=3)

boot.ci(boot.datos, index=1, type="bca")
boot.ci(boot.datos,type="bca", index=2)
boot.ci(boot.datos,type="bca", index=1)

#or 

lmcoef <- function(data, i){
  d <- data[i, ]
  d.reg <- lm(y~x, d)
  c(coef(d.reg)) }
lmboot <- boot(d, lmcoef, R=999)
m1
boot.ci(lmboot, index=2)

#FOR 99% BETAS
betas <- function(indata,indices) {
  tempdata <- indata[indices,]
  tempfit <- lm(bmi ~ skin, data=tempdata)
  tempcoef <- coef(tempfit)
  return(c(b1=tempcoef[1]))
}
boots<- boot(data, betas, R=10000)
boots
boot.ci(boots,conf=0.99)
#percentile
boot.ci(boots,conf=0.99,index=1)


#percentile

bootxbars <- rep(NA,nrep)

xbar <- mean(Bmore$x)

for (i in 1:nrep){
  tempsample <- sample(Bmore$x,replace=TRUE)
  bootts[i] <- (mean(tempsample)-xbar)/(sd(tempsample)/sqrt(length(tempsample)))
  bootxbars[i] <- mean(tempsample)
}


## the boott values to use for the 98% confidence interval
tstars <- sort(bootts)[c(.99,.01)*(nrep+1)]  ### equivalent to quantile(bootts,c(.99,.01),type=6)
### get the t-pivot interval
TPivot.interval <- xbar-tstars*sd(Bmore$x)/sqrt(length(Bmore$x))
TPivot.interval

### get the 98% percentile interval
Perc.interval <- sort(bootxbars)[c(.01,.99)*(nrep+1)]
Perc.interval


#BCA method
p0 <- mean(boot.means <= mean(data))
z0 <- qnorm(p0)
zp <- qnorm(.99)

theta.i.s <- rep(NA,length(data))
for (i in 1:length(rabbits)){
  theta.i.s[i] <- mean(data[-i])    # to omit the ith value 
}
mthetai <- mean(theta.i.s)
a <- sum((mthetai-theta.i.s)^3)/(6*(sum((mthetai-theta.i.s)^2))^(3/2))

# Calculate ZL, ZU
ZL <- (z0-zp)/(1-a*(z0-zp)) + z0
ZU <- (z0+zp)/(1-a*(z0+zp)) + z0

#  percentiles lowerperc <- pnorm(ZL)
upperperc <- pnorm(ZU)
sort(boot.means)[c(lowerperc,upperperc)*(9999+1)]

# 
p0 <- mean(boot.means <= mean(data$bmi))
z0 <- qnorm(p0)
zp <- qnorm(.99)
# Calculate ZL, ZU
ZL <- (z0-zp)/(1-a*(z0-zp)) + z0
ZU <- (z0+zp)/(1-a*(z0+zp)) + z0

#  percentiles lowerperc <- pnorm(ZL)
upperperc <- pnorm(ZU)
sort(boot.means)[c(lowerperc,upperperc)*(9999+1)]



#


betas <- function(indata,indices) {
  tempdata <- indata[indices,]
  tempfit <- lm(bmi ~ skin, data=tempdata)
  tempcoef <- coef(tempfit)
  return(c(b1=tempcoef[1]))
}
boots<- boot(data, betas, R=10000)
boots
boot.ci(boots,conf=0.99)
#percentile
#boot.ci(boots,conf=0.99,index=1)



#percentile using quantile and sample with replacement
sample(c(1:200),replace=TRUE)

index <- 1:nrow(data)
beta <- rep(NA, 10000)
for (i in 1:10000){
  tempsamp <- sample(index, replace=TRUE)
  tempdata <- data[tempsamp, ]
  beta[i] <- coef(lm(bmi~skin, data=tempdata))[2]
}

quantile(beta, c(0.005,0.995))


#loess

hist(data$bmi)
plot(data$skin,data$bmi, ylim=range( c(data$skin,data$bmi)))

#local linear fit

fitlo.linear <- loess(bmi~skin, data=data)
fitlo.linear

XforPred <- seq(0,65, length=50)

#calculate the prediction and the standard errors

predInfo <- predict(fitlo.linear,XforPred,se=TRUE)
lines(XforPred, predInfo$fit)
lines(XforPred, predInfo$fit-2*predInfo$se.fit,lty=2)
lines(XforPred, predInfo$fit+2*predInfo$se.fit,lty=2)

data1 <- read.csv("/Users/sbuciuma/Desktop/Final_NP/finalQ2.csv")

data

mcnemartest <- mcnemar.test(data1)
mcnemartest

#b Chi-Square Test
chisq.test(data1)

chisq.test(data1, simulate.p.value=TRUE , B=10000)


#mcnemartest

data <- matrix(c(4, 5, 0, 7), 
               nrow = 2, 
               dimnames = list( c("stat", "not-stat"),
                                c("Introvert", "Extrovert")))

data

mcnemartest <- mcnemar.test(data)
mcnemartest

#b Chi-Square Test
chisq.test(data)

chisq.test(data, simulate.p.value=TRUE , B=10000)



#x sampling


# Compute the estimate

Xs <- read.csv("/Users/sbuciuma/Desktop/Final_NP/finalQ2.csv")
a<- c(0.01,4.99,4.11,13.52,0.33,2.47,9.7,3.03,2.42,17.39,39.12,7.87)
theta.est <- var(a)
B <- 9999
# A vector to store the residuals
residthetas <- rep(NA,B)
for (i in 1:B){
  residthetas[i] <- var(sample(a,replace=TRUE))-theta.est
}
residthetas
theta.est
# The interval
theta.est-sort(residthetas)[c(.975,.025)*(B+1)]

# Create the confidence interval
CI.using.raw.residuals <- summary(orig.fit)$coef[2,1]-sort(bootts.rawresid)[c(.975,.025)*(nrep+1)]*summary(orig.fit)$coef[2,2]
CI.using.raw.residuals

b<- c(0.01,4.99,4.11,13.52,0.33,2.47,9.7,3.03,2.42,17.39,39.12,7.87)
set.seed(97971)

boot.stds <- rep(NA,10000)

for (i in 1:10000){
  tempsample <- sample(b,replace=TRUE)
  boot.stds[i] <- var(tempsample)
}


p0 <- mean(boot.stds<=var(b))
z0 <- qnorm(p0)
zp <- qnorm(.975)

theta.i.s <- rep(NA,length(b))
for (i in 1:length(b)){
  theta.i.s[i] <- var(b[-i])    # to omit the ith value 
}
mthetai <- var(theta.i.s)
a <- sum((mthetai-theta.i.s)^3)/(6*(sum((mthetai-theta.i.s)^2))^(3/2))

# Calculate ZL, ZU
ZL <- (z0-zp)/(1-a*(z0-zp)) + z0
ZU <- (z0+zp)/(1-a*(z0+zp)) + z0

#  percentiles lowerperc <- pnorm(ZL)
upperperc <- pnorm(ZU)
lowerperc<-pnorm(ZL)
sort(boot.stds)[c(lowerperc,upperperc)*(9999+1)]
