## Birthwt examples: we'll look for differences in the distribution
## of the child's birth weight (bwt) based on whether or not the mother
## smokes (smoke=1) or does not (smoke=0)

library(MASS)
library(perm)
attach(birthwt)

wilcox.test(bwt[smoke==1], bwt[smoke==0])
table(smoke)
wilcox.test(bwt[smoke==1], bwt[smoke==0])$statistic + 74*(75)/2




#### RMD.test  is a function to implement the test in Higgins, p. 55
### and is modeled after the code in Figure 2.8.2

RMD.test <- function(samp1,samp2,direction=c('two.sided','less','greater')[1],nsamp=10000){
  devs1 <- samp1-median(samp1)
  devs2 <- samp2-median(samp2)
  devs <- c(devs1,devs2)
  RMD <- mean(abs(devs1))/mean(abs(devs2))
  if (direction[1]=='two.sided'){
    RMD <- max(1/RMD, RMD)
  }
  RMDperms <- rep(NA,nsamp)
  for (i in 1:nsamp){
    tempdevs <- devs[sample(length(devs),length(devs),replace=FALSE)]

    RMDperms[i] <- mean(abs(tempdevs[1:length(devs1)]))/mean(abs(tempdevs[-(1:length(devs1))]))
    if (direction[1]=='two.sided') RMDperms[i] <- max(1/RMDperms[i], RMDperms[i])
  }
  if (direction[1]=='greater') pVal <- mean(RMDperms>=RMD)
  if (direction[1]=='less') pVal <- mean(RMDperms<=RMD)
  if (direction[1]=='two.sided') pVal <- mean(RMDperms>=RMD)
  print(paste("Test statistic:",round(RMD,4)))
  print(paste("Approximate p-value for ",direction[1],":  ",pVal,sep=""))
}

RMD.test(c(16.55,15.36,15.94,16.43,16.01),
  c(16.05,15.98,16.10,15.88,15.91),
  direction="greater")

RMD.test(c(16.55,15.36,15.94,16.43,16.01),c(16.05,15.98,16.10,15.88,15.91),direction="greater",nsamp=100000)

RMD.test(bwt[smoke==1],bwt[smoke==0])


### plot empirical CDF's

plot(0,0,type="n",xlim=c(5,22),ylim=c(0,1),xlab="x",ylab="Empirical CDF")
set.seed(907771)
samp1 <- sort(rnorm(15,15,4))
samp2 <- sort(15+2*rt(15,4))

for (i in 1:16){
  lines(c(0,samp1,30)[i:(i+1)],rep((i-1)/15,2),col="red",lwd=5)
  lines(c(0,samp2,30)[i:(i+1)],rep((i-1)/15,2),col="gray",lwd=2.5)
}

samp1
samp2

legend(5,1,paste("Sample",c("A","B")),col=c("red","gray"),lty=1,lwd=c(5,2.5))


# Comparison of several techniques #

RMD.test(samp1, samp2,nsamp=100000)

permTS(samp1,samp2,method="exact.mc",control=permControl(nmc=100000,seed=987971))

wilcox.test(samp1,samp2)

t.test(samp1,samp2)

ks.test(samp1,samp2)

ks.test(bwt[smoke==1],bwt[smoke==0])




