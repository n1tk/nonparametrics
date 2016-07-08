### In class Assignment

library(MASS)
library(perm)
attach(birthwt)

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

###RMD test

RMD.test(bwt[ht == 1], bwt[ht == 0])
RMD.test(bwt[ui == 1], bwt[ui == 0])
### KS test
ks.test(bwt[ht == 1], bwt[ht == 0])



### will start with KS test
ks.test(birthwt$bwt[birthwt$ht == 1], birthwt$bwt[birthwt$ht == 0])
table(birthwt$ht)

###
ks.test(birthwt$bwt[birthwt$ui == 1], birthwt$bwt[birthwt$ui == 0])
table(birthwt$ui)

hist(birthwt$bwt[birthwt$ui == 1])
hist(birthwt$bwt[birthwt$ui == 0])

###compute t.test
t.test(bwt~ui, data=birthwt)

#compute wilcox test 

wilcox.test(bwt~ui, data=birthwt)


