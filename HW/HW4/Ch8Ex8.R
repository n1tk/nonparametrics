#ch8ex8
install.packages("wBoot")
install.packages("simpleboot")
library(wBoot)
library(boot)

rural <- c(3,2,1,1,2,1,3,2,2,2,2,5,1,4,1,1,1,1,6,2,2,2,1,1)
urban <- c(1,0,1,1,0,0,1,1,1,8,1,1,1,0,1,1,2)

# A. Make a 90% CI for the difference between the means of the populations using BCA

boot.two.bca(rural, urban, mean, stacked = FALSE, conf.level = 0.90)

# B. Make a 90% bootstrap CI using z statistic method.
resid.r <- rural - mean(rural)
resid.u <- urban - mean(urban)

z <- rep(NA,10000)
for (i in 1:10000) {
  z[i] <- t.test(sample(resid.r,replace=TRUE),sample(resid.u,replace=TRUE),var.equal=FALSE)$statistic
}

std.error <- (mean(rural)-mean(urban))/t.test(rural,urban, var.equal=FALSE)$statistic

CI <- mean(rural)-mean(urban) -sort(z)[c(.95,.05)*(10000+1)]*std.error
CI
#computation of the standard error of the mean
sd.rural<-sd(rural)/sqrt(length(rural))
sd.rural
sd.urban<-sd(urban)/sqrt(length(urban))
sd.urban
#95% confidence intervals of the mean
CI <- mean(rural)-mean(urban) -sort(z)[c(.95,.05)*(10000+1)]*std.error
CI
