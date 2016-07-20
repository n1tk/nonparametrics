y <- c(4.5, 3.5, 1.0, 6.3, 4.3, 1.3, 5.7, 3.3, 1.0, 5.3, 5.0, 1.7)
tmt <- c("S", "M", "R", "S", "M", "R", "S", "M", "R", "S", "M", "R")
block <- rep(1:4, each = 3)



growdata <- data.frame(list(y=y, tmt=tmt, block=block))
fit <- aov(y ~tmt+as.factor(block), data = growdata)
summary(fit)
our.F <- (summary(aov(y~tmt+as.factor(block), data=growdata))[[1]])[1,4]
our.F

nperm <- 10000
Fstars <- rep(NA, nperm)

for (i in 1:nperm) {
  tempdata <- growdata
  tempdata$y <- c(sample(tempdata$y[1:3]), sample(tempdata$y[4:6]), sample(tempdata$y[7:9]),  sample(tempdata$y[10:12]))
  Fstars[i] <- (summary(aov(y~tmt+as.factor(block), data = tempdata))[[1]])[1,4]
}

hist(Fstars)
#F-stat

pvalue <- sum(Fstars >= our.F)/nperm
pvalue

