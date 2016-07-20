#class 22
#randomized blocks

y <- c(87, 84, 81, 95, 93, 96, 81, 82, 82)
tmt <- c("C", "A", "B", "B", "A", "C", "A", "C", "B")
block <- rep(1:3, each = 3)



growdata <- data.frame(list(y=y, tmt=tmt, block=block))
fit <- aov(y ~tmt+as.factor(block), data = growdata)
summary(fit)
our.F <- (summary(aov(y~tmt+as.factor(block), data=growdata))[[1]])[1,4]
our.F

nperm <- 10000
Fstars <- rep(NA, nperm)

for (i in 1:nperm) {
  tempdata <- growdata
  tempdata$y <- c(sample(tempdata$y[1:3]), sample(tempdata$y[4:6]), sample(tempdata$y[7:9]))
  Fstars[i] <- (summary(aov(y~tmt+as.factor(block), data = tempdata))[[1]])[1,4]
}

hist(Fstars)
#F-stat

pvalue <- sum(Fstars >= our.F)/nperm
pvalue
