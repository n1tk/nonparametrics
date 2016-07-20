x <- c(1:9, 100)
x
boot.mean <- rep(NA, 10000)
set.seed(781)
for (i in 1:10000) {
  boot.mean[i] <- mean(sample(x, replace=TRUE))
}

hist(boot.mean)
  

data811 <- c(7,11,15,16,20,22,24,25,29,33,34,37,41,42,49,57,66,71,84,90)

samp.mean <- mean(data811)
samp.var <- var(data811)
samp.min <- min(data811)
boot.means <- rep(NA, 10000)
boot.vars <- rep(NA, 10000)
boot.mins <- rep(NA, 10000)
set.seed(97971)

for (i in 1:10000) {
  boot.means[i] <- mean(sample(data811, replace = TRUE))
  boot.vars[i] <- var(sample(data811, replace = TRUE))
  boot.mins[i] <- min(sample(data811, replace = TRUE))
}

MSE.mean <- mean((boot.means - samp.mean)^2)
MSE.mean
sqrt(MSE.mean)

MSE.var <- var((boot.vars - samp.var)^2)
MSE.var

MSE.min <- min((boot.mins - samp.min)^2)
MSE.min
