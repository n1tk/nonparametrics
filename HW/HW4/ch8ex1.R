#ch8ex1

library(boot)
rabbits<-c(55,140,91,122,111,185,203,101,
           76,145,95,101,196,45,299,226,65,70,196,72,121,171,151,113,
           112,67,276,125,100,81,122,71,158,78,162,128,96,79,67,119)

hist(rabbits)
lines(density(rabbits), col = "red")
sample(rabbits, replace = T)
#MSE of sample mean

samp.mean <- mean(rabbits)
samp.std <- sd(rabbits)

set.seed(97971)

boot.means <- rep(NA,10000)
boot.stds <- rep(NA,10000)
boot.t <- rep(NA,10000)
boot.bca <- rep(NA,10000)

for (i in 1:10000){
  tempsample <- sample(rabbits,replace=TRUE)
  boot.means[i] <- mean(tempsample)
  boot.stds[i] <- sd(tempsample)
  boot.t[i]<- (mean(tempsample)-mean(rabbits))/(sd(tempsample)/sqrt(length(tempsample)))
}

#MSE, Standar eoor and margin of error for sample mean, the sample standard deviation and the coeficient of variations.
mse1<- mean((boot.means-samp.mean)^2)
mse1
stderr1<- sd(boot.stds)
stderr1
moe1<- 2*sqrt(mse1)
moe1

mse2<- mean((boot.stds-samp.std)^2)
mse2
stderr2<- sd(boot.stds)
stderr2
moe2<- 2*sqrt(mse2)
moe2

cvsample<-100*samp.std/samp.mean
cvboot<-100*boot.stds/boot.means
mse3<- mean((cvboot-cvsample)^2)
mse3
stderr3<- sd(cvboot)
stderr3
moe3<- 2*sqrt(mse3)
moe3
