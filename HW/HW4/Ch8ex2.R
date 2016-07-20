#ch8ex2

library(boot)

rabbits<-c(55,140,91,122,111,185,203,101,
           76,145,95,101,196,45,299,226,65,70,196,72,121,171,151,113,
           112,67,276,125,100,81,122,71,158,78,162,128,96,79,67,119)

#BCA method
p0 <- mean(boot.means <= mean(rabbits))
z0 <- qnorm(p0)
zp <- qnorm(.975)

theta.i.s <- rep(NA,length(rabbits))
for (i in 1:length(rabbits)){
  theta.i.s[i] <- mean(rabbits[-i])    # to omit the ith value 
}
mthetai <- mean(theta.i.s)
a <- sum((mthetai-theta.i.s)^3)/(6*(sum((mthetai-theta.i.s)^2))^(3/2))

# Calculate ZL, ZU
ZL <- (z0-zp)/(1-a*(z0-zp)) + z0
ZU <- (z0+zp)/(1-a*(z0+zp)) + z0

#  percentiles lowerperc <- pnorm(ZL)
upperperc <- pnorm(ZU)
sort(boot.means)[c(lowerperc,upperperc)*(9999+1)]


#t-pivot method

sampmean-sort(boot.t)[c(.975,.025)*(9999+1)]*samp.stds/sqrt(length(rabbits))
