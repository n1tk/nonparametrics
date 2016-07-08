### In-class R code To accompany Lecture 5


diffs <- c(-3, -6, 4, -8, 0,-1,-15)
diffs2 <- c(-3, -6, 4, -8, -1,-15)   # 0 removed


(dbar <- mean(diffs2))
dbar

### for Monte Carlo, we will permute a large number of differences
### and for each calculate the new dbar, called dbarstar

dbarstar <- rep(NA,100000)   ## to hold the 10000 permutation-based
											## values of dbarstar

for (k in 1:100000){
  permvals <- ((-1)^rbinom(7,1,.5))*diffs
  dbarstar[k] <- mean(permvals)
}
dbarstar[1:20]

# one-sided lower tail p-value:
mean(dbarstar <=  dbar)


### Classic paired t-test
t.test(diffs,alternative='less')    ### using already computed differences
t.test(c(62,67,62,75,58,61,63),c(65,73,58,83,58,62,78),alternative='less',paired=TRUE)   ### using undifferenced after and before measurements

### STAT importance example
importance <- read.csv("C:/Users/bbarney2/Downloads/importance.csv")
head(importance)
attach(importance)

# compute the differences
differs <- after-before

# paired t-test
t.test(differs,alternative='greater')

#permutation based p-value

dbar <- mean(differs)
dbar
dbarstar <- rep(NA,100000)   ## to hold the 10000 permutation-based
											## values of dbarstar

for (k in 1:100000){
  permvals <- ((-1)^rbinom(19,1,.5))*differs
  dbarstar[k] <- mean(permvals)
}
dbarstar[1:20]

# one-sided upper tail p-value:
mean(dbarstar >=  dbar)





#Wilcoxon signed-rank test
wilcox.test(differs,alternative='greater')

# Sign test of the values in differs
## 9 of 11 nonzero values are +
1-pbinom(8,11,.5)

qqnorm(differs)
qqline(differs)
hist(differs)










