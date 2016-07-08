
### Famous data set used in statistics, collected by Edgar Anderson.
iris
head(iris)
attach(iris)

boxplot(Petal.Length~Species,main="Petal Length")


# Standard ANOVA

aov(Petal.Length ~ Species)
summary(aov(Petal.Length~Species))

## See if there are any missing values of the petal length variable
table(is.na(Petal.Length))


## See how many observations there are from each species
ns <- table(Species)
ns

## See how many total observations there are
N <- sum(ns)
N



library(perm)

# Permutation-based p-value (using approximation)
permKS(Petal.Length~Species)

# Permutation-based p-value (using Monte Carlo sampling)
permKS(Petal.Length~Species, method='exact.mc',
 control=permControl(nmc=10000,seed=90771))


### My version of the permutation test: (based on steps in Higgins 2004, p. 81)

## Step 1: Get F-statistic

# Construct the F statistic "by hand"
means <- tapply(Petal.Length,Species,mean)
SSTreat <- sum(ns*(means-mean(Petal.Length))^2)

SSE <- sum((Petal.Length-rep(means,each=50))^2)

MSTreat <- SSTreat/(length(unique(Species))-1)
MSE <- SSE/(N-length(unique(Species)))

Fstat <- MSTreat/MSE
Fstat


## Step 2: I first check if feasible to get the exact p-value using all possible permutations

# How many possible permutations? 150!/ (50! 50! 50!)
exp(lfactorial(150)-3*lfactorial(50))

## Complete enumeration? Not going to happen ...

### Combined Steps 2 and 3: Sample the permutations and compute the F stat on each
set.seed(90711)
Fstat.star <- rep(NA, 10000)
for (i in 1:10000){
  #### Permute the observations
  tempPerm <- sample(Petal.Length,length(Petal.Length),replace=FALSE)
  Fstat.star[i] <- (unlist(summary(aov(tempPerm~Species))[[1]][4])[1])
  Fstat.star[i]
}


## Step 4
pvalue <- mean(unlist(Fstat.star) >= Fstat)
pvalue

# Histogram of the Fstat.star values:

hist(Fstat.star)

plot(tt<- seq(0,8,by=0.001),df(tt,2,147),type="l")
lines(density(Fstat.star),col="blue")

### Does the chi-squared-distribution approximation work well?



## Kruskal-Wallis Test 

#(using approximate distribution for p-value calculation)
kruskal.test(Petal.Length,Species)

# Repeat of kruskal.test results
permKS(rank(Petal.Length),Species)

# using Monte Carlo to approximate p-value
permKS(rank(Petal.Length),Species,
  method="exact.mc",control=permControl(nmc=10000,seed=5719))



### Permutation test for a difference in medians between setosa and virginica
setosa <- Petal.Length[Species=='setosa']
virginica <- Petal.Length[Species=='virginica']


# Step 1: the value of the statistic from the observed data

dif.median <- median(setosa)-median(virginica)

## Steps 2,3: Create permutations and compute the statistic from each

set.seed(1321341)
difmeds.star <- rep(NA,1000)
for (i in 1:1000){
  allvals <- c(setosa,virginica)
  shuffled.values <- sample(allvals,length(allvals),replace=FALSE)
  ## The first 50 of the values will be assumed to be from SETOSA in the reshuffling
  perm_setosa <- shuffled.values[1:50]
  perm_virg <- shuffled.values[51:100]
  difmeds.star[i] <- median(perm_setosa)-median(perm_virg)
}
hist(difmeds.star)

# Step 4: Approximate p-value
mean(difmeds.star<= dif.median)



##### Permutation test for the difference in means between two populations

# I'll compare versicolor (population 1) to virginica for petal ***width***

mean(versicolor)-mean(virginica)

