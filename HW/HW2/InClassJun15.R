
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
means
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
dif.median

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







vehicle <- read.csv("C:/Users/bbarney2/Downloads/vehicle.csv")
head(vehicle)
jpeg("C:/Users/bbarney2/Desktop/vehicleweights.jpg")
boxplot(load~weight,data=vehicle)
dev.off()


summary(aov(load~as.factor(weight)   ,data=vehicle))
class(as.factor(vehicle$weight))

permKS(load~as.factor(weight),data=vehicle,method="exact.mc",
 control=permControl(nmc=10000,seed=54))

kruskal.test(load~weight,data=vehicle)




d36 <- read.csv("C:/Users/bbarney2/Downloads/Exercise_3_6_head_injury.csv")
head(d36)
d36.alt <-data.frame(list(cartype=rep(names(d36),each=10),
 impact=matrix(t(t(d36)),byrow=FALSE,ncol=1)))

head(d36.alt)
kruskal.test(impact~cartype,data=d36.alt)
d36.alt$rank <- rank(d36.alt$impact)
head(d36.alt)


### for Fisher's Protected LSD, given that there was a significant global test.
### (see kruskal.test above)
for (i in 1:6){
  for (j in (i+1):7){
    TIJ <- rep(NA,10000)
    for (k in 1:10000){
      temp.sample <- sample(d36.alt$rank,70,replace=FALSE)
      TIJ[k] <- mean(temp.sample[(i-1)*10+1:10])-
             mean(temp.sample[(j-1)*10+1:10])
    }
   LSD.crit.val <-    quantile(abs(TIJ),.95)
   our.TIJ <- mean(d36.alt$rank[(i-1)*10+1:10])-
             mean(d36.alt$rank[(j-1)*10+1:10])
   if(abs(our.TIJ) >= LSD.crit.val) print(
     paste("Difference between Group",unique(d36.alt$cartype)[i],
           "and Group", unique(d36.alt$cartype)[j]))
  }
}

### for Tukey's HSD

TIJ <- rep(NA,100000)
for (k in 1:100000){
  tempdata <- data.frame(list(ctype=d36.alt$cartype,
                       permrank=sample(d36.alt$rank,70,replace=FALSE)))
  tempmeans <- tapply(tempdata$permrank,tempdata$ctype,mean)
  pwdiffs <- outer(tempmeans,tempmeans,"-")


      TIJ[k] <- max(abs(pwdiffs))
    }
hist(TIJ)
HSD.crit.val <- quantile(TIJ,.95)

for(i in 1:6){
  for (j in (i+1):7){
   our.TIJ <- abs(mean(d36.alt$rank[(i-1)*10+1:10])-
             mean(d36.alt$rank[(j-1)*10+1:10]))
   if(abs(our.TIJ) >= HSD.crit.val) print(
     paste("Difference between Group",unique(d36.alt$cartype)[i],
           "and Group", unique(d36.alt$cartype)[j]))
  }
}




### For Bonferroni Test
for(i in 1:6){
  for (j in (i+1):7){
    if(permTS(d36.alt$rank[(i-1)*10+1:10],d36.alt$rank[(j-1)*10+1:10])$p.value <= (.05/21)) print(
     paste("Difference between Group",unique(d36.alt$cartype)[i],
           "and Group", unique(d36.alt$cartype)[j]))
  }
}


### No differences were significant per Bonferroni with experimentwise error rate of .05.


















