###Sergiu Buciumas HW1 R code used for solving problem(solutions). STAT7900_Nonparametrics
library(perm)
library(asbio)
###Chapter 1 Exercises: 3

rain <- c(21.3, 28.8, 17.6, 23.0, 27.2, 28.5, 32.8, 28.2, 25.9, 22.5, 27.2, 33.1, 28.7, 24.8, 24.3, 27.1, 30.6, 26.8, 18.9, 36.3, 28.0, 17.9, 25.0, 27.5, 27.7, 32.1, 28.0, 30.9, 20.0, 20.2, 33.5, 26.4, 30.9, 33.2)
sortedrain <- sort(rain)
# a) create a 95% interval for the median, will start with finding the median for rain
sortedrain
summary(rain)

length(rain)
rain[1:5]
mean(rain)
median(rain)
sd(rain)

test.median <- function(datvec,medval,Ha.dir=c('two.sided','less','greater')){
  n <- length(datvec[datvec!=medval])
  if (sum(datvec==medval)>0) print(paste("Note: ",sum(datvec==medval)," value(s) ignored because of equality to the null value of the median"))
  B <- sum(datvec>medval)
  print(paste("Number of data values exceeding the null median: B=",B, " of ",n))
  if (Ha.dir[1]=="less"){
    pval <- pbinom(B,n,.5)
    print(paste("P-value for Ha: median < ",medval,":  ",pval))
  } else {
    if (Ha.dir[1]=="greater"){
      pval <- 1-pbinom(B-1,n,.5)
      print(paste("P-value for Ha: median > ",medval,":  ",pval))
    } else {
      if (Ha.dir[1]=="two.sided") {
        pval <- sum(dbinom(0:n,n,.5)[dbinom(0:n,n,.5)<= dbinom(B,n,.5)])
        print(paste("P-value for Ha: median not equal to ",medval,":  ",pval))
      }
    }
  }
  return(pval)
}

test.median(rain, 27.35, "greater")

ci.median<-function(x,conf=.95){
  n<-nrow(as.matrix(x))
  if(qbinom((1-conf)/2,n,0.5)==0)stop("CI not calculable")
  L<- qbinom((1-conf)/2,n,0.5)
  U<-n-L+1
  if(L>=U)stop("CI not calculable")
  order.x<-sort(x)
  res<-list()
  res$head<-paste(paste(as.character(conf*100),"%",sep=""),c("Confidence interval for population median"))
  res$ci<-c(median=median(x),lower=order.x[L],upper=order.x[n-L+1])
  res$ends<-c("Estimate",paste(as.character(c((1-conf)/2,1-((1-conf)/2))*100),"%",sep=""))
  res$coverage<-1-(2*pbinom(q=L-1,n,0.5))
  class(res)<-"ci"
  res
}



ci.median(rain, conf = 0.95)

lower<- -1.96*sqrt(34*0.5*0.5)+0.5*34 
upper<- 1.96*sqrt(34*0.5*0.5)+0.5*34+1

lower
upper

conflevel<-sum(dbinom(12:(24-1),34,0.5))
conflevel

###b 

ci.median(rain, conf = 0.90)
#20
lower<- -1.645*sqrt(34*0.2*0.8)+0.2*34
upper<- 1.645*sqrt(34*0.8*0.2)+0.2*34+1

lower
upper

conflevel<-sum(dbinom(4:(12-1),34,0.2))
conflevel

#80

lower<- -1.645*sqrt(34*0.2*0.8)+0.8*34
upper<- 1.645*sqrt(34*0.8*0.2)+0.8*34+1
lower
upper
conflevel<-sum(dbinom(24:(32-1),34,0.8))
conflevel

conflevel<-sum(dbinom(23:(31-1),34,0.8))
conflevel


##c
acf(rain, type="correlation")
acf(sortedrain, type="correlation")

#need to answer

###Chapter 1 Ex 5
#creating dataset
data<-c(rep(75.1,39),90)
data
mean<-mean(data)
median <- median(data)
summary(data)
sd<-sd(data)

1-pnorm(mean,75,sd/sqrt(40))

test.median(data,75,"greater")
#b

t.test(data, alternative="greater", mu=75)

### Chapter 1 Ex 6

#a. The value of the power of binomial test when  = 75 is equal to 0.05.
#b. As  gets large the power will also increase.
#c. By increasing the the sample size will result in increasing the the power of the binomial test.

#Chapter 2
#Problem Chapter 2
###4

section1 <- c(5, 11, 16, 8, 12)
section2 <- c(17, 14, 15, 21, 19, 13)


## perm package is installed
library(perm)             ## Load the perm package
permTS(section1, section2, method="exact.ce")  ### Permutation-based p-value

mean(section1)
mean(section2)

teststatistic <- mean(section1)-mean(section2)
teststatistic
##b Wilcoxon Rank Sum p-value
wilcox.test(section1,section2,exact=TRUE,alternative='two.sided')  ### Wilcoxon Rank Sum p-value
  
#Chapter 2. Ex5.
speciesA <- c(5.1, 9.4, 7.2, 8.1, 8.8)
speciesB <- c(2.5, 4.2, 6.9, 5.5, 5.3)
wilcox.test(speciesA,speciesB,exact=TRUE,alternative='two.sided')  ### Wilcoxon Rank Sum p-value
### Wilcoxon Rank Sum p-value

#Chapter 2. Ex6.
Treatment1 <- c(91, 87, 99, 77, 88, 91, 85, 79)
Treatment2 <- c(101, 110, 125, 93, 99, 205, 115) 

wilcox.test(Treatment1,Treatment2,exact=TRUE,alternative='two.sided') 
t.test(Treatment1,Treatment2)
#need to complete
