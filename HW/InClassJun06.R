### trial and error approach to finding the a, b pair
### for which x_(a), x_(b) is a 90 confidence interval for 
### the population median if n=21:

a<- 7   ## try a value here
b <- 15   ### try a value here (make sure that b>a).
conf.level <- sum(dbinom(a:(b-1),21,.5))
conf.level


### To get better initial values for a and b, I use the normal approximation
### to solve for a and b and then I round these to integer values
approx.a <- -1.645*sqrt(.25*21)+21*.5
approx.b <- 1.645*sqrt(.25*21)+21*.5+1
round(approx.a)
round(approx.b)

###### In-class example exploring the bwt variable from the birthwt data set.
library(MASS)    #### load the package that contains the data
birthwt$bwt        ### view the variable of interest
#  birth <- read.csv("C:/Users/bbarney2/Downloads/birthwt.csv")   ##could read in the data
																					#### this way, if so desired

x <- birthwt$bwt    ### to save myself keystrokes in typing
xsort <- sort(x)    ### the sorted values of x.
xsort               ### verify that the values are sorted.
sum(xsort>2700)   ## test stat for H0: median is 2700 vs. alternative: Ha: median>2700
length(xsort)     ## n

1-pbinom(119,189,.5)   ### Exact p-value: Pr(X>= 120), which equals 1-Pr(X<= 119),
                       ### where X has  binomial(N=189, p=.5) distribution.
1-pnorm(120,189*.5,sqrt(189*.5*.5))   ### normal approx. p-value
1-pnorm(119.5,189*.5,sqrt(189*.5*.5))  ## normal approx w/ cont. corr.


### what percentile is a baby born at 2800 g
sum(xsort<= 2800)/length(xsort)    ### the sample percentile, about 41st.
phat <-mean(xsort<= 2800)          ### same value as above, but now stored for later use
phat +c(-1,1)*2.576*sqrt(phat*(1-phat)/189)   ### a 99% CI for F(2800)
### We are 99% confident that 2800 is anywhere from the 31.5th to the 49.9th percentile


### 95% CI for median birth weight?
qnorm(.025)     ### about -1.96



approx.a <- -1.96*sqrt(.25*189)+189*.5
approx.b <- 1.96*sqrt(.25*189)+189*.5+1
approx.a
approx.b

## I want about the 81st and 109th smallest values. The confidence level would be slightly
## high if I used the 81st and 109th values, though. We can still have 95% confidence
## by using the 82nd and 109th smallest values as the interval endpoints. [We could
## instead use the 81st and 108th smallest values and get this same level of confidence.]

a<- 82
b <- 109
conf.level <- sum(dbinom(a:(b-1),189,.5))
conf.level
xsort[c(82,109)]  ### getting the 82nd and 109th smallest values from the sorted birthweights


#### Functions to accompany Chapter 1

### Binomial Test for Median
### Inputs: 
####  datvec: vector of data points (should not have any missing values)
###   medval: null value of median
###   Ha.dir: "two.sided", "less", OR "greater" (the default is "two.sided")
### Displays the test statistic and p-value, and it returns the p-value

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



### example data from Table 1.1.1 of the textbook
table.1.1.1 <- c(72.1,72.8,72.9,73.3,73.3,73.3,73.9,74.0,74.2,74.2,
                 74.3,74.6,74.7,75.0,75.1,75.1,75.2,75.3,75.3,75.3,
                 75.4,76.1,76.5,76.5,76.6, 76.9,77.1,77.2,77.4,77.4,
                 77.7,78.0,78.3,78.6,78.8,78.9,79.7,80.3,80.5,81.0)

### example usage of the function
test.median(table.1.1.1,   75,   "greater")   


#### Installing the "perm" package ####
install.packages("perm")
### Loading the 'perm' package
library(perm)
### bringing up the html help page that corresponds to the perm package documentation.
help(package="perm")

