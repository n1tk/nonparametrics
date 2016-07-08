## (The pound sign is for comments) 
getwd()   ### see what the current working directory is 
setwd("C:/Users/bbarney2/Desktop")   ### I changed the working directory to this folder
5*8   ### just demonstrating five times eight
exp(-3^2)   #### just demonstrating e^(-9)

iris     ### One of the pre-installed data sets in R--this displays all of the data in iris
head(iris)   ### Displaying the first several rows of the iris data set
str(iris)    ### Information about the structure of iris

hist(iris$Petal.Length)    #### A histogram of the Petal.Length variable of the iris data set
iris$Petal.Ratio <- iris$Petal.Length/iris$Petal.Width   ### creating a new variable in the iris data set
head(iris)  ### Again looking at the first several rows of iris
boxplot(iris$Petal.Ratio)   ###Making a boxplot of my new variable
mean(iris$Petal.Ratio)      ## Calculating the average of the Petal.Ratio variable

msas.expected <- c(85,120,70,95,85,60,85,90,80,90,70,50,125,95,100,60,85)  ### in-class data
                       ### on student guesses of the expected MSAS salary five years after graduation

msas.expected[1:5]    ### displaying the first five numbers
length(msas.expected)     ## 17 values
mean(msas.expected)   ### the average
median(msas.expected)   ### the median
sd(msas.expected)    #### the standard deviation, s

### the standard t-test for the mean if H0: mu=90 and Ha: mu not equal to 90

(85-90)/(19.76/sqrt(17))   #### the test statistic for testing H0: mu=90 vs. HA: mu not equal to 90,
                           ### which is about -1.04

pvalue <- 2*(pt(-1.04,16))   ### If assumptions hold, then the distribution of the test statistic
                            #### is t with df=n-1=16 if the null hypothesis is actually true.
                            ### Thus, I compute 2*Pr(T <=   -1.04)
pvalue   ### displaying the value stored as pvalue



##### Binomial test of the median. 
#### H0: median=90 vs. Ha: median > 90 (note the one-sided nature of this test)

#### sort the values
sort(msas.expected) 



####  The two values in the sample data that exactly equal the null value are ignored.
##### That leaves n=15 values that don't equal the null median.


### If the median were really 90, the chance of any given value exceeding 90 would be .5
### Assuming *independence* of the 15 values, which seems reasonable,
### the count for how many of the 15 values exceed 90 would have
### a Binomial(n=15,p=.5) distribution if H0 is true.

## Side Note: Among other properties, then, we would have expected
## half of these 15 values (or 7.5) to exceed 90, with a standard deviation of
## sqrt(np(1-p))=sqrt(15*.5*(1-.5))=sqrt(3.75), or about 1.94


#### The test statistic: how many of the 15 values actually exceeded the null median of 90?
#### 5 values exceed the null median of 90

### Test statistic is 5. p-value=?
### p-value is the chance of obtaining a test statistic at least as extreme as the observed
### test statistic if H0 is true.  

### Because Ha: median GREATER than 90, then what is considered "at least as extreme"?
### The reasoning goes like this: if Ha is true, then the chance of any one value being above 90 
### would be even more than 0.5. Which would mean we would have on average even more than 7.5 of 15
### values exceeding 90. Which suggests that in this instance (i.e., with Ha: median EXCEEDS null value), 
### extremity is indicated by getting even LARGER values
### of the test statistic. So the p-value = Pr (X>= test statistic), where X~Binom(15,.5)

### We need Pr(X>=5), and below I show three ways to compute this--each of them work fine.

pbinom(4,15,.5,lower=FALSE)  ### Pr(X>4), which matches what we want: Pr(X>= 5)
1-pbinom(4,15,.5)   ### 1-Pr(X<=4), which matches what we want: Pr(X>=5)
sum(dbinom(5:15,15,.5))  #### Pr(X>=5) by adding Pr(X=5)+Pr(X=6)+Pr(X=7)+...+Pr(X=15)
?pbinom     ## If you want to bring up the help page for the pbinom function.

### The p-value is 0.941, which is not statistically significant. (Not even a little bit)


### The probabilities for each possible value of the test statistic if H0 is true:
###  (we'll revisit this on Monday, June 6th).
plot(0:15,dbinom(0:15,15,.5))


