setwd("/Users/sbuciuma/Desktop/School/summer")

Maxvals <- rep(NA, 1000)

for (i in 1:1000){
  Maxvals[i] <- max(rt(15,7))
}

plot(density(Maxvals))

getwd()

5*8
exp(-3^2)
iris
head(iris)
str(iris)

hist(iris$Petal.Length)
iris$Petal.Ratio <- iris$Petal.Length/iris$Petal.Width

head(iris)
boxplot(iris$Petal.Ratio)
mean(iris$Petal.Ratio)

msas.expected <- c(85, 120, 70, 95, 85, 60, 85, 90, 80, 90, 70, 50, 125, 95, 100, 60, 85)
length(msas.expected)
msas.expected[1:5]
mean(msas.expected)
median(msas.expected)
sd(msas.expected)

##test statistics
(85-90)/(19.76/sqrt(17))

##pvalue calculation in R (n-1 degree of freedom)
pvalue <- 2*(1- pt(1.04, 16))
pvalue
pvalue1 <- 2*(pt(-1.04, 16))
pvalue1
plot(mtcars)

plot(0:15, dbinom(0:15, 15, .5))

pbinom(4, 15,  .5, lower = FALSE)
1- pbinom(4, 15, .5)

sum(dbinom(5:15, 15, .5))
pbinom(5,15, .5)