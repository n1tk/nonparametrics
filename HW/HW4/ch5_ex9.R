#ch5Ex9

act <- c(rep(1,13), rep(2, 22), rep(3, 21))
grades <- c(rep(4,4), rep(3, 5), rep(2, 1), rep(1,3), rep(4, 5), rep(3,8), rep(2, 6), rep(1,3), rep(4, 1), rep(3, 4), rep(2, 10), rep(1, 6))
data <- c(act, grades)
res.chi <- chisq.test(act, grades, simulate.p.value=TRUE, B=1000)$statistic
res.chi

#Permutation test for contingency table
permutation <- function(x,y,B)
{
  results <- rep(NA,B)
  for (i in 1:B) results[i] <- chisq.test(sample(x,length(x)),y, simulate.p.value=TRUE)$statistic
  results
}
permX <- permutation(act, grades, 10000)
permX
mean(permX >= chisq.test(act, grades, simulate.p.value=TRUE)$statistic) #to get p-value manually

#compute using chisq.test and replecate to 10000
chisq.test(act, grades, simulate.p.value=TRUE, B=10000)

#kruskal test

kruskal.test(act, grades)

#c Exact/permutation version of Jonckheere-Terpstra test

install.packages("clinfun")
library(clinfun)

jonckheere.test(act, grades, alternative = "decreasing", nperm = 10000)
