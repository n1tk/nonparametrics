library(perm)

Treatment1 <- c(91, 87, 99, 77, 88, 91, 85, 79)
Treatment2 <- c(101, 110, 125, 93, 99, 205, 115) 

hist(Treatment1)
hist(Treatment2)
###Permutation-based p-value
permTS(Treatment1, Treatment2, method="exact.ce")  ### Permutation-based p-value

##b Wilcoxon Rank Sum p-value
wilcox.test(Treatment1, Treatment2,exact=FALSE)  ### Wilcoxon Rank Sum p-value

