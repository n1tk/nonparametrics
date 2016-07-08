fakedata <- c(4,4,5,7,9,11)  ## Ha:mu>83
?t.test   ### Bringing up the help for the t-test
t.test(fakedata,mu=83,alternative="greater")  ### Computing the
# test statistic and p-value for H0: mu=83 vs. Ha: mu > 83

?wilcox.test   ## Bringing up the help for the Wilcoxon test

### In-class exercise data:
tmt1 <- c(59.1, 60.3, 58.1, 61.3, 65.1, 55.0, 63.4, 67.8)
tmt2 <- c(60.1, 62.1, 59.3, 55.0, 54.6, 64.4, 58.7, 62.5)


install.packages("perm")   	## Ensure that the 
												## perm package is installed
library(perm)             ## Load the perm package
permTS(tmt1,tmt2,method="exact.ce")  ### Permutation-based p-value
wilcox.test(tmt1,tmt2,exact=FALSE)  ### Wilcoxon Rank Sum p-value
