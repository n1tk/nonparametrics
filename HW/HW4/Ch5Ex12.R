data <- matrix(c(4, 5, 14, 7), 
               nrow = 2, 
               dimnames = list( c("Missed First Attempt", "Made First Attempt"),
                                c("Missed Second Attempt", "Made Second Attempt")))

data

mcnemartest <- mcnemar.test(data)
mcnemartest

#b Chi-Square Test
chisq.test(data)

chisq.test(data, simulate.p.value=TRUE , B=10000)

