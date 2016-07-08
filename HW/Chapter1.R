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

test.median(c(13,16,18,40,22),40,"less")
test.median(c(13,16,18,40,22),40)


table.1.1.1 <- c(72.1,72.8,72.9,73.3,73.3,73.3,73.9,74.0,74.2,74.2,
                 74.3,74.6,74.7,75.0,75.1,75.1,75.2,75.3,75.3,75.3,
                 75.4,76.1,76.5,76.5,76.6, 76.9,77.1,77.2,77.4,77.4,
                 77.7,78.0,78.3,78.6,78.8,78.9,79.7,80.3,80.5,81.0)
test.median(table.1.1.1,   75,   "greater")



