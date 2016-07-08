diffs <- c(-3, -6, 4, -8, 0, -1, -15)
dbar <- mean(diffs)
dbar

### Random permutations of the sign 

permdbar <- rep(NA, 10000)
for (k in 1:10000) {
  permdiff <- sample(c(-1, 1), length(diffs), replace = TRUE) * diffs
  permdbar <- mean(permdiff)
  print(permdbar)
}


set.seed(428571)
permdbar <- rep(NA, 10000)
for (k in 1:10000){
  permdiff <- sample(c(-1,1), length(diffs), replace = TRUE) * diffs
  permdbar[k] <- mean(permdiff)
}

hist(permdbar)

### p-value when Ha: differences tend to be positive
mean(permdbar >= dbar)

### p-value when Ha: diferrences tend to be positive or differences tend to be positive

mean(abs(permdbar) >= abs(dbar))

0.773*64
5/64

###look at distribution of diffs
hist(diffs)
t.test(diffs, mu = 0)


##wilcoxon signed rank test
before <- c(65, 73, 58, 83, 58, 62, 78)
after <- c(62, 67, 62, 75, 58, 61, 63)

wilcox.test(after, before, paired = TRUE)
