#ch8Ex5

library(boot)

moisture <- c(355, 370, 380, 380, 380, 400, 400, 415, 415, 415, 415, 430, 440, 440, 470)
shrew <- c(0.5, 2, 2, 0.5, 3, 0, 4, 0.5, 2, 4, 4, 1, 2, 5, 7)

data <- data.frame(moisture, shrew)

betas <- function(indata,indices) {
  tempdata <- indata[indices,]
  tempfit <- lm(moisture~shrew, data=tempdata)
  tempcoef <- coef(tempfit)
  return(c(b1=tempcoef[2]))
}
boots<- boot(data, betas, R=10000)
boots
boot.ci(boots,conf=0.9)
