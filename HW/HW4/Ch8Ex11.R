############# Code below is only a slight adaptation of code from the boot.ci 
############# documentation to repeat the calculation of
############# Higgins 2004, example 8.5.2
# studentized confidence interval for the two sample 
# difference of means problem using the final two series
# of the gravity data. 

library(boot)

diff.means <- function(d, f)
{    n <- nrow(d)
gp1 <- 1:table(as.numeric(d$series))[1]
m1 <- sum(d[gp1,1] * f[gp1])/sum(f[gp1])
m2 <- sum(d[-gp1,1] * f[-gp1])/sum(f[-gp1])
ss1 <- sum(d[gp1,1]^2 * f[gp1]) - (m1 *  m1 * sum(f[gp1]))
ss2 <- sum(d[-gp1,1]^2 * f[-gp1]) - (m2 *  m2 * sum(f[-gp1]))
# c(m1 - m2, (ss1 + ss2)/(sum(f) - 2))  # this line was changed to that below
c(m1-m2, (ss1/(length(gp1)*(length(gp1)-1))+ss2/((n-length(gp1))*(n-length(gp1)-1))))
}
#grav1 <- gravity[as.numeric(gravity[,2]) >= 7, ]  
## this line was changed into the line below so that we compare 1 to 8, not 7 to 8
grav1 <- gravity[as.numeric(gravity[,2]) %in% c(1,8), ]
set.seed(481)      ### I added this to allow replicability
grav1.boot <- boot(grav1, diff.means, R = 50000, stype = "f",   ### Here I increased R
                   strata = grav1[ ,2])
boot.ci(grav1.boot)


d811 <- read.csv("/Users/sbuciuma/Desktop/School/summer/nonparametric_statistics/HW4/Exercise8_11.csv")

head(d811)

beta.funcs <- function(indata,indices){
  tempdata <- indata[indices,]
  tempfit <- lm(DH~FH+MH,data=tempdata)
  tempcoef <- coef(tempfit)
  return(c(b1=tempcoef[2],b2=tempcoef[3],
           b1Minusb2=(tempcoef[2]-tempcoef[3])))  
}

boot.datos <- boot(d811,beta.funcs,R=10000)
boot.datos 
### Check if FH has nonzero slope
boot.ci(boot.datos,index=1)

## check if MH has nonzero slope
boot.ci(boot.datos,index=2)

### not required, but this tests if beta_FH=beta_MH because
### it evaluates to beta_FH-beta_MH
boot.ci(boot.datos,index=3)


## 95% CI's for beta1
boot.ci(boot.datos,index=1)  

## 95% CI's for beta2
boot.ci(boot.datos,index=2)

## 95% CI's for beta1+beta2
boot.ci(boot.datos,index=3)

boot.ci(boot.datos, index=1, type="bca")
boot.ci(boot.datos,type="bca", index=2)
boot.ci(boot.datos,type="bca", index=3)


####parb b Fixed x sampling, based on raw residuals
orig.fit <- lm(DH~FH+MH,data=d811)
resids <- resid(orig.fit)  ## compute the raw residuals from the model using the resid function
resids
d811$resids <- resids    ## Create a new variable in Angell that contains the raw residuals
d811
# Get bootstrap replicates of the desired t statistic (based off using the resampled raw residuals as the response)
bootts.rawresid <- rep(NA,nrep)
for (i in 1:nrep){
  temp.fit1 <- lm(sample(d811$resids,replace=TRUE)~log(d811$DH))
  bootts.rawresid[i] <- summary(temp.fit1)$coef[2,3]
}
# Create the confidence interval
CI.using.raw.residuals <- summary(orig.fit)$coef[2,1]-sort(bootts.rawresid)[c(.975,.025)*(nrep+1)]*summary(orig.fit)$coef[2,2]
CI.using.raw.residuals

### Fixed x sampling, based on residuals with correction for leverage
#### The hat diagonals (that is, diagonal values of the H matrix) may be obtained by using the hatvalues
#### function on the model
d811$correctedRes <- resid(orig.fit)/sqrt(1-hatvalues(orig.fit))

# Note the comparison of the raw residuals and leverage-corrected residuals
d811

# Get bootstrap replicates of the desired t statistic (based off using the resampled corrected residuals as the response)
bootts.correctedresid <- rep(NA,nrep)
for (i in 1:nrep){
  temp.fit2 <- lm(sample(d811$correctedRes,replace=TRUE)~log(d811$DH))
  bootts.correctedresid[i] <- summary(temp.fit2)$coef[2,3]
}
# Create the confidence interval
CI.using.corrected.residuals <- summary(orig.fit)$coef[2,1]-sort(bootts.correctedresid)[c(.975,.025)*(nrep+1)]*summary(orig.fit)$coef[2,2]
CI.using.corrected.residuals
