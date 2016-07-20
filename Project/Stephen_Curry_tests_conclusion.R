#Curry project for non-parametrics
#loading data
library(perm)
Curry <- read.csv("/Users/sbuciuma/Desktop/School/summer/nonparametric_statistics/Project/CurryComp.csv")

#selecting data in offseason and in season

Curry <- Curry[Curry[,2]==1,]
Curry

# create vectors for our data Min data for easy analysis

min.playoffs1 <- c(Curry$MIN[Curry$Playoffs == 1])
min.playoffs1
min.playoffs0 <- c(Curry$MIN[Curry$Playoffs == 0])
min.playoffs0

#mean
mean(min.playoffs1)
mean(min.playoffs0)

#median
median(min.playoffs1)
median(min.playoffs0)

#visualizing the data
boxplot(min.playoffs1, min.playoffs0, col = c("red","blue"), names = c("min.playoffs1","min.playoffs0"))
hist(min.playoffs1)
hist(min.playoffs0)

#create vectors for our data PTS data for easy analysisPTS
PTS.playoffs1 <- c(Curry$PTS[Curry$Playoffs == 1])
PTS.playoffs1
PTS.playoffs0 <- c(Curry$PTS[Curry$Playoffs == 0])
PTS.playoffs0

#mean

mean(PTS.playoffs1)
mean(PTS.playoffs0)

#median

median(PTS.playoffs1)
median(PTS.playoffs0)

#boxplot 
boxplot(PTS.playoffs1, PTS.playoffs0, col = c("royalblue2","palevioletred1"), names = c("PTS.playoffs1","PTS.playoffs0") )
#histogram
hist(PTS.playoffs1)
hist(PTS.playoffs0)

#create vectors for our data REB for easy and compact analysis

REB.playoffs1 <- c(Curry$REB[Curry$Playoffs == 1])
REB.playoffs1
REB.playoffs0 <- c(Curry$REB[Curry$Playoffs == 0])
REB.playoffs0

#mean

mean(REB.playoffs1)
mean(REB.playoffs0)

#median

median(REB.playoffs1)
median(REB.playoffs0)

#boxplot
boxplot(REB.playoffs1, REB.playoffs0, col = c("royalblue2","red"), names = c("REB.playoffs1","REB.playoffs0"))

#hist
hist(REB.playoffs1)
hist(REB.playoffs0)

#summary of our variables
x <- c(PTS.playoffs1, PTS.playoffs0, REB.playoffs1, REB.playoffs0, min.playoffs1, min.playoffs0)
summary(x)
summary(PTS.playoffs1)
summary(PTS.playoffs0)
summary(REB.playoffs1)
summary(REB.playoffs0)
summary(min.playoffs1)
summary(min.playoffs0)
#wilcoxon test for MIN
# Ho: Median change in PTSs is 0
# two.sided test
#mu = 0 to test is the difference in median is 0
#alt= two.sided to perform a 2 sided test or 2 sided alternative
#paired = T are paired or depended
#apecified confidence interval what we like to have outputed.
wilcox.test(Curry$MIN[Curry$Playoffs == 1], Curry$MIN[Curry$Playoffs == 0], mu=0 )
#or
wilcox.test(min.playoffs1, min.playoffs0, alternative = "two.sided", mu=0, paired = F, conf.int = T, conf.level = 0.95, exact = F)

#wilcoxon test for PTS
# Ho: Median change in PTSs is 0
# two.sided test
#mu = 0 to test is the difference in median is 0
#alt= two.sided to perform a 2 sided test or 2 sided alternative
#paired = T are paired or depended
#apecified confidence interval what we like to have outputed.

wilcox.test(PTS.playoffs1, PTS.playoffs0, mu=0, alt = "two.sided" , paired = F, conf.int = T, conf.level = 0.95, exact = F)
wilcox.test(PTS.playoffs1, PTS.playoffs0, mu=0, alternative = "two.sided")
#or
wilcox.test(Curry$PTS[Curry$Playoffs == 1], Curry$PTS[Curry$Playoffs == 0])

#wilcoxon test for REB
wilcox.test(REB.playoffs1, REB.playoffs0, alternative = "two.sided", mu=0, paired = F, conf.int = T, conf.level = 0.95, exact = F)
#or
wilcox.test(Curry$REB[Curry$Playoffs == 1], Curry$REB[Curry$Playoffs == 0])

#wilcoxon signed-rank test
wilcox.test(PTS.playoffs1, PTS.playoffs0, exact = TRUE)


#Mann-Whitney-Wilcoxon Test to see if the PTS influenced the winning or loss.
boxplot(PTS.playoffs1 ~ Curry$W.L[Curry$Playoffs == 1] )
boxplot(PTS.playoffs0 ~ Curry$W.L[Curry$Playoffs == 0] )
wilcox.test(PTS.playoffs1 ~ Curry$W.L[Curry$Playoffs == 1], alternative = "two.sided", mu=0, conf.int = T, conf.level = 0.95, paired=F, exact = T, correct=T)
wilcox.test(PTS.playoffs0 ~ Curry$W.L[Curry$Playoffs == 0], alternative = "two.sided", mu=0, conf.int = T, conf.level = 0.95, paired=F, exact = T, correct=T)

wilcox.test(Curry$W.L[Curry$Playoffs == 1] ~ PTS.playoffs1, alternative = "two.sided", mu=0, conf.int = T, conf.level = 0.95, paired=F, exact = T, correct=T)

#Kruskal-Wallis tests

kruskal.test(PTS.playoffs1, PTS.playoffs0)

#Perform a t-test and a Wilcoxon test to see if prices differ between the two cities
#t-test 

t.test(PTS.playoffs1, PTS.playoffs0)
t.test(PTS.playoffs1, PTS.playoffs1)
t.test(PTS.playoffs1, PTS.playoffs1)
t.test(PTS.playoffs1, PTS.playoffs1)

#Perform an omnibus test of a difference in distributions between prices in the two cities.
ks.test(PTS.playoffs1, PTS.playoffs0)
ks.test(min.playoffs1, min.playoffs0)
ks.test(REB.playoffs1, REB.playoffs0)

#d perform RMD test
RMD.test <- function(samp1,samp2,direction=c('two.sided','less','greater')[1],nsamp=10000){
  devs1 <- samp1-median(samp1)
  devs2 <- samp2-median(samp2)
  devs <- c(devs1,devs2)
  RMD <- mean(abs(devs1))/mean(abs(devs2))
  if (direction[1]=='two.sided'){
    RMD <- max(1/RMD, RMD)
  }
  RMDperms <- rep(NA,nsamp)
  for (i in 1:nsamp){
    tempdevs <- devs[sample(length(devs),length(devs),replace=FALSE)]
    
    RMDperms[i] <- mean(abs(tempdevs[1:length(devs1)]))/mean(abs(tempdevs[-(1:length(devs1))]))
    if (direction[1]=='two.sided') RMDperms[i] <- max(1/RMDperms[i], RMDperms[i])
  }
  if (direction[1]=='greater') pVal <- mean(RMDperms>=RMD)
  if (direction[1]=='less') pVal <- mean(RMDperms<=RMD)
  if (direction[1]=='two.sided') pVal <- mean(RMDperms>=RMD)
  print(paste("Test statistic:",round(RMD,4)))
  print(paste("Approximate p-value for ",direction[1],":  ",pVal,sep=""))
}

RMD.test(PTS.playoffs1, PTS.playoffs0 ,direction='two.sided')
RMD.test(min.playoffs1, min.playoffs0 ,direction='two.sided')
RMD.test(REB.playoffs1, REB.playoffs0 ,direction='two.sided')
#permTS
permTS(PTS.playoffs1, PTS.playoffs0, conf.level = 0.95, method="exact.mc",control=permControl(nmc=100000,seed=987971))
permTS(min.playoffs1, min.playoffs0, conf.level = 0.95, method="exact.mc",control=permControl(nmc=100000,seed=987971))
#permTS for REB
permTS(REB.playoffs1, REB.playoffs0, conf.level = 0.95, method="exact.mc",control=permControl(nmc=100000,seed=987971))

#Conclusion: Statistically that Stephen did perform as well in the playoffs as he did during the regular season. And all the results
#are graphically and used permutations to came up with decision