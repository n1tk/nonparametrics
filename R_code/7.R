#Curry project for non-parametrics
#loading data
Curry <- read.csv("/Users/sbuciuma/Desktop/School/summer/nonparametric_statistics/Project/CurryComp.csv")

#selecting data in offseason and in season

Curry <- Curry[Curry[,2]==1,]
Curry
#wilcoxon test
#MIN
wilcox.test(Curry$MIN[Curry$Playoffs == 1], Curry$MIN[Curry$Playoffs == 0])

#PTS
wilcox.test(Curry$PTS[Curry$Playoffs == 1], Curry$PTS[Curry$Playoffs == 0])

#FGM 

wilcox.test(Curry$FGM[Curry$Playoffs == 1], Curry$FGM[Curry$Playoffs == 0])

#FGA 
wilcox.test(Curry$FGA[Curry$Playoffs == 1], Curry$FGA[Curry$Playoffs == 0])

#FGperc 
wilcox.test(Curry$FGperc[Curry$Playoffs == 1], Curry$FGperc[Curry$Playoffs == 0])

#ThreePM 
wilcox.test(Curry$ThreePM[Curry$Playoffs == 1], Curry$ThreePM[Curry$Playoffs == 0])

#ThreePA 
wilcox.test(Curry$ThreePA[Curry$Playoffs == 1], Curry$ThreePA[Curry$Playoffs == 0])

#ThreePerc
wilcox.test(Curry$ThreePerc[Curry$Playoffs == 1], Curry$ThreePerc[Curry$Playoffs == 0])

# FTM 
wilcox.test(Curry$FTM[Curry$Playoffs == 1], Curry$FTM[Curry$Playoffs == 0])

#FTA 
wilcox.test(Curry$FTA[Curry$Playoffs == 1], Curry$FTA[Curry$Playoffs == 0])

#FTperc 
wilcox.test(Curry$FTperc[Curry$Playoffs == 1], Curry$FTperc[Curry$Playoffs == 0])

#OREB 
wilcox.test(Curry$OREB[Curry$Playoffs == 1], Curry$OREB[Curry$Playoffs == 0])

#DREB 
wilcox.test(Curry$DREB[Curry$Playoffs == 1], Curry$DREB[Curry$Playoffs == 0])

#REB 
wilcox.test(Curry$REB[Curry$Playoffs == 1], Curry$REB[Curry$Playoffs == 0])

#AST 
wilcox.test(Curry$AST[Curry$Playoffs == 1], Curry$AST[Curry$Playoffs == 0])

#STL 
wilcox.test(Curry$STL[Curry$Playoffs == 1], Curry$STL[Curry$Playoffs == 0])

#BLK 
wilcox.test(Curry$BLK[Curry$Playoffs == 1], Curry$BLK[Curry$Playoffs == 0])

#TOV 
wilcox.test(Curry$TOV[Curry$Playoffs == 1], Curry$TOV[Curry$Playoffs == 0])

#PF 
wilcox.test(Curry$PF[Curry$Playoffs == 1], Curry$PF[Curry$Playoffs == 0])

#PlusMinus
wilcox.test(Curry$PlusMinus[Curry$Playoffs == 1], Curry$PlusMinus[Curry$Playoffs == 0])


#Mann-Whitney-Wilcoxon Test
wilcox.test(Curry$PlusMinus[Curry$Playoffs == 0] ~ Curry$MIN[Curry$Playoffs == 0])

#kruskal.test(Ozone ~ Month, data = airquality) 

kruskal.test( W.L ~ PlusMinus, data=Curry)
#in season
kruskal.test( Curry$W.L[Curry$Playoffs == 1] ~ PlusMinus[Curry$Playoffs == 1], data=Curry)
x <- (Curry$PlusMinus[Curry$Playoffs == 1])
x
y <- (Curry$PlusMinus[Curry$Playoffs == 0])
hist(y)
mean(y)
median(y)
hist(x)
mean(x)
median(x)
#offseason


#OREB DREB #REB is total so we can compare from on season with off season
kruskal.test( Curry$W.L[Curry$Playoffs == 0] ~ PlusMinus[Curry$Playoffs == 0], data=Curry)
