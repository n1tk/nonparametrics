

m <- c(70, 69, 65, 64, 66, 65, 64, 66, 60, 70, 66)

d <- c(67, 64, 62, 64, 69, 70, 65, 66, 63, 74, 60)

hist(m)

hist(d)

plot(m, d)
#compute Spearman's correlation, Kendall's Tau-b
cor.test(m, d, method="spearman", exact = TRUE) 
cor.test(m, d, method="kendall", exact = TRUE) 

#Test for stastistical significance of spearman's correlation, Kendalls Tau-B




#comopute person correlation

person <- cor(d, m, use="complete.obs")
person
summary(person)

#build regression

RegModel.2 <- lm(d~m)
summary(RegModel.2)


x1 <- c(3, -12, 6, 3, 6)
x2 <- c(4.7, 18, 18, 6, 25)

cor.test(x1, x2, method="kendall", exact = TRUE) 

cor.test(x1, x2, method="kendall", exact = TRUE) 

