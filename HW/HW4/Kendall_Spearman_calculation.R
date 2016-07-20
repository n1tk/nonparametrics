#hw4 

#chapter5Ex5

#build vestors with data
moisture <- c(355, 370, 380, 380, 380, 400, 400, 415, 415, 415, 415, 430, 440, 440, 470)
shrew <- c(0.5, 2, 2, 0.5, 3, 0, 4, 0.5, 2, 4, 4, 1, 2, 5, 7)

#test for significant association between our variables using computed spearman correlation 
spearman.test <- cor.test(moisture, shrew, method = "spearman", exact = T)
spearman.test

#test for significant association between our variables using computed Kendall tau 

kendall.test <- cor.test(moisture, shrew, method = "kendall", exact = T)
kendall.test


