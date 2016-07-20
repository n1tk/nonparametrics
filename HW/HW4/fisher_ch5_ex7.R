#Chapter 5.Ex7

data <- matrix(c(4, 9, 3, 0), 
               nrow = 2, 
               dimnames = list( c("Low", "High"),
                                c("Nearby", "Not Nearby")))

data

fisher <- fisher.test(data)
fisher
