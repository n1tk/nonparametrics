#Spark interface in R

install.packages("devtools")
devtools::install_github("rstudio/sparklyr")


library(sparklyr)
spark_install(version = "1.6.1")

library(sparklyr)
library(dplyr)
sc <- spark_connect(master = "local")