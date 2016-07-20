#genimic week 1
install.packages("rafalib")
library(rafalib)

install.packages("swirl")
library(swirl)
version
v <- c(2.23, 3.45, 1.87, 2.11, 7.33, 18.34, 19.23)
mean(v)



my_vector <- vector("numeric", 25L)
for (i in 1:25){
  v = i^2
  my_vector[i] <- v
}
print(my_vector)
sumvect <- sum(my_vector)
sumvect


data("cars")
cars
class(cars)
dim(cars)
nrow(cars)
colnames(cars)[2]
averagedistance <- cars[2]
rbind(averagedistance)
lapply(averagedistance, mean, na.rm = TRUE)
which(cars$dist == 85)


library(twitteR)
# Register an application at https://aps.twitter.com
setup_twitter_oauth(
  consumer_key = "X38S4eTmAc9OflRxlx2f48Q3Z", consumer_secret = "C9MczgTnc6FUHJCcsCER4PwloRRSnaiL7WaRIojAoYkQcmZyjq",
  access_token = "19675099-S3fviiHDna9ngxjUMFatu1uhV4Sx1yJDUGtAyXX1v", access_secret = "g3i540XFtPM6UEmE4nxA0DEvAmjfX2X6gZKtD9A3bTYBd")
data <- searchTwitter('@ML')
data

install.packages("sparklyr")
library(sparklyr)
library(dplyr)

# Install sparklyr
install.packages("devtools")
devtools::install_github("rstudio/sparklyr")
spark_install()

# Connect and load a dataset to Spark
sc <- spark_connect(master = "local")
dataset <- spark_read_parquet(sc, "iris", "data/iris.parquet")

# Sample the dataset back to R
sampled <- dataset %>% sample_n(size = 5) %>% collect
View(sampled)

spark_disconnect(sc)