# Question 2

data("mtcars")

## Reformat Hp and MPG

mpg <- mtcars$mpg
hp <- mtcars$hp

nums <- seq(1,10)

res <- c()

for (n in nums){
  km <- kmeans(cbind(hp, mpg), n)
  res <- append(res, km$tot.withinss)
}

res <- res/max(res)
res_diff <- diff(res)

threshold <- 0.05
under_threshold <- abs(res_diff) < 0.05

num_clusters <- min(which(under_threshold))

print(paste("The ideal number of clusters to use is", num_clusters))

