library(dplyr)
library(magrittr)
library(ggplot2)


set.seed(101)

# inputs
m <- 2
myScatterInput <- as.data.frame(matrix(runif(12, 1, 10), ncol=m)) 

points <- myScatterInput
n <- nrow(myScatterInput)
myClusterNum <- 2 #as.integer(runif(1, 2, n))

# 1) random assignment
points <- points %>% 
  mutate(clusterAssignment = rep_len(1:myClusterNum, n))

# 2) compute centroids
centriods <- c()
for(i in 1:myClusterNum){
  centriods <- rbind(centriods, points %>% 
                       filter(clusterAssignment == i) %>% 
                       colMeans())
}

# 3) distance from each data point to centriod 
# by rbinding the centriods to dataframe of points, the dist() function can be used
distanceMatrix <- as.data.frame(as.matrix(dist(pointsCentriods)))

# 4) assign each point to the cluster centriod with minimal distance
distanceMatrix
for(i in 1:n){
  for(k in 1:myClusterNum){
    
  }
}






'''
points %<>% 
  #mutate(sum=rowSums(.[1:5]))
  mutate(mean=rowMeans(.[1:m]))

for(i in 1:n){
  distanceMatix <- lapply()
}

centriods <- points$mean

# get Euclidean distance
as.data.frame(lapply(points, function(x) x - centriods))
points

dist(points, method="euclidean")

# the cluster class
testInput
testInput <- as.data.frame(matrix(c(1,2,3,4,5,6,7,8,9,10), ncol=2)) 
dist(testInput)



ave(c(1,2,3),c(1,2,3), FUN=seq_along )
'''
