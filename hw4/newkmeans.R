library(dplyr)
library(magrittr)
library(ggplot2)

set.seed(101)

# inputs
m <- 3 # m = dimensions
myScatterInput <- as.data.frame(matrix(runif(24, 1, 10), ncol=m)) 

points <- myScatterInput
n <- nrow(myScatterInput)
myClusterNum <- 2 # as.integer(runif(1, 2, n))

#### initial sanity check plot 
ggplot(points) + geom_point(aes(x=V1, y=V2))

# 1) random assignment
points <- points %>% 
  mutate(clusterAssignment = rep_len(1:myClusterNum, n))

###### plot by cluster assignment
ggplot(points) + geom_point(aes(x=V1, y=V2, color=clusterAssignment)) 

swapped = T
while(swapped == T){
  
  # 2) compute centroids
  centroids <- c()
  for(i in 1:myClusterNum){
    centroids <- rbind(centroids, points %>% 
                         filter(clusterAssignment == i) %>% 
                         colMeans())
  }
  
  # 3) distance from each data point to centriod 
  # by rbinding the centriods to dataframe of points, the dist() function can be used
  pointsCentroids <- rbind(points, centroids)
  distanceMatrix <- as.data.frame(as.matrix(dist(pointsCentroids)))
  distanceMatrix
  
  # 4a) calculate closest centriod
  newClusters <- c()
  for(i in 1:n){
    newClusters[i] <- which.min(distanceMatrix[[i]][(n+1):(n+myClusterNum)]) # this is honestly amazingly simple
    # the beauty is that i is set to iterate over only n vectors, so it auto ignores the last (two) cluster matrices
    # then, the subsetting of the final rows (7:8) can be dynamic.
  }
  newClusters
  
  # remove centriods from pointsCentriods
  pointsCentroids <- slice(pointsCentroids, -(n+1):-(n+myClusterNum))
  
  # 4b) assign to centriod
  swapped = F
  for(i in 1:n){
    if(!points['clusterAssignment'][[1]][i] == newClusters[i]) { # use the column called 'clusterAssignment' to compare with newClusters
      print("swap")
      points['clusterAssignment'][[1]][i] = newClusters[i]
      swapped = T # repeat until this will not be reset 
    } else {
      print("noswap")
    }
  }
  print(swapped)
}

###### plot centriods!
ggplot(points) + 
  geom_point(aes(x=V1, y=V2, color=clusterAssignment)) + 
  geom_point(data=as.data.frame(centroids), aes(x=V1, y=V2, color=clusterAssignment), size=6, pch=13)


