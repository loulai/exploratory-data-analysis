library(dplyr)
library(magrittr)
library(ggplot2)
library(pdist)
library(microbenchmark)

############
# TEST DATA 1
set.seed(101)
myScatterInput <- data_frame(myCol_01 = runif(100000, -1, 1))
myClusterNum <- 2

# TEST DATA 2
set.seed(102)
myScatterInput <- data_frame(myCol_01 = runif(100000, -1, 1))
myClusterNum <- 4

# TEST DATA 3
set.seed(103)
myScatterInput <- data_frame(myCol_01 = runif(10000, -5, 20), myCol_02 = c(rnorm(3000, 20, 5), rnorm(5000, -4, 2), rnorm(2000, 40, 2)))
#myScatterInput <- myScatterInput[1:100,]
myClusterNum <- 3

# TEST DATA 4
set.seed(104)
myScatterInput <- data_frame(myCol_01 = c(rnorm(3000, 20, 20), rnorm(5000, -4, 2), rnorm(2000, 40, 2)), 
                             myCol_02 = runif(10000, -5, 20))
myScatterInput <- myScatterInput[1:1000,]
myClusterNum <- 6

# TEST DATA 5
set.seed(105)
myScatterInput <- data_frame(myCol_01 = c(rnorm(3000, 20, 20), rnorm(5000, -4, 2), rnorm(2000, 40, 2)), 
                             myCol_02 = runif(10000, -5, 20),
                             myCol_03 = runif(10000, -100, 100),
                             myCol_04 = c(runif(4000, -5, 20), rnorm(6000)),
                             myCol_05 = runif(10000, -10, 200),
                             myCol_06 = rnorm(10000, -300, 1000),
                             myCol_07 = rnorm(10000, -1000000, 1000000),
                             myCol_08 = rnorm(10000, 30, 2))
myClusterNum <- 3

# TEST DATA 6
set.seed(106)
myScatterInput <- data_frame(myCol_01 = c(rnorm(3000, 20, 20), rnorm(5000, -4, 2), rnorm(2000, 40, 2)), 
                             myCol_02 = runif(10000, -5, 20),
                             myCol_03 = runif(10000, -100, 100),
                             myCol_04 = c(runif(4000, -5, 20), rnorm(6000)),
                             myCol_05 = runif(10000, -10, 200),
                             myCol_06 = rnorm(10000, -300, 1000),
                             myCol_07 = rnorm(10000, -1000000, 1000000),
                             myCol_08 = rnorm(10000, 30, 2))
myClusterNum <- 12

#############

myKMeans <- function(myScatterInput, myClusterNum){
  n <- nrow(myScatterInput) # n = num vectors
  m <- ncol(myScatterInput) # m = dimensions
  
  # 1) random assignment
  myScatterInput <- myScatterInput %>% 
    mutate(clusterAssignment = rep_len(1:myClusterNum, n))
  
  swapped = T
  while(swapped == T){
    
    # 2) compute centroids
    centroids <- c()
    for(i in 1:myClusterNum){
      centroids <- as.data.frame(rbind(centroids, myScatterInput %>% 
                           filter(clusterAssignment == i) %>% 
                           colMeans()))
    }
    
    # 3) distance from each data point to centriod 
    # we use the pdist package, which is like the dist package without the unnecessary computations
    # n = rows
    newClusters <- c()
    for(i in 1:n){
      distPointCentriods <- pdist(X = centroids[1:m,], Y=myScatterInput[i, 1:m]) # targets=centriods, query=point.
      newClusters[1] <- which.min(distPointCentriods@dist) # we target centriod is the min dist between query and all points in target
    }
    newClusters
    pdist(X = centroids[1:m,], Y=myScatterInput[10, 1:m])
    which.min(pdist(X = centroids[1:m,], Y=myScatterInput[10, 1:m])@dist)
    
    # 4b) assign to centroid
    swapped = F
    if(!identical(myScatterInput['clusterAssignment'][[1]], newClusters)){ # if previous clusters don't equal new clusters, swap and indicate swap occured
      myScatterInput['clusterAssignment'] = newClusters
      swapped=T
    }
    print(swapped)
  }
  
  # 5) plot
  if(m==2){
    print(ggplot(myScatterInput) + 
      geom_point(aes(x=myCol_01, y=myCol_02, color=clusterAssignment)) + 
      geom_point(data=as.data.frame(centroids), aes(x=myCol_01, y=myCol_02, color=clusterAssignment), size=6, pch=13) +
      scale_color_continuous(breaks = c(1:myClusterNum)) +
      theme_classic())
  }
}

# timing
microbenchmark(myFunc=myKMeans(myScatterInput, myClusterNum), times=1)

##########

# points
if(m == 2){
  ggplot(myScatterInput) + geom_point(aes(x=V1, y=V2)) + theme_classic()
}

# points + randomized assignment
if(m == 2){
  ggplot(myScatterInput) + geom_point(aes(x=myCol_01, y=myCol_02, color=clusterAssignment)) +
    scale_color_continuous(breaks = c(1:myClusterNum)) +
    theme_classic()
}




