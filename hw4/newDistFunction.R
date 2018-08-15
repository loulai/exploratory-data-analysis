library(dplyr)
library(magrittr)
library(ggplot2)
install.packages("rdist")
library(pdist)
library(rdist)
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
myClusterNum <- 3

# TEST DATA 4
set.seed(104)
myScatterInput <- data_frame(myCol_01 = c(rnorm(3000, 20, 20), rnorm(5000, -4, 2), rnorm(2000, 40, 2)), myCol_02 = runif(10000, -5, 20))
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
#myScatterInput <- myScatterInput[1:1000,]

myKMeans <- function(myScatterInputOriginal, myClusterNum, numIter){
  totalDistances <- Inf
  
  for(i in 1:numIter){
    myScatterInput <- as.matrix(myScatterInputOriginal)
    n <- nrow(myScatterInput) # n = rows
    m <- ncol(myScatterInput) # m = dimensions
    
    # 1) random assignment
    myScatterInput <- cbind(myScatterInput, clusterAssignment=rep_len(1:myClusterNum, n))
    
    swapped = T
    while(swapped == T){
      # 2) compute centriods
      centroids <- matrix(data=NA, nrow=myClusterNum, ncol=m)
      if(m > 1){ # this is due to some funky subsetting thing
        centroids <- as.matrix(aggregate(list(myScatterInput), by=list(myScatterInput[,'clusterAssignment']), FUN=mean)[2:(m+1)]) # ideally, pass parameter to remove the initial grouping
      } else {
        centroids <- as.matrix(aggregate(list(myScatterInput), by=list(myScatterInput[,'clusterAssignment']), FUN=mean)[2:2]) 
      }
      
      #  dplyr - summarise_all(funs(mean))
      
      myClusterNum <- nrow(centroids) # this is here in case empty clusters arise, we need to change number of centroids
      
      # 3) distance from each data point to centriod 
      # we use the wonderful cdist package, which calculates difference between two matrices
      newClusters <- vector(mode="double", length=n)
      for(i in 1:n){
        distPointCentroids <- rdist::cdist(X = centroids[1:myClusterNum,,drop=F], Y=myScatterInput[i, 1:m, drop=F]) # targets=centriods, query=point
        newClusters[i] <- which.min(distPointCentroids) # we target centriod is the min dist between query and all points in target
      }
      
      #is.atomic(newClusters) # yass bitch still a vector
      
      # 4) assign to centroid
      swapped = F
      if(!identical(myScatterInput[,'clusterAssignment'], newClusters)){ # if previous clusters don't equal new clusters, swap and indicate swap occured
        myScatterInput[,'clusterAssignment'] = newClusters
        swapped=T
      }
    }
    
    # 5) get sum of distances
    distances <- vector(mode="double", length=myClusterNum)
    for(i in 1:myClusterNum){
      clusterOfPoints <- as.data.frame(myScatterInput) %>% filter(clusterAssignment == i) %>% select(-clusterAssignment)
      distances[i] <- sum(rdist::cdist(X = centroids[1:myClusterNum,,drop=F], Y=as.matrix(clusterOfPoints)))
    }
    newTotalDistances <- sum(distances)
  }
  
  # 6) if 2D, plot
  if(m==2){
    print(ggplot(as.data.frame(myScatterInput)) + 
            geom_point(aes(x=myCol_01, y=myCol_02, color=clusterAssignment)) + 
            geom_point(data=as.data.frame(centroids), aes(x=myCol_01, y=myCol_02), size=6, pch=13) +
            scale_color_continuous(breaks = c(1:myClusterNum)) +
            theme_classic())
  }
  
  # 7) assign lowest distance
  if(newTotalDistances < totalDistances){
    totalDistances <- newTotalDistances
  }
  return(totalDistances)
}


# timing
microbenchmark(myFunc=myKMeans(myScatterInput, myClusterNum, 2), times=1)

myKMeans(myScatterInput, myClusterNum, 2)

##########

'''
    as.matrix(centroids[1:myClusterNum,])
    centroids
    myScatterInput[1, 1:m]
    pdist(X = as.matrix(centroids[1:myClusterNum,]), Y=myScatterInput[1, 1:m])@dist
   
 X = centroids[1:myClusterNum,,drop=F]
    Y=myScatterInput[1, 1:m, drop=F]
    rdist::cdist(X = centroids[1:myClusterNum,,drop=F], Y=myScatterInput[1, 1:m, drop=F])
    distPointCentroids <- rdist::cdist(X = centroids[1:myClusterNum,,drop=F], Y=myScatterInput[1, 1:m, drop=F]) 
    which.min(distPointCentroids)
    ncol(myScatterInput[1, 1:2])
    View(myScatterInput)
    myScatterInput[1, 1:2]
    centroids[1:3,]
    cdist
    is.matrix(centroids[1:myClusterNum,, drop=F])
    is.matrix(myScatterInput[2, 1:m, drop=F])

'''

