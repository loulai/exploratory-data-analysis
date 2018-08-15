library(dplyr)
library(magrittr)
library(ggplot2)
library(pdist)
library(microbenchmark)

############

# TEST DATA 3
set.seed(103)
myScatterInput <- data_frame(myCol_01 = runif(10000, -5, 20), 
                             myCol_02 = c(rnorm(3000, 20, 5), rnorm(5000, -4, 2), rnorm(2000, 40, 2)))
#myScatterInput <- myScatterInput[1:100,]
myClusterNum <- 3

# TEST DATA 4
set.seed(104)
myScatterInput <- data_frame(myCol_01 = c(rnorm(3000, 20, 20), rnorm(5000, -4, 2), rnorm(2000, 40, 2)), 
                             myCol_02 = runif(10000, -5, 20))
myScatterInput <- as.matrix(myScatterInput[1:10,])
myClusterNum <- 2
View(myScatterInput)


#############

myKMeans <- function(myScatterInput, myClusterNum){
  n <- nrow(myScatterInput) # n = num vectors
  m <- ncol(myScatterInput) # m = dimensions
  
  #1) random assignment
  myScatterInput <- cbind(myScatterInput, clusterAssignment=rep_len(1:myClusterNum, n))
  #class(myScatterInput)

  swapped = T
  while(swapped == T){
    
    # 2) compute centriods
    centroids <- rep(NA, myClusterNum)
    centroids <- as.matrix(aggregate(list(myScatterInput), by=list(myScatterInput[,'clusterAssignment']), FUN=mean)[2:(m+1)]) # ideally, pass parameter to remove the initial grouping
    #View(centroids)
              
    # 3) distance from each data point to centriod 
    # we use the pdist package, which is like the dist package without the unnecessary computations
    newClusters <- rep(NA, n)
    newClusters <- vector(mode="double", length=n)
    for(i in 1:n){
      distPointCentroids <- pdist(X = centroids[1:m,], Y=myScatterInput[i, 1:m]) # targets=centriods, query=point.
      newClusters[i] <- which.min(distPointCentroids@dist) # we target centriod is the min dist between query and all points in target
      # convert to double for comparison later
    }
    
    #is.atomic(newClusters) # yass bitch still a vector
    
    # 4) assign to centroid
    swapped = F
    #class(myScatterInput)
    if(!identical(myScatterInput[,'clusterAssignment'], newClusters)){ # if previous clusters don't equal new clusters, swap and indicate swap occured
      myScatterInput[,'clusterAssignment'] = newClusters
      swapped=T
    }
  }
  
  # 5) plot
  if(m==2){
    print(ggplot(as.data.frame(myScatterInput)) + 
            geom_point(aes(x=myCol_01, y=myCol_02, color=clusterAssignment)) + 
            geom_point(data=as.data.frame(centroids), aes(x=myCol_01, y=myCol_02), size=6, pch=13) +
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




