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
myClusterNum <- 6

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
myScatterInput <- myScatterInput[1:100,]
myKMeans <- function(myScatterInput, myClusterNum){
  print("hello?")
  myScatterInput <- as.matrix(myScatterInput)
  n <- nrow(myScatterInput) # n = rows
  m <- ncol(myScatterInput) # m = dimensions
  
  #1) random assignment
  myScatterInput <- cbind(myScatterInput, clusterAssignment=rep_len(1:myClusterNum, n))
  #class(myScatterInput)
  #View(myScatterInput)

  swapped = T
  count = 1
  while(swapped == T){
    print("in while")

    # 2) compute centriods
    centroids <- c()
    if(m > 1){ # this is due to some funky subsetting thing
      centroids <- as.matrix(aggregate(list(myScatterInput), by=list(myScatterInput[,'clusterAssignment']), FUN=mean)[2:(m+1)]) # ideally, pass parameter to remove the initial grouping
    } else {
      centroids <- as.matrix(aggregate(list(myScatterInput), by=list(myScatterInput[,'clusterAssignment']), FUN=mean)[2:2]) # ideally, pass parameter to remove the initial grouping
    }
    if(print(nrow(centroids)))
    #centroids
    # 3) distance from each data point to centriod 
    # we use the pdist package, which is like the dist package without the unnecessary computations
    newClusters <- vector(mode="double", length=n)
    for(i in 1:n){
      print("in pdist calc")
      distPointCentroids <- pdist(X = as.matrix(centroids[1:myClusterNum,]), Y=myScatterInput[i, 1:m]) # targets=centriods, query=point
      newClusters[i] <- which.min(distPointCentroids@dist) # we target centriod is the min dist between query and all points in target
    }

    #is.atomic(newClusters) # yass bitch still a vector

    # 4) assign to centroid
    swapped = F
    #class(myScatterInput)
    if(!identical(myScatterInput[,'clusterAssignment'], newClusters)){ # if previous clusters don't equal new clusters, swap and indicate swap occured
      myScatterInput[,'clusterAssignment'] = newClusters
      swapped=T
    }
    #View(myScatterInput)
  }
  
  # 5) plot, if possible
  if(m==2){
    print(ggplot(as.data.frame(myScatterInput)) + 
            geom_point(aes(x=myCol_01, y=myCol_02, color=clusterAssignment)) + 
            geom_point(data=as.data.frame(centroids), aes(x=myCol_01, y=myCol_02), size=6, pch=13) +
            scale_color_continuous(breaks = c(1:myClusterNum)) +
            theme_classic())
  }

  print(count)
  count=count+1
}

# timing
microbenchmark(myFunc=myKMeans(myScatterInput, myClusterNum), times=1)

##########




'''
    as.matrix(centroids[1:myClusterNum,])
    centroids
    myScatterInput[1, 1:m]
    pdist(X = as.matrix(centroids[1:myClusterNum,]), Y=myScatterInput[1, 1:m])@dist
    '''
