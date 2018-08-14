set.seed(1234)

# create some data
nFeature = 5
dataSet <- matrix(rnorm(300 * nFeature), ncol = nFeature) # 300 rows, 5 cols
query <- matrix(rnorm(1 * nFeature), ncol = nFeature) # 1 row, 5 cols

# parameter for distance calculations
K = 4  

library(pdist)

pdist_wrapper <- function(ref, target, k) {
  distAll <- pdist(X = ref, Y = target)
  which.min(distAll@dist)
  iNN <- order(distAll@dist)[1:4]
  return(list(knnIndexDist = matrix(c(iNN, distAll[iNN]^2), nrow = 1), k = k))
  # the element knnIndexDist from the list is a vector which
  # contains the indices of the nearest neighbors on position 1 to k and all
  # distances afterwards (position k+1 to 2k)
}
resPDIST <- pdist_wrapper(ref = dataSet, target = query, k = K)

distAll <- pdist(dataSet, query)
slot(distAll)
order(distAll@dist)
?order

class(distAll@dist)

View(distAll@dist[177])
distAll@dist[1]
iNN
