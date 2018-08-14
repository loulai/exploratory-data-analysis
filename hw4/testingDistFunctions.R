set.seed(1234)

# create some data
nFeature = 55
dataSet <- matrix(rnorm(3000 * nFeature), ncol = nFeature) # 3000 rows, 55 cols
query <- matrix(rnorm(1 * nFeature), ncol = nFeature) # 1 row, 55 cols

# parameter for distance calculations
K = 10  

install.packages("pdist")
library(pdist)

pdist_wrapper <- function(ref, target, k) {
  distAll <- pdist(X = ref, Y = target)
  iNN <- order(distAll)[1:k]
  return(list(knnIndexDist = matrix(c(iNN, distAll[iNN]^2), nrow = 1), k = k))
  # similar to ann the element knnIndexDist from the list is a vector which
  # contains the indices of the nearest neighbors on position 1 to k and all
  # distances afterwards (position k+1 to 2k)
}
resPDIST <- pdist_wrapper(ref = dataSet, target = query, k = K)