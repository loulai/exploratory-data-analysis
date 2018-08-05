library(dplyr)
library(ggplot2)


myGamma <- rgamma(100000, shape=2, rate=1.5)

myExp <- rexp(100000, rate=3/4)

ggplot() + 
  geom_histogram(aes(myGamma)) +
  geom_histogram(aes(myExp), fill="yellow", alpha=0.5)+
  labs(title="Gamma Distribution")

max(myGamma) # 9.511353

myInv <- c()

for(i in 1:length(myExp)){
  myInv[i] <- inverseExp(myExp[i])
  myGamma <- dgamma(myInv[i], shape=2, rate=1.5)
}

inverseExp <- function(y) {
  return((-4/3) * log((4*y)/3))
}

myExp[1]

inverseExp(myExp[1])

3/4 * exp(-3/4 * -0.3978236)

dexp()
