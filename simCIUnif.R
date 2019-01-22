#Peter Rosenquist
#function for simulating the confidence interval for the parameter
#of uniform distribution
simCIUnif <- function(n, theta){
  cdf_of_X_CI_count <- 0 
  mle_CI_count <- 0
  mme_CI_count <- 0
  for(i in 1:1000){
    X <- runif(n, 0, theta)
    cdf_of_X_CI <- max(X)/.95^(1/n)
    mle_CI <- (-log(.95)/n + 1) * max(X)
    mme_CI <- (-1.645/sqrt(3*n) + 1) * (2*mean(X))
    if(theta >= cdf_of_X_CI) cdf_of_X_CI_count = cdf_of_X_CI_count + 1
    if(theta >= mle_CI) mle_CI_count = mle_CI_count + 1
    if(theta >= mme_CI) mme_CI_count = mme_CI_count + 1
  }
  print(c(cdf_of_X_CI_count,
          mle_CI_count,
          mme_CI_count)/1000)
}
levels <- c(5, 10, 30, 50, 100, 1000)
for(i in levels){
  simCIUnif(i, 100)
}
#the CI from the mle and cdf of X seem
#to produce nearly identical values.  The CI
#from the mme is consistently the furthest
#from 0.95. Changing the number of loops in 
#SimCIUnif to 10^6 gave the following results:
#> simCIUnif(100, 100)
#[1] .950038 .950060 .965475
#The stark difference between the first two
#and the CI from the mme is apparent here.
#Perhaps a more precise value than -1.645 would
#yield more accurate results.

