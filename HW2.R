## Peter Rosenquist
## Problem 9
values<-rexp(3000, rate = 1)
matrix<-matrix(values, nrow = 1000)
theta <- as.data.frame(x = matrix(rep(NA, 4000), ncol = 4))
colnames(theta) <- c("theta_one",
                     "theta_two",
                     "theta_thr",
                     "theta_fiv")
for (i in 1:1000) {
  theta[i, 1:4] <- c(matrix[i, 1],
                     sum(matrix[i, 1:2]) / 2,
                     sum(matrix[i, 1], 2*matrix[i, 2]) / 3,
                     sum(matrix[i, 1:3]) / 3)
}
summary(theta)
var(theta[1:4])
## theta_five has the lowest variance, this is what we found in problem two.