#Peter Rosenquist
simMLEBeta <- function(n, theta){
  theta_hat_set <- NULL
  theta_hat_minus_theta_set <- NULL
  for(i in 1:1000){
    data <- rbeta(n, theta, 1)
    log_data <- log(data)
    theta_hat <- -n/sum(log_data)
    theta_hat_minus_theta <- sqrt(n)*(theta_hat-theta)
    theta_hat_set <- c(theta_hat, 
                       theta_hat)
    theta_hat_minus_theta_set <- c(theta_hat_minus_theta_set, 
                                   theta_hat_minus_theta)
  }
  mean_theta_hat <- mean(theta_hat_set)
  variance <- var(theta_hat_minus_theta_set)
  results <- c(mean_theta_hat, variance)
  return(results)
}
#visualizing results
n_levels <- c(10, 50, 100, 500, 1000, 10000)
par(mfrow=c(3,2))
for(i in n_levels){
  theta <- 1
  results <- simMLEBeta(i, theta)
  barplot(results,
          names.arg = c("average theta hat", "variance of theta hat-theta"),
          ylim = c(0, theta^2 + theta),
          main = paste0("theta = ", theta, ", n = ", i))
  abline(h = theta, lty = 1)
  abline(h = theta^2, lty = 2)
  text(.70, .5, round(results[1], digits = 3))
  text(1.9, .5, round(results[2], digits = 3))
}
#solid line shows value of theta
#dashed line shows aymptotic variance
