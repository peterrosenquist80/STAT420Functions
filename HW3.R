#Peter Rosenquist
simMMEGamme <- function(n, alpha, beta){
  beta_hat <- NULL
  alpha_hat <- NULL
  for(i in 1:1000){
    data_set <- rgamma(n=n, shape = alpha, rate = beta)
    data_set_squared <- data_set^2
    sample_mean <- mean(data_set)
    data_set_squared_mean <- mean(data_set_squared)
    sample_beta_hat <- sample_mean / (data_set_squared_mean - sample_mean^2)
    sample_alpha_hat <- sample_mean^2 / (data_set_squared_mean - sample_mean^2)
    beta_hat <- c(beta_hat, sample_beta_hat)
    alpha_hat <- c(alpha_hat, sample_alpha_hat)
  }
  print(paste("alpha_hat: ", as.character(mean(alpha_hat)), "|| true alpha: ", as.character(alpha)))
  print(paste("beta_hat : ", as.character(mean(beta_hat)), "|| true beta : ", as.character(beta)))
}
simMMEGamme(10, 1, 1)
simMMEGamme(50, 1, 1)
simMMEGamme(100, 1, 1)
simMMEGamme(500, 1, 1)
simMMEGamme(1000, 1, 1)
#As was to be expected, as n-->inf,
#we see alpha_hat and beta_hat converge 
#to their true values.

