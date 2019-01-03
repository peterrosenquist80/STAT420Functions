#Peter Rosenquist 
simMinUnif <- function(n, B){
  Xn <- NULL
  for(i in 1:B){
    data <- runif(n, 0, 1)
    minimum <- min(data)
    Xn <- c(Xn, minimum * n)
  }
  plot(ecdf(Xn), 
       xlim = c(0, 6), 
       ylim = c(0, 1),
       col = "red",
       lwd = 3,
       lty = 1,
       ylab = "",
       main = paste(n, "samples"))
  par(new=TRUE)
  curve(1 - exp(-x),
        xlim = c(0, 6),
        ylim = c(0,1), 
        col = "blue", 
        lwd = 3,
        lty = 2,
        ylab = "",
        from = 0, to = 10
        )
}
n_levels <- c(3,5,10,50,100,1000)
par(mfrow=c(3,2))
for(i in n_levels){
  simMinUnif(i, 1000)
}
#If we take the limit of the cdf of Xn, we see that it converges in
#distribution to the cdf of X, i.e. 1 - exp(-x) 
#      lim n-> inf (1 - (1 - F(x/n))^n) = 1 - exp(-x)
#We see pretty clear convergence as n->inf.  From approximately n >= 50, 
#the empirical cdf and the cdf of X ~ Exp(1) are nearly identical. 
#Bringing in an additional 2 orders of magnitude we see...
simMinUnif(n = 100000, 1000)
