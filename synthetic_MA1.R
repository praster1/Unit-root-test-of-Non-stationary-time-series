### Simulation of a first order MA(1)
synthetic_MA1 = function(coef=-0.45, mean = 0, sd = 1, length = 100)
{
    X <- c()
    Z <- rnorm(length, mean = mean, sd = sd)
    
    for (i in 2:length(Z)) 
    {
        X[i] <- Z[i] - coef*Z[i-1]
    }
    
    return(X)
}