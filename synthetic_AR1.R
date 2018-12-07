### Simulating an AR(1)
synthetic_AR1 = function(initVal = 0, coef=-0.45, mean = 0, sd = 1, length = 100)
{
    X <- initVal
    Z <- rnorm(length, mean = mean, sd = sd)

    for (i in 2:length(Z)) 
    {
        X[i] <- coef*X[i-1]+Z[i]
    }

    return(X)
}
