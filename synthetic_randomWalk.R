##### Random Walk process simulation
synthetic_randomWalk = function(initVal = 0, mean = 0, sd = 1, length = 100)
{
    X = initVal
    Z <- rnorm(length, mean = mean, sd = sd)

    for (i in 2:length(Z))
    {  
        X[i] <- X[i-1] + Z[i]
    }

    return(X)
}