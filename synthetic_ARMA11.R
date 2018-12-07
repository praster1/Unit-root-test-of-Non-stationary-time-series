### ARMA(1,1) process simulation
synthetic_ARMA11 = function(initVal = 0, coefAR=-0.45, coefMA=-0.45, mean = 0, sd = 1, length = 100)
{
    X <- initVal
    Z <- rnorm(length, mean = mean, sd = sd)

    for (i in 2:length(Z)) 
    {
        X[i] <- coefAR*X[i-1] + Z[i] + coefMA*Z[i-1]
    }

    return(X)
}
