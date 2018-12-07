##### Simulation of a random time series
synthetic_pureRP = function(constMean = 2, mean = 0, sd = 1, length = 100)
{
    # purely random process with mean 0 and standard deviation 1.5
    eps <- rnorm(length, mean = mean, sd = sd)
    mu <- constMean     # the constant mean
    # The process
    X_t <- mu + eps

    return(X_t)
}
