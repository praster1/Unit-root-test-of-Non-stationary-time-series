require(forecast)
require(tsoutliers)



dataLen = 500
### ARMA(1,1) process simulation

# purely random process with mean 0 and standard deviation 1.5
Z <- rnorm(dataLen, mean = 0, sd = 1.5)

# Process
X <- rnorm(1)

for (i in 2:length(Z)) 
{
    X[i] <- X[i-1] + Z[i] + Z[i-1]
}


X[100:length(X)] = X[100:length(X)] - 100

X[255] = 50

X[300:325] = X[300:325] + rnorm(dataLen, mean = -50, sd = 3.5)



plot(X, type="l")

# auto.arima(X)
outliers = tso(as.ts(X))
outliers

plot(outliers)