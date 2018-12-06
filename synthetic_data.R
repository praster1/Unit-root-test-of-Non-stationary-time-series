##### Simulation of a random time series
# purely random process with mean 0 and standard deviation 1.5
eps <- rnorm(100, mean = 0, sd = 1)
mu <- 2 # the constant mean
# The process
X_t <- mu + eps

# plotting the time series
ts.plot(X_t, main = "Example of (random) stationary time series", ylab = expression(X[t]))

acf(X_t, main = "Auto-covariance function of X")





##### Random Walk process simulation
# seed X_0 = 0
X <- 0

# purely random process with mean 0 and standard deviation 1.5
Z <- rnorm(100, mean = 0.5, sd = 1.5)

# the process
for (i in 2:length(Z)){
  X[i] <- X[i-1] + Z[i]
}

# process plotting
ts.plot(X, main = "Random walk process")


### differencing and plotting of the random walk process
ts.plot(diff(X))





##### Moving Average of order q: MA(q)
### Simulation of a first order MA(1)

# purely random process with mean 0 and standard deviation 1.5 (arbitrary choice)
Z <- rnorm(100, mean = 0, sd = 1.5)

# process simulation
X <- c()
for (i in 2:length(Z)) {
  X[i] <- Z[i] - 0.45*Z[i-1]
}

# process plotting
ts.plot(X, main = "Moving Average or order 1 process")





##### Auto-Regression of order p: AR(p)
### Simulating an AR(1)

# constant alpha
alpha = 0.5

# purely random process with mean 0 and standard deviation 1.5
Z <- rnorm(100, mean = 0, sd = 1.5)

# seed
X <- rnorm(1)

# the process
for (i in 2:length(Z)) {
  X[i] <- 0.7*X[i-1]+Z[i]
}

# process plotting
ts.plot(X)





##### Autoregressive moving average process: ARMA(p,q)
### ARMA(1,1) process simulation

# purely random process with mean 0 and standard deviation 1.5
Z <- rnorm(100, mean = 0, sd = 1.5)

# Process
X <- rnorm(1)

for (i in 2:length(Z)) 
{
    X[i] <- 0.35*X[i-1] + Z[i] + 0.4*Z[i-1]
}

# process plotting
ts.plot(X, main = "ARMA(1,1) process")

# ACF et PACF
par(mfrow = c(1,2))
acf(X); pacf(X)





##### SARIMA(p,d,q)(P,D,Q) process
# R packages to be used
library(forecast)
library(TSA)
Example 1:
# Data from TSA package
data("co2")
data("boardings")

# fitting
fit <- auto.arima(co2)

# Time series plot
plot(fc <- forecast(fit, h = 15))



### Example 2
data("boardings")

# fitting
fit2 <- auto.arima(boardings[,"log.price"])

# forecasting
plot(fc2 <- forecast(fit2, h = 15))