rm(list = ls())



setwd("/home/lv999/Dropbox/Github/Unit-root-test-of-Non-stationary-time-series")



source("seqDatetime_byEnddate.R")   # 시작일(startDate)부터 종료일(endDate) 직전까지 날짜 벡터 구하기      
#seqDatetime_byEnddate(startDate="2000-01-01", endDate="2000-01-04", split=7)
source("seqDatetime_byLength.R")    # 시작일(startDate)부터 길이(length)만큼 날짜 벡터 구하기
# seqDatetime_byLength(startDate="2000-01-04", length=6, split=7)


source("getUniqVec.R")  # datetime의 index를 구하는 함수       # getUniqVec(datetimeVec, index="YYYYMMDDHHMMDD")
source("getCalcVec.R")  # split의 시작값, 종료값, 평균값, 중앙값 등을 구하는 함수       # getCalcVec(dataVec, datetimeIndexVec, calc="last")





dataLen = 96*365*3      # 105120






##### Simulation of a random time series
par(mfrow = c(6, 1))

source("synthetic_pureRP.R")
dataVec = synthetic_pureRP(constMean = 0, mean = 0, sd = 1, length = dataLen)
plot(dataVec, main = "Stationary time series", ylab = expression(X[t]), type="l")



# outlier
   # - Additive Outliers (AO): 시계열 자료 중 한 개가 지나치게 크거나 작은 값을 갖는 관측치
   # - innovational outliers(IO): 이후의 시계열이 전혀 다른 형태를 보이는 특징을 가지는 이상치
   # - level shift outliers(LSO): 이후에 시계열 전체가 위나 아래로 이동한 경우
   # - temporary chance outlers(TCO): 일시적으로 시계열이 이동하였으나 지수적으로 빠르게 원래의 상태로 돌아가는 시계열의 형태
   # - seasonal level shift(SLS): 계절적으로 특정한 시기에 나타나는 이상치
   # - variance change(VC): 이후에 시간이 흐를 수록 변동폭이 커지거나 작아지는 이상치

outlierIndexs = sort(ceiling(runif(5, 1,dataLen)))

outlierIndexs_IO = outlierIndexs[1]
dataVec[outlierIndexs_IO:dataLen] = synthetic_pureRP(constMean = 0, mean = 0, sd = runif(1, 0, 3), length = length(outlierIndexs_IO:dataLen))
plot(dataVec, main = "Stationary time series with IO", ylab = expression(X[t]), type="l")
points(outlierIndexs_IO, dataVec[outlierIndexs_IO], col="red", lwd=5)

outlierIndexs_LSO = outlierIndexs[2]
dataVec[outlierIndexs_LSO:dataLen] = synthetic_pureRP(constMean = 0, mean = runif(1, -3, 3), sd = runif(1, 0, 3), length = length(outlierIndexs_LSO:dataLen))
plot(dataVec, main = "Stationary time series with LSO", ylab = expression(X[t]), type="l")
points(outlierIndexs_LSO, dataVec[outlierIndexs_LSO], col="red", lwd=5)

outlierIndexs_TCO = outlierIndexs[3:4]
dataVec[outlierIndexs_TCO[1]:outlierIndexs_TCO[2]] = dataVec[outlierIndexs_TCO[1]:outlierIndexs_TCO[2]] + runif(1, -3, 3)
plot(dataVec, main = "Stationary time series with TCO", ylab = expression(X[t]), type="l")
points(outlierIndexs_TCO, dataVec[outlierIndexs_TCO], col="red", lwd=5)

outlierIndexs_VC = outlierIndexs[5]
addVarianceVec = seq(1, 3, length=length(outlierIndexs_VC:dataLen))
dataVec[outlierIndexs_VC:dataLen] = dataVec[outlierIndexs_VC:dataLen] * addVarianceVec
plot(dataVec, main = "Stationary time series with VC", ylab = expression(X[t]), type="l")
points(outlierIndexs_VC, dataVec[outlierIndexs_VC], col="red", lwd=5)

outlierIndexs_AO = runif(10, 1, dataLen)
dataVec[outlierIndexs_AO] = dataVec[outlierIndexs_AO] + 10 * runif(10, -5, 5)
plot(dataVec, main = "Stationary time series with AO", ylab = expression(X[t]), type="l")
points(outlierIndexs_AO, dataVec[outlierIndexs_AO], col="red", lwd=5)





##### Random Walk process simulation
# par(mfrow = c(6, 1))

# source("synthetic_randomWalk.R")
# dataVec = synthetic_randomWalk(initVal = 1000, mean = 0, sd = 1, length = dataLen)
# ts.plot(dataVec, main = "Random walk process")


# outlierIndexs = sort(ceiling(runif(5, 1,dataLen)))

# outlierIndexs_IO = outlierIndexs[1]
# dataVec[outlierIndexs_IO:dataLen] = synthetic_randomWalk(initVal = dataVec[outlierIndexs_IO-1], mean = 0, sd = runif(1, 0, 3), length = length(outlierIndexs_IO:dataLen))
# ts.plot(dataVec, main = "Stationary time series with IO", ylab = expression(X[t]))

# outlierIndexs_LSO = outlierIndexs[2]
# dataVec[outlierIndexs_LSO:dataLen] = synthetic_randomWalk(initVal = dataVec[outlierIndexs_LSO-1], mean = 0, sd = runif(1, 0, 3), length = length(outlierIndexs_LSO:dataLen))
# ts.plot(dataVec, main = "Stationary time series with LSO", ylab = expression(X[t]))

# outlierIndexs_TCO = outlierIndexs[3:4]
# dataVec[outlierIndexs_TCO[1]:outlierIndexs_TCO[2]] = dataVec[outlierIndexs_TCO[1]:outlierIndexs_TCO[2]] + runif(1, -3, 3)
# ts.plot(dataVec, main = "Stationary time series with TCO", ylab = expression(X[t]))

# outlierIndexs_VC = outlierIndexs[5]
# addVarianceVec = seq(1, 3, length=length(outlierIndexs_VC:dataLen))
# dataVec[outlierIndexs_VC:dataLen] = dataVec[outlierIndexs_VC:dataLen] * addVarianceVec
# ts.plot(dataVec, main = "Stationary time series with VC", ylab = expression(X[t]))

# outlierIndexs_AO = runif(10, 1, dataLen)
# dataVec[outlierIndexs_AO] = dataVec[outlierIndexs_AO] + 100 * runif(10, -5, 5)
# ts.plot(dataVec, main = "Stationary time series with AO", ylab = expression(X[t]))





##### Moving Average of order q: MA(q)
# source("synthetic_MA1.R")
# dataVec = synthetic_MA1(coef=-0.45, mean = 0, sd = 1, length = dataLen)
# ts.plot(dataVec, main = "Moving Average or order 1 process")





##### Auto-Regressive of order p: AR(p)
# source("synthetic_AR1.R")
# dataVec = synthetic_AR1(initVal = 1, coef=-0.3, mean = 0, sd = 1.5, length = dataLen)
# ts.plot(dataVec)





##### Autoregressive moving average process: ARMA(p,q)
# source("synthetic_ARMA11.R")
# dataVec = synthetic_ARMA11(initVal = 1000, coefAR=-0.45, coefMA=-0.45, mean = 0, sd = 1, length = dataLen)
# ts.plot(X, main = "ARMA(1,1) process")





##### 전력데이터
# Building A : 녹지캠
# Building B : 인문대          # 86641번째 index가 NA
# Building C : 하나과학관

# data = read.csv("./datasets/buildingA_15min.csv")
# dataVec = data[,5]
datetime = seqDatetime_byLength(startDate="2015-09-01", length=length(dataVec), split=96)   # 15분씩 나뉘어있으므로 split=96

# indexVec = getUniqVec(datetime, index="YYYYMMDDHHMM")   # 1일 단위로 하려면 YYYYMMDD / 1시간 단위로 하려면 YYYYMMDDHH / 15분 단위로 하려면 YYYYMMDDHHMM
# res = getCalcVec(dataVec, indexVec, calc="sum")
# temp = cbind(indexVec, res)
# dataVec = as.numeric(temp[,2])



##### 주가지수 데이터      # Not Completed
# library(quantmod)
# library(Quandl)

# today <- Sys.Date()
# ski_xts <- getSymbols(Symbols="096770.KS", 
                      # src = "yahoo", 
                      # from= "2013-01-01", 
                      # to = today, auto.assign = FALSE)

                      
                      
                      
                      
                      
                      
                      
                      
                      
##### Sample Vector 리스트 구하기
source("getPartialData.R")  # dataVec을 stepSize만큼 건너뛰면서 partialLength씩 자른다.

### for Trend Test
partialLen_Trend = 96*14
stepSize_Trend = 96

signif_Trend = 0.001

sampleVec_Trend = getPartialData(dataVec, partialLength=partialLen_Trend, stepSize=stepSize_Trend)


### for Unit Root Test
partialLen_UnitRoot = 96*3.5
stepSize_UnitRoot = 96

lag_UnitRoot = 96
signif_UnitRoot = 0.001


sampleVec_UnitRoot = getPartialData(dataVec, partialLength=partialLen_UnitRoot, stepSize=stepSize_UnitRoot)





library(urca)

analysisRes = lapply(sampleVec_UnitRoot$data, ur.df, lags=lag_UnitRoot, type='trend')                                        # ADF Test: Trend
# analysisRes = lapply(sampleVec_UnitRoot$data, ur.df, selectlags='Fixed', type='trend')                            # ADF Test: Trend
# analysisRes = lapply(sampleVec_UnitRoot$data, ur.df, selectlags='AIC', type='trend')                             # ADF Test: Trend
# analysisRes = lapply(sampleVec_UnitRoot$data, ur.df, selectlags='BIC', type='trend')                             # ADF Test: Trend
# analysisRes = lapply(sampleVec_UnitRoot$data, ur.df, lags=lag, type='drift')                                         # ADF Test: Drift
# analysisRes = lapply(sampleVec_UnitRoot$data, ur.pp, type='Z-tau', model='trend', lags='long')             # PP Test: Trend
# analysisRes = lapply(sampleVec_UnitRoot$data, ur.pp, type='Z-tau', model='constant', lags='long')        # PP Test: constant
# analysisRes = lapply(sampleVec_UnitRoot$data, ur.ers, type='DF-GLS', model='trend', lag.max=lag)        # ERS Test: DF-GLS: Trend
# analysisRes = lapply(sampleVec_UnitRoot$data, ur.ers, type='P-test', model='trend')                            # ERS Test: P-Test
# analysisRes = lapply(sampleVec_UnitRoot$data, ur.sp, type='tau', pol.deg=2, signif=signif_UnitRoot)                     # SP Test: tau
# analysisRes = lapply(sampleVec_UnitRoot$data, type='rho', pol.deg=2, signif=signif_UnitRoot)                              # SP Test: rho
# analysisRes = lapply(sampleVec_UnitRoot$data, type='rho', pol.deg=2, signif=signif_UnitRoot)                              # KPSS Test: rho





source("plotAll.R")
source("plotTrendTest.R")
source("plotUnitRootTest.R")
par(mfrow = c(4, 1))
plotAll(dataVec, datetime)


# plot(decompose(ts(dataVec, frequency = 96*365))$trend, main="trend")
# plot(decompose(ts(dataVec, frequency = 96*365))$seasonal, main="sensonal")


plotAll(dataVec, datetime)
plotTrendTest(sampleVec_Trend, type="none")     ### Trend Test
plotUnitRootTest(sampleVec_UnitRoot, analysisRes)       ### Unit Root Test


plotAll(dataVec, datetime)
plotTrendTest(sampleVec_Trend, type="inc")     ### Trend Test
plotUnitRootTest(sampleVec_UnitRoot, analysisRes)       ### Unit Root Test


plotAll(dataVec, datetime)
plotTrendTest(sampleVec_Trend, type="des")     ### Trend Test
plotUnitRootTest(sampleVec_UnitRoot, analysisRes)       ### Unit Root Test