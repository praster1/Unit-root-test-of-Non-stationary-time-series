rm(list = ls())



setwd("/home/lv999/Dropbox/Github/Unit-root-test-of-Non-stationary-time-series")
# setwd("E:/Dropbox/Github/Unit-root-test-of-Non-stationary-time-series")



source("seqDatetime_byEnddate.R")   # 시작일(startDate)부터 종료일(endDate) 직전까지 날짜 벡터 구하기      
#seqDatetime_byEnddate(startDate="2000-01-01", endDate="2000-01-04", split=7)
source("seqDatetime_byLength.R")    # 시작일(startDate)부터 길이(length)만큼 날짜 벡터 구하기
# seqDatetime_byLength(startDate="2000-01-04", length=6, split=7)


source("getUniqVec.R")  # datetime의 index를 구하는 함수       # getUniqVec(datetimeVec, index="YYYYMMDDHHMMDD")
source("getCalcVec.R")  # split의 시작값, 종료값, 평균값, 중앙값 등을 구하는 함수       # getCalcVec(dataVec, datetimeIndexVec, calc="last")

set.seed(123456)









##### Simulation of a random time series
dataLen = 96*365*3      # 105120
par(mfrow = c(6, 1))

source("synthetic_pureRP.R")
dataVec = synthetic_pureRP(constMean = 0, mean = 0, sd = 1, length = dataLen)
# plot(dataVec, main = "Stationary time series", ylab = expression(X[t]), type="l")



# outlier
   # - 1. Additive Outliers (AO): 시계열 자료 중 한 개가 지나치게 크거나 작은 값을 갖는 관측치
   # - 2. innovational outliers(IO): 이후의 시계열이 전혀 다른 형태를 보이는 특징을 가지는 이상치
   # - 3. level shift outliers(LSO): 이후에 시계열 전체가 위나 아래로 이동한 경우
   # - 4. temporary chance outlers(TCO): 일시적으로 시계열이 이동하였으나 지수적으로 빠르게 원래의 상태로 돌아가는 시계열의 형태
   # - 5. seasonal level shift(SLS): 계절적으로 특정한 시기에 나타나는 이상치
   # - 6. variance change(VC): 이후에 시간이 흐를 수록 변동폭이 커지거나 작아지는 이상치

outlierIndexs = sort(ceiling(runif(5, 1,(dataLen-1000))))

# - 2. innovational outliers(IO)
outlierIndexs_IO = outlierIndexs[1]
dataVec[outlierIndexs_IO:dataLen] = synthetic_pureRP(constMean = 0, mean = 0, sd = runif(1, 0, 3), length = length(outlierIndexs_IO:dataLen))
# plot(dataVec, main = "Stationary time series with IO", ylab = expression(X[t]), type="l")
# points(outlierIndexs_IO, dataVec[outlierIndexs_IO], col="red", lwd=5)

# - 3. level shift outliers(LSO)
outlierIndexs_LSO = outlierIndexs[2]
dataVec[outlierIndexs_LSO:dataLen] = synthetic_pureRP(constMean = 0, mean = runif(1, -3, 3), sd = runif(1, 0, 3), length = length(outlierIndexs_LSO:dataLen))
# plot(dataVec, main = "Stationary time series with LSO", ylab = expression(X[t]), type="l")
# points(outlierIndexs_LSO, dataVec[outlierIndexs_LSO], col="red", lwd=5)

# - 4. temporary chance outlers(TCO)
outlierIndexs_TCO = outlierIndexs[3:4]
dataVec[outlierIndexs_TCO[1]:outlierIndexs_TCO[2]] = dataVec[outlierIndexs_TCO[1]:outlierIndexs_TCO[2]] + runif(1, -3, 3)
# plot(dataVec, main = "Stationary time series with TCO", ylab = expression(X[t]), type="l")
# points(outlierIndexs_TCO, dataVec[outlierIndexs_TCO], col="red", lwd=5)

# - 6. variance change(VC)
outlierIndexs_VC = outlierIndexs[5]
addVarianceVec = seq(1, 3, length=length(outlierIndexs_VC:dataLen))
dataVec[outlierIndexs_VC:dataLen] = dataVec[outlierIndexs_VC:dataLen] * addVarianceVec
# plot(dataVec, main = "Stationary time series with VC", ylab = expression(X[t]), type="l")
# points(outlierIndexs_VC, dataVec[outlierIndexs_VC], col="red", lwd=5)

# - 1. Additive Outliers (AO)
outlierIndexs_AO = runif(10, 1, dataLen)
dataVec[outlierIndexs_AO] = dataVec[outlierIndexs_AO] + 10 * runif(10, -5, 5)
# plot(dataVec, main = "Stationary time series with AO", ylab = expression(X[t]), type="l")
# points(outlierIndexs_AO, dataVec[outlierIndexs_AO], col="red", lwd=5)





set.seed(123456)
##### Random Walk process simulation
par(mfrow = c(6, 1))

source("synthetic_randomWalk.R")
dataVec = synthetic_randomWalk(initVal = 1000, mean = 0, sd = 1, length = dataLen)
# plot(dataVec, main = "Random Walk process", ylab = expression(X[t]), type="l")


outlierIndexs = sort(ceiling(runif(5, 1,dataLen)))

outlierIndexs_IO = outlierIndexs[1]
dataVec[outlierIndexs_IO:dataLen] = synthetic_randomWalk(initVal = dataVec[outlierIndexs_IO-1], mean = 0, sd = runif(1, 0, 3), length = length(outlierIndexs_IO:dataLen))
# plot(dataVec, main = "Random Walk process with IO", ylab = expression(X[t]), type="l")
# points(outlierIndexs_IO, dataVec[outlierIndexs_IO], col="red", lwd=5)

outlierIndexs_LSO = outlierIndexs[2]
dataVec[outlierIndexs_LSO:dataLen] = synthetic_randomWalk(initVal = dataVec[outlierIndexs_LSO-1], mean = 0, sd = runif(1, 0, 3), length = length(outlierIndexs_LSO:dataLen))
# plot(dataVec, main = "Random Walk process with TCO", ylab = expression(X[t]), type="l")
# points(outlierIndexs_LSO, dataVec[outlierIndexs_LSO], col="red", lwd=5)

outlierIndexs_TCO = outlierIndexs[3:4]
dataVec[outlierIndexs_TCO[1]:outlierIndexs_TCO[2]] = dataVec[outlierIndexs_TCO[1]:outlierIndexs_TCO[2]] + runif(1, -3, 3)
# plot(dataVec, main = "Random Walk process with TCO", ylab = expression(X[t]), type="l")
# points(outlierIndexs_TCO, dataVec[outlierIndexs_TCO], col="red", lwd=5)

outlierIndexs_VC = outlierIndexs[5]
addVarianceVec = seq(1, 3, length=length(outlierIndexs_VC:dataLen))
dataVec[outlierIndexs_VC:dataLen] = dataVec[outlierIndexs_VC:dataLen] * addVarianceVec
# plot(dataVec, main = "Random Walk process with VC", ylab = expression(X[t]), type="l")
# points(outlierIndexs_VC, dataVec[outlierIndexs_VC], col="red", lwd=5)

outlierIndexs_AO = runif(10, 1, dataLen)
dataVec[outlierIndexs_AO] = dataVec[outlierIndexs_AO] + 100 * runif(10, -5, 5)
# plot(dataVec, main = "Random Walk process with AO", ylab = expression(X[t]), type="l")
# points(outlierIndexs_AO, dataVec[outlierIndexs_AO], col="red", lwd=5)





##### Moving Average of order q: MA(q)
# source("synthetic_MA1.R")
# dataVec = synthetic_MA1(coef=-0.45, mean = -5, sd = 1, length = dataLen)
# plot(dataVec, main = "Moving Average or order 1 process", type="l")





set.seed(123456)
##### Auto-Regressive of order p: AR(p)
source("synthetic_AR1.R")
dataVec = synthetic_AR1(initVal = 10, coef=1, mean = runif(1, 0, 3), sd = 5, length = dataLen)
# plot(dataVec, type="l")


absVec = rep(c(1, -1), length=10)
changeIndex = c(1, sort(ceiling(runif(9, 1,(dataLen-1000)))), dataLen)
dataVec = NULL;
for (i in 1:10)
{
    initVal = NULL
    if (i == 1) {
        initVal = 10;    
    } else {        
        initVal = dataVec[changeIndex[i]-1];    
    }
    
    dataVec[changeIndex[i]:changeIndex[i+1]] = synthetic_AR1(initVal = initVal, coef=1, mean = absVec[i]*runif(1, 0, 5), sd = runif(1, 0, 100), length = length(changeIndex[i]:changeIndex[i+1]))
}

# plot(dataVec, type="l")
# points(changeIndex, dataVec[changeIndex], col="red", lwd=5)





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





##### ENTOS-E 데이터
### 데이터 출처: https://www.entsoe.eu/data/data-portal/?fbclid=IwAR3OF_cAHJN4Xbg2C54j_gvMflY42oS-Liijqye64lpdq1D-8nZSaFeaXgI
### 2006년 1월 1일 1시 1분부터 2015년 12월 31일까지, 1시간 단위
data = read.csv("./datasets/ENTOS-E_Monthly-hourly-load-values_2006-2015.csv")
uniq = unique(data[,1])
newData = list()
for (i in 1:length(uniq))
{
	newData[[i]] = data[data[,1] == uniq[i],]
}

# AT BA BE BG CH CS CY CZ DE DK DK_W EE ES FI FR GB GR HR HU IE IS IT LT LU LV ME MK NI NL NO PL PT RO RS SE SI SK UA_W
# dataVec = as.numeric(t(as.matrix(newData[[6]][,6:29])))
# dataVec = as.numeric(t(as.matrix(newData[[10]][,6:29])))
dataVec = as.numeric(t(as.matrix(newData[[26]][,6:29])))
datetime = seqDatetime_byLength(startDate="2006-01-01", length=length(dataVec), split=24)   # 15분씩 나뉘어있으므로 split=96

### Imputation
NAwhichs = which(is.na(dataVec))
NAwhichs

## Imputation: 값을 전부 (이전값 + 이후값)/2로 대체
for (i in 1:length(NAwhichs))
{
	dataVec[NAwhichs[i]] = (dataVec[NAwhichs[i]-1] + dataVec[NAwhichs[i]-2])/2
}

## Imputation: 값을 전부 1로 대체
# dataVec[NAwhichs] = 1

summary(dataVec)





##### 주가지수 데이터      # Not Completed
# library(quantmod)
# library(Quandl)

# today <- Sys.Date()
# ski_xts <- getSymbols(Symbols="096770.KS", 
                      # src = "yahoo", 
                      # from= "2013-01-01", 
                      # to = today, auto.assign = FALSE)

                      
                      
                      
                      
                      
# plot(dataVec, type="l")
# points(changeIndex, dataVec[changeIndex], col="red", lwd=5)
                                            
                     
                      
##### Sample Vector 리스트 구하기
source("getPartialData.R")  # dataVec을 stepSize만큼 건너뛰면서 partialLength씩 자른다.

### for Trend Test
partialLen_Trend = 24*24
stepSize_Trend = 24
signif_Trend = 0.0001

sampleVec_Trend = getPartialData(dataVec, partialLength=partialLen_Trend, stepSize=stepSize_Trend)


### for Unit Root Test
partialLen_UnitRoot = 24*7
stepSize_UnitRoot = 24



sampleVec_UnitRoot = getPartialData(dataVec, partialLength=partialLen_UnitRoot, stepSize=stepSize_UnitRoot)





library(urca)
source("plotAll.R")
source("plotTrendTest.R")

par(mfrow = c(7, 2))
# par(mfrow = c(4, 1))
plotAll(dataVec, datetime)

xlab = "Time Index"
ylab = "X"


source("plotUnitRootTest_urdf.R")
# ADF Test: Trend
analysisRes = lapply(sampleVec_UnitRoot$data, ur.df, lags=24, type='trend')                                        
plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="ADF Test: Trend")
plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
plotUnitRootTest_urdf(sampleVec_UnitRoot, analysisResult=analysisRes, critVal=2, lwd=3)       ### Unit Root Test

# ADF Test: Drift
analysisRes = lapply(sampleVec_UnitRoot$data, ur.df, lags=24, type='drift')                                         
plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="ADF Test: Drift")
plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
plotUnitRootTest_urdf(sampleVec_UnitRoot, analysisResult=analysisRes, critVal=1, lwd=3)       ### Unit Root Test


source("plotUnitRootTest_urpp.R")
# PP Test: Trend    #
# analysisRes = lapply(sampleVec_UnitRoot$data, ur.pp, type='Z-alpha', model='trend', use.lag=24)
# plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="PP Test: Z-alpha, Trend")
# plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
# plotUnitRootTest_urpp(sampleVec_UnitRoot, analysisRes, critVal=2, lwd=3)       ### Unit Root Test

# PP Test: constant     #
# analysisRes = lapply(sampleVec_UnitRoot$data, ur.pp, type='Z-alpha', model='constant', use.lag=24)
# plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="PP Test: Z-alpha, constant")
# plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
# plotUnitRootTest_urpp(sampleVec_UnitRoot, analysisRes, critVal=2, lwd=3)       ### Unit Root Test

# PP Test: Trend    #
analysisRes = lapply(sampleVec_UnitRoot$data, ur.pp, type='Z-tau', model='trend', use.lag=24)
plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="PP Test: Z-tau, Trend")
plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
plotUnitRootTest_urpp(sampleVec_UnitRoot, analysisRes, critVal=1, lwd=3)       ### Unit Root Test

# PP Test: constant     #
analysisRes = lapply(sampleVec_UnitRoot$data, ur.pp, type='Z-tau', model='constant', use.lag=24)
plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="PP Test: Z-tau, constant")
plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
plotUnitRootTest_urpp(sampleVec_UnitRoot, analysisRes, critVal=1, lwd=3)       ### Unit Root Test


source("plotUnitRootTest_urers.R")
# ERS Test: DF-GLS: Trend
analysisRes = lapply(sampleVec_UnitRoot$data, ur.ers, type='DF-GLS', model='trend', lag.max=4)        
plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="ERS Test: DF-GLS: Trend")
plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
plotUnitRootTest_urers(sampleVec_UnitRoot, analysisRes, critVal=2, lwd=3)       ### Unit Root Test

# ERS Test: DF-GLS: Constant
analysisRes = lapply(sampleVec_UnitRoot$data, ur.ers, type='DF-GLS', model='constant', lag.max=4)        
plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="ERS Test: DF-GLS: Trend")
plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
plotUnitRootTest_urers(sampleVec_UnitRoot, analysisRes, critVal=2, lwd=3)       ### Unit Root Test

# ERS Test: P-Test      #
analysisRes = lapply(sampleVec_UnitRoot$data, ur.ers, type='P-test', model='trend')                            
plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="ERS Test: P-Test")
plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
plotUnitRootTest_urers(sampleVec_UnitRoot, analysisRes, critVal=1, lwd=3)       ### Unit Root Test

# ERS Test: P-Test      #
analysisRes = lapply(sampleVec_UnitRoot$data, ur.ers, type='P-test', model='constant')                            
plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="ERS Test: P-Test")
plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
plotUnitRootTest_urers(sampleVec_UnitRoot, analysisRes, critVal=1, lwd=3)       ### Unit Root Test


source("plotUnitRootTest_ursp.R")
# SP Test: tau   #
analysisRes = lapply(sampleVec_UnitRoot$data, ur.sp, type='tau', pol.deg=4, signif=0.000001)
plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="SP Test: tau")
plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
plotUnitRootTest_ursp(sampleVec_UnitRoot, analysisRes, lwd=3)       ### Unit Root Test

# SP Test: rho      #
analysisRes = lapply(sampleVec_UnitRoot$data, ur.sp, type='rho', pol.deg=4, signif=0.00001)
plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="SP Test: rho")
plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
plotUnitRootTest_ursp(sampleVec_UnitRoot, analysisRes, lwd=3)       ### Unit Root Test


source("plotUnitRootTest_urkpss.R")
# KPSS Test: mu     #
analysisRes = lapply(sampleVec_UnitRoot$data, ur.kpss, type='mu', use.lag=24)
plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="KPSS Test: mu")
plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
plotUnitRootTest_urkpss(sampleVec_UnitRoot, analysisRes, critVal=1, lwd=3)       ### Unit Root Test

# KPSS Test: tau        #
analysisRes = lapply(sampleVec_UnitRoot$data, ur.kpss, type='tau', use.lag=24)
plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="KPSS Test: tau")
plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
plotUnitRootTest_urkpss(sampleVec_UnitRoot, analysisRes, critVal=1, lwd=3)       ### Unit Root Test
