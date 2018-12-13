rm(list = ls())



setwd("/home/lv999/Dropbox/Github/Unit-root-test-of-Non-stationary-time-series")
# setwd("E:/Dropbox/Github/Unit-root-test-of-Non-stationary-time-series")



source("seqDatetime_byEnddate.R")   # 시작일(startDate)부터 종료일(endDate) 직전까지 날짜 벡터 구하기      
#seqDatetime_byEnddate(startDate="2000-01-01", endDate="2000-01-04", split=7)
source("seqDatetime_byLength.R")    # 시작일(startDate)부터 길이(length)만큼 날짜 벡터 구하기
# seqDatetime_byLength(startDate="2000-01-04", length=6, split=7)


source("getUniqVec.R")  # datetime의 index를 구하는 함수       # getUniqVec(datetimeVec, index="YYYYMMDDHHMMDD")
source("getCalcVec.R")  # split의 시작값, 종료값, 평균값, 중앙값 등을 구하는 함수       # getCalcVec(dataVec, datetimeIndexVec, calc="last")





dataLen = 96*365*3      # 105120





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



datetime = seqDatetime_byLength(startDate="2015-09-01", length=length(dataVec), split=96)   # 15분씩 나뉘어있으므로 split=96

                      


                      
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



sampleVec_UnitRoot = getPartialData(dataVec, partialLength=partialLen_UnitRoot, stepSize=stepSize_UnitRoot)





library(urca)
source("plotAll.R")
source("plotTrendTest.R")

# par(mfrow = c(6, 2))
# plotAll(dataVec, datetime)

xlab = ""
ylab = "X"




par(mfrow = c(7, 2))
source("plotUnitRootTest_urdf.R")
# ADF Test: Trend
analysisRes = lapply(sampleVec_UnitRoot$data, ur.df, lags=96, type='trend')                                        
plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="ADF Test: Trend")
plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
plotUnitRootTest_urdf(sampleVec_UnitRoot, analysisResult=analysisRes, critVal=3, testReverse=FALSE, lwd=3)       ### Unit Root Test

# ADF Test: Drift
analysisRes = lapply(sampleVec_UnitRoot$data, ur.df, lags=96, type='drift')                                         
plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="ADF Test: Drift")
plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
plotUnitRootTest_urdf(sampleVec_UnitRoot, analysisResult=analysisRes, critVal=3, testReverse=FALSE, lwd=3)       ### Unit Root Test


source("plotUnitRootTest_urpp.R")
# PP Test: Trend    #
analysisRes = lapply(sampleVec_UnitRoot$data, ur.pp, type='Z-alpha', model='trend', use.lag=12)
plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="PP Test: Z-alpha, Trend")
plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
plotUnitRootTest_urpp(sampleVec_UnitRoot, analysisRes, critVal=3, testReverse=FALSE, lwd=3)       ### Unit Root Test

# PP Test: constant     #
analysisRes = lapply(sampleVec_UnitRoot$data, ur.pp, type='Z-alpha', model='constant', use.lag=12)
plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="PP Test: Z-alpha, Constant")
plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
plotUnitRootTest_urpp(sampleVec_UnitRoot, analysisRes, critVal=3, testReverse=FALSE, lwd=3)       ### Unit Root Test

# PP Test: Trend    #
analysisRes = lapply(sampleVec_UnitRoot$data, ur.pp, type='Z-tau', model='trend', use.lag=12)
plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="PP Test: Z-tau, Trend")
plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
plotUnitRootTest_urpp(sampleVec_UnitRoot, analysisRes, critVal=3, testReverse=FALSE, lwd=3)       ### Unit Root Test

# PP Test: constant     #
analysisRes = lapply(sampleVec_UnitRoot$data, ur.pp, type='Z-tau', model='constant', use.lag=12)
plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="PP Test: Z-tau, Constant")
plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
plotUnitRootTest_urpp(sampleVec_UnitRoot, analysisRes, critVal=3, testReverse=FALSE, lwd=3)       ### Unit Root Test


source("plotUnitRootTest_urers.R")
# ERS Test: DF-GLS: Trend
analysisRes = lapply(sampleVec_UnitRoot$data, ur.ers, type='DF-GLS', model='trend', lag.max=12)
plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="ERS Test: DF-GLS, Trend")
plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
plotUnitRootTest_urers(sampleVec_UnitRoot, analysisRes, critVal=3, testReverse=FALSE, lwd=3)       ### Unit Root Test

# ERS Test: DF-GLS: Constant
analysisRes = lapply(sampleVec_UnitRoot$data, ur.ers, type='DF-GLS', model='constant', lag.max=12)
plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="ERS Test: DF-GLS, Constent")
plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
plotUnitRootTest_urers(sampleVec_UnitRoot, analysisRes, critVal=3, testReverse=FALSE, lwd=3)       ### Unit Root Test

# ERS Test: P-Test      #
analysisRes = lapply(sampleVec_UnitRoot$data, ur.ers, type='P-test', model='trend', lag.max=12)                
plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="ERS Test: P-Test, Trend")
plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
plotUnitRootTest_urers(sampleVec_UnitRoot, analysisRes, critVal=3, testReverse=FALSE, lwd=3)       ### Unit Root Test

# ERS Test: P-Test      #
analysisRes = lapply(sampleVec_UnitRoot$data, ur.ers, type='P-test', model='constant', lag.max=12)                   
plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="ERS Test: P-Test, Constant")
plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
plotUnitRootTest_urers(sampleVec_UnitRoot, analysisRes, critVal=3, testReverse=FALSE, lwd=3)       ### Unit Root Test


source("plotUnitRootTest_ursp.R")
# SP Test: tau   #
analysisRes = lapply(sampleVec_UnitRoot$data, ur.sp, type='tau', pol.deg=12, signif=0.000001)
plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="SP Test: tau")
plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
plotUnitRootTest_ursp(sampleVec_UnitRoot, analysisRes, testReverse=TRUE, lwd=3)       ### Unit Root Test

# SP Test: rho      #
analysisRes = lapply(sampleVec_UnitRoot$data, ur.sp, type='rho', pol.deg=12, signif=0.000001)
plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="SP Test: rho")
plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
plotUnitRootTest_ursp(sampleVec_UnitRoot, analysisRes, testReverse=TRUE, lwd=3)       ### Unit Root Test


source("plotUnitRootTest_urkpss.R")
# KPSS Test: mu     #
analysisRes = lapply(sampleVec_UnitRoot$data, ur.kpss, type='mu', use.lag=12)
plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="KPSS Test: mu")
plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
plotUnitRootTest_urkpss(sampleVec_UnitRoot, analysisRes, critVal=1, testReverse=FALSE, lwd=3)       ### Unit Root Test

# KPSS Test: tau        #
analysisRes = lapply(sampleVec_UnitRoot$data, ur.kpss, type='tau', use.lag=12)
plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="KPSS Test: tau")
plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
plotUnitRootTest_urkpss(sampleVec_UnitRoot, analysisRes, critVal=1, testReverse=FALSE, lwd=3)       ### Unit Root Test
