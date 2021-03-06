rm(list = ls())



setwd("/home/lv999/Dropbox/Github/Unit-root-test-of-Non-stationary-time-series")
# setwd("E:/Dropbox/Github/Unit-root-test-of-Non-stationary-time-series")



source("seqDatetime_byEnddate.R")   # 시작일(startDate)부터 종료일(endDate) 직전까지 날짜 벡터 구하기      
#seqDatetime_byEnddate(startDate="2000-01-01", endDate="2000-01-04", split=7)
source("seqDatetime_byLength.R")    # 시작일(startDate)부터 길이(length)만큼 날짜 벡터 구하기
# seqDatetime_byLength(startDate="2000-01-04", length=6, split=7)


source("getUniqVec.R")  # datetime의 index를 구하는 함수       # getUniqVec(datetimeVec, index="YYYYMMDDHHMMDD")
source("getCalcVec.R")  # split의 시작값, 종료값, 평균값, 중앙값 등을 구하는 함수       # getCalcVec(dataVec, datetimeIndexVec, calc="last")





dataLen = 96*365
IDXGap_ADFTrend = NULL
IDXGap_ADFDrift = NULL
IDXGap_PPTrend = NULL
IDXGap_PPDrift = NULL
IDXGap_ERSDGTrend = NULL
IDXGap_ERSDGConst = NULL
IDXGap_ERSPTrend = NULL
IDXGap_ERSPConst = NULL
IDXGap_SPTau = NULL
IDXGap_SPRho = NULL
IDXGap_KPSSTau = NULL



for (ell in 1:100)
{
	set.seed(ell)
	##### Simulation of a random time series
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
	source("IDXGap.R")

	# par(mfrow = c(6, 2))
	# plotAll(dataVec, datetime, main="Stationary time series with outliers")

	xlab = ""
	ylab = "X"




	# par(mfrow = c(7, 2))
	source("plotUnitRootTest_urdf.R")
	# ADF Test: Trend
	analysisRes = lapply(sampleVec_UnitRoot$data, ur.df, lags=96, type='trend')                                        
	# plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="ADF Test: Trend")
	# plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
	# plotUnitRootTest_urdf(sampleVec_UnitRoot, analysisResult=analysisRes, critVal=3, testReverse=FALSE, lwd=3)       ### Unit Root Test

	IDXGap_IO = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_IO, critVal=3, testReverse=FALSE)
	IDXGap_LSO = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_LSO, critVal=3, testReverse=FALSE)
	IDXGap_TCO1 = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_TCO[1], critVal=3, testReverse=FALSE)
	IDXGap_TCO2 = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_TCO[2], critVal=3, testReverse=FALSE)
	IDXGap_VC = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_VC, critVal=3, testReverse=FALSE)
	IDXGap_ADFTrend = rbind(IDXGap_ADFTrend, c(IDXGap_IO, IDXGap_LSO, IDXGap_TCO1, IDXGap_TCO2, IDXGap_VC))


	# ADF Test: Drift
	analysisRes = lapply(sampleVec_UnitRoot$data, ur.df, lags=96, type='drift')                                         
	# plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="ADF Test: Drift")
	# plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
	# plotUnitRootTest_urdf(sampleVec_UnitRoot, analysisResult=analysisRes, critVal=3, testReverse=FALSE, lwd=3)       ### Unit Root Test

	IDXGap_IO = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_IO, critVal=3, testReverse=FALSE)
	IDXGap_LSO = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_LSO, critVal=3, testReverse=FALSE)
	IDXGap_TCO1 = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_TCO[1], critVal=3, testReverse=FALSE)
	IDXGap_TCO2 = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_TCO[2], critVal=3, testReverse=FALSE)
	IDXGap_VC = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_VC, critVal=3, testReverse=FALSE)
	IDXGap_ADFDrift = rbind(IDXGap_ADFDrift, c(IDXGap_IO, IDXGap_LSO, IDXGap_TCO1, IDXGap_TCO2, IDXGap_VC))



	source("plotUnitRootTest_urpp.R")
	## PP Test: Trend    #
	# analysisRes = lapply(sampleVec_UnitRoot$data, ur.pp, type='Z-alpha', model='trend', use.lag=4)
	# plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="PP Test: Z-alpha, Trend")
	# plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
	# plotUnitRootTest_urpp(sampleVec_UnitRoot, analysisRes, critVal=3, testReverse=FALSE, lwd=3)       ### Unit Root Test


	## PP Test: constant     #
	# analysisRes = lapply(sampleVec_UnitRoot$data, ur.pp, type='Z-alpha', model='constant', use.lag=4)
	# plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="PP Test: Z-alpha, Constant")
	# plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
	# plotUnitRootTest_urpp(sampleVec_UnitRoot, analysisRes, critVal=3, testReverse=FALSE, lwd=3)       ### Unit Root Test


	# PP Test: Trend    #
	analysisRes = lapply(sampleVec_UnitRoot$data, ur.pp, type='Z-tau', model='trend', use.lag=4)
	# plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="PP Test: Z-tau, Trend")
	# plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
	# plotUnitRootTest_urpp(sampleVec_UnitRoot, analysisRes, critVal=3, testReverse=TRUE, lwd=3)       ### Unit Root Test

	IDXGap_IO = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_IO, critVal=3, testReverse=FALSE)
	IDXGap_LSO = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_LSO, critVal=3, testReverse=FALSE)
	IDXGap_TCO1 = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_TCO[1], critVal=3, testReverse=FALSE)
	IDXGap_TCO2 = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_TCO[2], critVal=3, testReverse=FALSE)
	IDXGap_VC = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_VC, critVal=3, testReverse=FALSE)
	IDXGap_PPTrend = rbind(IDXGap_PPTrend, c(IDXGap_IO, IDXGap_LSO, IDXGap_TCO1, IDXGap_TCO2, IDXGap_VC))


	# PP Test: constant     #
	analysisRes = lapply(sampleVec_UnitRoot$data, ur.pp, type='Z-tau', model='constant', use.lag=4)
	# plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="PP Test: Z-tau, Constant")
	# plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
	# plotUnitRootTest_urpp(sampleVec_UnitRoot, analysisRes, critVal=3, testReverse=TRUE, lwd=3)       ### Unit Root Test

	IDXGap_IO = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_IO, critVal=3, testReverse=FALSE)
	IDXGap_LSO = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_LSO, critVal=3, testReverse=FALSE)
	IDXGap_TCO1 = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_TCO[1], critVal=3, testReverse=FALSE)
	IDXGap_TCO2 = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_TCO[2], critVal=3, testReverse=FALSE)
	IDXGap_VC = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_VC, critVal=3, testReverse=FALSE)
	IDXGap_PPDrift = rbind(IDXGap_PPDrift, c(IDXGap_IO, IDXGap_LSO, IDXGap_TCO1, IDXGap_TCO2, IDXGap_VC))



	source("plotUnitRootTest_urers.R")
	# ERS Test: DF-GLS: Trend
	analysisRes = lapply(sampleVec_UnitRoot$data, ur.ers, type='DF-GLS', model='trend', lag.max=(96/2))
	# plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="ERS Test: DF-GLS, Trend")
	# plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
	# plotUnitRootTest_urers(sampleVec_UnitRoot, analysisRes, critVal=3, testReverse=FALSE, lwd=3)       ### Unit Root Test

	IDXGap_IO = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_IO, critVal=3, testReverse=FALSE)
	IDXGap_LSO = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_LSO, critVal=3, testReverse=FALSE)
	IDXGap_TCO1 = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_TCO[1], critVal=3, testReverse=FALSE)
	IDXGap_TCO2 = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_TCO[2], critVal=3, testReverse=FALSE)
	IDXGap_VC = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_VC, critVal=3, testReverse=FALSE)
	IDXGap_ERSDGTrend = rbind(IDXGap_ERSDGTrend, c(IDXGap_IO, IDXGap_LSO, IDXGap_TCO1, IDXGap_TCO2, IDXGap_VC))


	# ERS Test: DF-GLS: Constant
	analysisRes = lapply(sampleVec_UnitRoot$data, ur.ers, type='DF-GLS', model='constant', lag.max=96)
	# plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="ERS Test: DF-GLS, Constent")
	# plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
	# plotUnitRootTest_urers(sampleVec_UnitRoot, analysisRes, critVal=3, testReverse=FALSE, lwd=3)       ### Unit Root Test

	IDXGap_IO = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_IO, critVal=3, testReverse=FALSE)
	IDXGap_LSO = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_LSO, critVal=3, testReverse=FALSE)
	IDXGap_TCO1 = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_TCO[1], critVal=3, testReverse=FALSE)
	IDXGap_TCO2 = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_TCO[2], critVal=3, testReverse=FALSE)
	IDXGap_VC = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_VC, critVal=3, testReverse=FALSE)
	IDXGap_ERSDGConst = rbind(IDXGap_ERSDGConst, c(IDXGap_IO, IDXGap_LSO, IDXGap_TCO1, IDXGap_TCO2, IDXGap_VC))


	# ERS Test: P-Test      #
	analysisRes = lapply(sampleVec_UnitRoot$data, ur.ers, type='P-test', model='trend', lag.max=12)                
	# plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="ERS Test: P-Test, Trend")
	# plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
	# plotUnitRootTest_urers(sampleVec_UnitRoot, analysisRes, critVal=3, testReverse=TRUE, lwd=3)       ### Unit Root Test

	IDXGap_IO = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_IO, critVal=3, testReverse=FALSE)
	IDXGap_LSO = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_LSO, critVal=3, testReverse=FALSE)
	IDXGap_TCO1 = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_TCO[1], critVal=3, testReverse=FALSE)
	IDXGap_TCO2 = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_TCO[2], critVal=3, testReverse=FALSE)
	IDXGap_VC = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_VC, critVal=3, testReverse=FALSE)
	IDXGap_ERSPTrend = rbind(IDXGap_ERSPTrend, c(IDXGap_IO, IDXGap_LSO, IDXGap_TCO1, IDXGap_TCO2, IDXGap_VC))


	# ERS Test: P-Test      #
	analysisRes = lapply(sampleVec_UnitRoot$data, ur.ers, type='P-test', model='constant', lag.max=12)                   
	# plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="ERS Test: P-Test, Constant")
	# plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
	# plotUnitRootTest_urers(sampleVec_UnitRoot, analysisRes, critVal=3, testReverse=TRUE, lwd=3)       ### Unit Root Test

	IDXGap_IO = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_IO, critVal=3, testReverse=FALSE)
	IDXGap_LSO = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_LSO, critVal=3, testReverse=FALSE)
	IDXGap_TCO1 = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_TCO[1], critVal=3, testReverse=FALSE)
	IDXGap_TCO2 = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_TCO[2], critVal=3, testReverse=FALSE)
	IDXGap_VC = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_VC, critVal=3, testReverse=FALSE)
	IDXGap_ERSPConst = rbind(IDXGap_ERSPConst, c(IDXGap_IO, IDXGap_LSO, IDXGap_TCO1, IDXGap_TCO2, IDXGap_VC))



	source("plotUnitRootTest_ursp.R")
	# SP Test: tau   #
	analysisRes = lapply(sampleVec_UnitRoot$data, ur.sp, type='tau', pol.deg=12, signif=0.00001)
	# plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="SP Test: tau")
	# plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
	# plotUnitRootTest_ursp(sampleVec_UnitRoot, analysisRes, testReverse=TRUE, lwd=3)       ### Unit Root Test

	IDXGap_IO = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_IO, testReverse=FALSE, ursp=TRUE)
	IDXGap_LSO = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_LSO, testReverse=FALSE, ursp=TRUE)
	IDXGap_TCO1 = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_TCO[1], testReverse=FALSE, ursp=TRUE)
	IDXGap_TCO2 = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_TCO[2], testReverse=FALSE, ursp=TRUE)
	IDXGap_VC = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_VC, testReverse=FALSE, ursp=TRUE)
	IDXGap_SPTau = rbind(IDXGap_SPTau, c(IDXGap_IO, IDXGap_LSO, IDXGap_TCO1, IDXGap_TCO2, IDXGap_VC))


	# SP Test: rho      #
	analysisRes = lapply(sampleVec_UnitRoot$data, ur.sp, type='rho', pol.deg=12, signif=0.00001)
	# plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="SP Test: rho")
	# plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
	# plotUnitRootTest_ursp(sampleVec_UnitRoot, analysisRes, testReverse=TRUE, lwd=3)       ### Unit Root Test

	IDXGap_IO = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_IO, testReverse=FALSE, ursp=TRUE)
	IDXGap_LSO = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_LSO, testReverse=FALSE, ursp=TRUE)
	IDXGap_TCO1 = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_TCO[1], testReverse=FALSE, ursp=TRUE)
	IDXGap_TCO2 = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_TCO[2], testReverse=FALSE, ursp=TRUE)
	IDXGap_VC = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_VC, testReverse=FALSE, ursp=TRUE)
	IDXGap_SPRho = rbind(IDXGap_SPRho, c(IDXGap_IO, IDXGap_LSO, IDXGap_TCO1, IDXGap_TCO2, IDXGap_VC))


	source("plotUnitRootTest_urkpss.R")
	## KPSS Test: mu     #
	# analysisRes = lapply(sampleVec_UnitRoot$data, ur.kpss, type='mu', use.lag=12)
	# plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="KPSS Test: mu")
	# plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
	# plotUnitRootTest_urkpss(sampleVec_UnitRoot, analysisRes, critVal=3, testReverse=FALSE, lwd=3)       ### Unit Root Test

	# KPSS Test: tau        #
	analysisRes = lapply(sampleVec_UnitRoot$data, ur.kpss, type='tau', use.lag=12)
	# plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="KPSS Test: tau")
	# plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
	# plotUnitRootTest_urkpss(sampleVec_UnitRoot, analysisRes, critVal=3, testReverse=FALSE, lwd=3)       ### Unit Root Test

	IDXGap_IO = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_IO, critVal=3, testReverse=FALSE)
	IDXGap_LSO = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_LSO, critVal=3, testReverse=FALSE)
	IDXGap_TCO1 = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_TCO[1], critVal=3, testReverse=FALSE)
	IDXGap_TCO2 = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_TCO[2], critVal=3, testReverse=FALSE)
	IDXGap_VC = IDXGap(sampleVec_UnitRoot$index, analysisRes, outlierIndexs_VC, critVal=3, testReverse=FALSE)
	IDXGap_KPSSTau = rbind(IDXGap_KPSSTau, c(IDXGap_IO, IDXGap_LSO, IDXGap_TCO1, IDXGap_TCO2, IDXGap_VC))
	
	
	print(ell)
}


write.csv(IDXGap_ADFTrend, "01_IDXGap_ADFTrend.csv")
write.csv(IDXGap_ADFDrift, "01_IDXGap_ADFDrift.csv")
write.csv(IDXGap_PPTrend, "01_IDXGap_PPTrend.csv")
write.csv(IDXGap_PPDrift, "01_IDXGap_PPDrift.csv")
write.csv(IDXGap_ERSDGTrend, "01_IDXGap_ERSDGTrend.csv")
write.csv(IDXGap_ERSDGConst, "01_IDXGap_ERSDGConst.csv")
write.csv(IDXGap_ERSPTrend, "01_IDXGap_ERSPTrend.csv")
write.csv(IDXGap_ERSPConst, "01_IDXGap_ERSPConst.csv")
write.csv(IDXGap_SPTau, "01_IDXGap_SPTau.csv")
write.csv(IDXGap_SPRho, "01_IDXGap_SPRho.csv")
write.csv(IDXGap_KPSSTau, "01_IDXGap_KPSSTau.csv")