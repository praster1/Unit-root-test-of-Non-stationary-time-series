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










##### ENTSO-E 데이터
### 데이터 출처: https://www.entsoe.eu/data/data-portal/?fbclid=IwAR3OF_cAHJN4Xbg2C54j_gvMflY42oS-Liijqye64lpdq1D-8nZSaFeaXgI
### 2006년 1월 1일 1시 1분부터 2015년 12월 31일까지, 1시간 단위
data = read.csv("./datasets/ENTSO-E_Monthly-hourly-load-values_2006-2015.csv")
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

par(mfrow = c(6, 1))
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
plotUnitRootTest_urpp(sampleVec_UnitRoot, analysisRes, critVal=3, lwd=3)       ### Unit Root Test

# PP Test: constant     #
analysisRes = lapply(sampleVec_UnitRoot$data, ur.pp, type='Z-tau', model='constant', use.lag=24)
plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="PP Test: Z-tau, constant")
plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
plotUnitRootTest_urpp(sampleVec_UnitRoot, analysisRes, critVal=3, lwd=3)       ### Unit Root Test




source("plotUnitRootTest_urers.R")
# ERS Test: DF-GLS: Trend
analysisRes = lapply(sampleVec_UnitRoot$data, ur.ers, type='DF-GLS', model='trend', lag.max=4)        
plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="ERS Test: DF-GLS: Trend")
plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
plotUnitRootTest_urers(sampleVec_UnitRoot, analysisRes, critVal=1, lwd=3, testReverse=TRUE)       ### Unit Root Test

# ERS Test: DF-GLS: Constant
analysisRes = lapply(sampleVec_UnitRoot$data, ur.ers, type='DF-GLS', model='constant', lag.max=4)        
plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="ERS Test: DF-GLS: Trend")
plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
plotUnitRootTest_urers(sampleVec_UnitRoot, analysisRes, critVal=1, lwd=3, testReverse=TRUE)       ### Unit Root Test



# ERS Test: P-Test      #
analysisRes = lapply(sampleVec_UnitRoot$data, ur.ers, type='P-test', model='trend')                            
plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="ERS Test: P-Test")
plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
plotUnitRootTest_urers(sampleVec_UnitRoot, analysisRes, critVal=1, lwd=3, testReverse=TRUE)       ### Unit Root Test

# ERS Test: P-Test      #
analysisRes = lapply(sampleVec_UnitRoot$data, ur.ers, type='P-test', model='constant')                            
plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="ERS Test: P-Test")
plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
plotUnitRootTest_urers(sampleVec_UnitRoot, analysisRes, critVal=1, lwd=3, testReverse=TRUE)       ### Unit Root Test



source("plotUnitRootTest_ursp.R")
# SP Test: tau   #
analysisRes = lapply(sampleVec_UnitRoot$data, ur.sp, type='tau', pol.deg=4, signif=0.000001)
plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="SP Test: tau")
plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
plotUnitRootTest_ursp(sampleVec_UnitRoot, analysisRes, lwd=3, testReverse=TRUE)       ### Unit Root Test

# SP Test: rho      #
analysisRes = lapply(sampleVec_UnitRoot$data, ur.sp, type='rho', pol.deg=4, signif=0.00001)
plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="SP Test: rho")
plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
plotUnitRootTest_ursp(sampleVec_UnitRoot, analysisRes, lwd=3, testReverse=TRUE)       ### Unit Root Test



source("plotUnitRootTest_urkpss.R")
# KPSS Test: mu     #
# analysisRes = lapply(sampleVec_UnitRoot$data, ur.kpss, type='mu', use.lag=24)
# plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="KPSS Test: mu")
# plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
# plotUnitRootTest_urkpss(sampleVec_UnitRoot, analysisRes, critVal=1, lwd=3)       ### Unit Root Test

	
# KPSS Test: tau        #
analysisRes = lapply(sampleVec_UnitRoot$data, ur.kpss, type='tau', use.lag=24)
plotAll(dataVec, datetime, xlab=xlab, ylab=ylab, main="KPSS Test: tau")
plotTrendTest(sampleVec_Trend, type="none", signIf=signif_Trend)     ### Trend Test
plotUnitRootTest_urkpss(sampleVec_UnitRoot, analysisRes, critVal=1, lwd=3, testReverse=TRUE)       ### Unit Root Test
