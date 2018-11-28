rm(list = ls())

# Building A : 녹지캠
# Building B : 인문대
# Building C : 하나과학관


setwd("/home/lv999/Dropbox/Github/Unit-root-test-of-Non-stationary-time-series")



source("seqDatetime_byEnddate.R")   # 시작일(startDate)부터 종료일(endDate) 직전까지 날짜 벡터 구하기      
#seqDatetime_byEnddate(startDate="2000-01-01", endDate="2000-01-04", split=7)
source("seqDatetime_byLength.R")    # 시작일(startDate)부터 길이(length)만큼 날짜 벡터 구하기
# seqDatetime_byLength(startDate="2000-01-04", length=6, split=7)


source("getUniqVec.R")  # datetime의 index를 구하는 함수       # getUniqVec(datetimeVec, index="YYYYMMDDHHMMDD")
source("getCalcVec.R")  # split의 시작값, 종료값, 평균값, 중앙값 등을 구하는 함수       # getCalcVec(dataVec, datetimeIndexVec, calc="last")


data = read.csv("./datasets/buildingA_15min.csv")
dataVec = data[,5]
datetime = seqDatetime_byLength(startDate="2015-09-01", length=length(dataVec), split=96)




# 1일 단위로 하려면 YYYYMMDD
# 1시간 단위로 하려면 YYYYMMDDHH
indexVec = getUniqVec(datetime, index="YYYYMMDDHHMM")
res = getCalcVec(dataVec, indexVec, calc="sum")
temp = cbind(indexVec, res)


source("getPartialData.R")  # dataVec을 stepSize만큼 건너뛰면서 partialLength씩 자른다.
sampleVec = getPartialData(dataVec, partialLength=96*3, stepSize=96)



source("plotAll.R")
plotAll(dataVec, datetime)

library(urca)

# analysisRes = lapply(sampleVec$data, ur.df, lags=1, type='trend')                                         # ADF Test: Trend
# analysisRes = lapply(sampleVec$data, ur.df, lags=1, type='drift')                                           # ADF Test: Drift
# analysisRes = lapply(sampleVec$data, ur.pp, type='Z-tau', model='trend', lags='long')             # PP Test: Trend
# analysisRes = lapply(sampleVec$data, ur.pp, type='Z-tau', model='constant', lags='long')        # PP Test: constant
analysisRes = lapply(sampleVec$data, ur.ers, type='DF-GLS', model='trend', lag.max=4)          # ERS Test: DF-GLS: Trend
# analysisRes = lapply(sampleVec$data, ur.ers, type='P-test', model='trend')                            # ERS Test: P-Test
# analysisRes = lapply(sampleVec$data, ur.sp, type='tau', pol.deg=2, signif=0.05)                      # SP Test: tau
# analysisRes = lapply(sampleVec$data, type='rho', pol.deg=2, signif=0.05)                               # SP Test: rho
# analysisRes = lapply(sampleVec$data, type='rho', pol.deg=2, signif=0.05)                               # KPSS Test: rho


len = length(sampleVec$data)
for (i in 1:len)
{
    testStat = analysisRes[[i]]@teststat[1]
    cval = analysisRes[[i]]@cval[1,2]
    print(paste("i:", i, "/", len, "     ", testStat < cval))
    if (testStat < cval)
    {
        points(sampleVec$index[[i]], sampleVec$data[[i]], type="l", col="red");	
    }
}
