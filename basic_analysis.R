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
datetime = seqDatetime_byLength(startDate="2015-09-01", length=length(dataVec), split=96)   # 15분씩 나뉘어있으므로 split=96




# 1일 단위로 하려면 YYYYMMDD
# 1시간 단위로 하려면 YYYYMMDDHH
indexVec = getUniqVec(datetime, index="YYYYMMDDHHMM")
res = getCalcVec(dataVec, indexVec, calc="sum")
temp = cbind(indexVec, res)


partialLen = 96*20
stepSize = 96*10

lag = 96/2
signif = 0.001

source("getPartialData.R")  # dataVec을 stepSize만큼 건너뛰면서 partialLength씩 자른다.
sampleVec = getPartialData(dataVec, partialLength=partialLen, stepSize=stepSize)



library(urca)

analysisRes = lapply(sampleVec$data, ur.df, lags=lag, type='trend')                                        # ADF Test: Trend
# analysisRes = lapply(sampleVec$data, ur.df, selectlags='Fixed', type='trend')                            # ADF Test: Trend
# analysisRes = lapply(sampleVec$data, ur.df, selectlags='AIC', type='trend')                             # ADF Test: Trend
# analysisRes = lapply(sampleVec$data, ur.df, selectlags='BIC', type='trend')                             # ADF Test: Trend
# analysisRes = lapply(sampleVec$data, ur.df, lags=lag, type='drift')                                         # ADF Test: Drift
# analysisRes = lapply(sampleVec$data, ur.pp, type='Z-tau', model='trend', lags='long')             # PP Test: Trend
# analysisRes = lapply(sampleVec$data, ur.pp, type='Z-tau', model='constant', lags='long')        # PP Test: constant
# analysisRes = lapply(sampleVec$data, ur.ers, type='DF-GLS', model='trend', lag.max=lag)        # ERS Test: DF-GLS: Trend
# analysisRes = lapply(sampleVec$data, ur.ers, type='P-test', model='trend')                            # ERS Test: P-Test
# analysisRes = lapply(sampleVec$data, ur.sp, type='tau', pol.deg=2, signif=signif)                     # SP Test: tau
# analysisRes = lapply(sampleVec$data, type='rho', pol.deg=2, signif=signif)                              # SP Test: rho
# analysisRes = lapply(sampleVec$data, type='rho', pol.deg=2, signif=signif)                              # KPSS Test: rho


### Cox-Stuart Trend Test
source("cox_stuart_test.R")
source("cox_stuart_test_inc.R")
source("cox_stuart_test_des.R")




source("plotAll.R")
par(mfrow = c(3, 1))
plotAll(dataVec, datetime)


coxres = lapply(sampleVec$data, cox_stuart_test)
for (i in 1:len)
{
    if (as.numeric(coxres[[i]]$statistic) < signif)
    {
        #points(cbind(sampleVec$index[[i]], i))
        if (names(coxres[[i]]$statistic) == "Increasing trend, p-value")
        {
            rect(min(sampleVec$index[[i]]), min(dataVec), max(sampleVec$index[[i]]), max(dataVec), col="lightpink", lty=0)
        }
        else
        {
            rect(min(sampleVec$index[[i]]), min(dataVec), max(sampleVec$index[[i]]), max(dataVec), col="lightblue", lty=0)
        }
    }
}


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
    else
    {
        points(sampleVec$index[[i]], sampleVec$data[[i]], type="l", col="black");	
    }
}




plotAll(dataVec, datetime)
coxIncres = lapply(sampleVec$data, cox_stuart_test_inc)
for (i in 1:len)
{
    if (as.numeric(coxIncres[[i]]$statistic) < signif)
    {
        #points(cbind(sampleVec$index[[i]], i))
        rect(min(sampleVec$index[[i]]), min(dataVec), max(sampleVec$index[[i]]), max(dataVec), col="lightpink", lty=0)
    }
    else
    {
        rect(min(sampleVec$index[[i]]), min(dataVec), max(sampleVec$index[[i]]), max(dataVec), col="white", lty=0)
    }
}


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
    else
    {
        points(sampleVec$index[[i]], sampleVec$data[[i]], type="l", col="black");	
    }
}




plotAll(dataVec, datetime)
coxDesres = lapply(sampleVec$data, cox_stuart_test_des)
for (i in 1:len)
{
    if (as.numeric(coxDesres[[i]]$statistic) < signif)
    {
        #points(cbind(sampleVec$index[[i]], i))
        rect(min(sampleVec$index[[i]]), min(dataVec), max(sampleVec$index[[i]]), max(dataVec), col="lightblue", lty=0)
    }
    else
    {
        rect(min(sampleVec$index[[i]]), min(dataVec), max(sampleVec$index[[i]]), max(dataVec), col="white", lty=0)
    }
}


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
    else
    {
        points(sampleVec$index[[i]], sampleVec$data[[i]], type="l", col="black");	
    }
}


