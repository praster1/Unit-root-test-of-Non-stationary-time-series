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
# 15분 단위로 하려면 YYYYMMDDHHMM
indexVec = getUniqVec(datetime, index="YYYYMMDDHHMM")
res = getCalcVec(dataVec, indexVec, calc="sum")
temp = cbind(indexVec, res)
dataVec = as.numeric(temp[,2])





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









### Cox-Stuart Trend Test
source("cox_stuart_test.R")
source("cox_stuart_test_inc.R")
source("cox_stuart_test_des.R")




source("plotAll.R")
par(mfrow = c(4, 1))
plotAll(dataVec, datetime)





plotAll(dataVec, datetime)


### Trend Test
len = length(sampleVec_Trend$data)
coxres = lapply(sampleVec_Trend$data, cox_stuart_test)
for (i in 1:len)
{
    if (as.numeric(coxres[[i]]$statistic) < signif_Trend)
    {
        #points(cbind(sampleVec$index[[i]], i))
        if (names(coxres[[i]]$statistic) == "Increasing trend, p-value")
        {
            rect(min(sampleVec_Trend$index[[i]]), min(dataVec), max(sampleVec_Trend$index[[i]]), max(dataVec), col="lightpink", lty=0)
        }
        else
        {
            rect(min(sampleVec_Trend$index[[i]]), min(dataVec), max(sampleVec_Trend$index[[i]]), max(dataVec), col="lightblue", lty=0)
        }
    }
}


### Unit Root Test
len = length(sampleVec_UnitRoot$data)
for (i in 1:len)
{
    testStat = analysisRes[[i]]@teststat[1]
    cval = analysisRes[[i]]@cval[1,3]
    print(paste("i:", i, "/", len, "     ", testStat < cval))
    if (testStat < cval)
    {
        points(sampleVec_UnitRoot$index[[i]], sampleVec_UnitRoot$data[[i]], type="l", col="red");	
    }
    else
    {
        points(sampleVec_UnitRoot$index[[i]], sampleVec_UnitRoot$data[[i]], type="l", col="black");	
    }
}





plotAll(dataVec, datetime)

### Trend Test
len = length(sampleVec_Trend$data)
coxIncres = lapply(sampleVec_Trend$data, cox_stuart_test_inc)
for (i in 1:len)
{
    if (as.numeric(coxIncres[[i]]$statistic) < signif_Trend)
    {
        #points(cbind(sampleVec$index[[i]], i))
        rect(min(sampleVec_Trend$index[[i]]), min(dataVec), max(sampleVec_Trend$index[[i]]), max(dataVec), col="lightpink", lty=0)
    }
    else
    {
        rect(min(sampleVec_Trend$index[[i]]), min(dataVec), max(sampleVec_Trend$index[[i]]), max(dataVec), col="white", lty=0)
    }
}


### Unit Root Test
len = length(sampleVec_UnitRoot$data)
for (i in 1:len)
{
    testStat = analysisRes[[i]]@teststat[1]
    cval = analysisRes[[i]]@cval[1,3]
    print(paste("i:", i, "/", len, "     ", testStat < cval))
    if (testStat < cval)
    {
        points(sampleVec_UnitRoot$index[[i]], sampleVec_UnitRoot$data[[i]], type="l", col="red");	
    }
    else
    {
        points(sampleVec_UnitRoot$index[[i]], sampleVec_UnitRoot$data[[i]], type="l", col="black");	
    }
}





plotAll(dataVec, datetime)

### Trend Test
len = length(sampleVec_Trend$data)
coxDesres = lapply(sampleVec_Trend$data, cox_stuart_test_des)
for (i in 1:len)
{
    if (as.numeric(coxDesres[[i]]$statistic) < signif_Trend)
    {
        #points(cbind(sampleVec$index[[i]], i))
        rect(min(sampleVec_Trend$index[[i]]), min(dataVec), max(sampleVec_Trend$index[[i]]), max(dataVec), col="lightblue", lty=0)
    }
    else
    {
        rect(min(sampleVec_Trend$index[[i]]), min(dataVec), max(sampleVec_Trend$index[[i]]), max(dataVec), col="white", lty=0)
    }
}


### Unit Root Test
len = length(sampleVec_UnitRoot$data)
for (i in 1:len)
{
    testStat = analysisRes[[i]]@teststat[1]
    cval = analysisRes[[i]]@cval[1,3]
    print(paste("i:", i, "/", len, "     ", testStat < cval))
    if (testStat < cval)
    {
        points(sampleVec_UnitRoot$index[[i]], sampleVec_UnitRoot$data[[i]], type="l", col="red");	
    }
    else
    {
        points(sampleVec_UnitRoot$index[[i]], sampleVec_UnitRoot$data[[i]], type="l", col="black");	
    }
}


