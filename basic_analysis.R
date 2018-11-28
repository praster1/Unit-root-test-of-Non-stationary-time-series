rm(list = ls())

# Building A : 녹지캠
# Building B : 인문대
# Building C : 하나과학관


setwd("/home/lv999/Dropbox/Github/Unit-root-test-of-Non-stationary-time-series")



source("seqDatetime_byEnddate.R")   # 시작일(startDate)부터 종료일(endDate) 직전까지 날짜 벡터 구하기      #seqDatetime_byEnddate(startDate="2000-01-01", endDate="2000-01-04", split=7)
source("seqDatetime_byLength.R")    # 시작일(startDate)부터 길이(length)만큼 날짜 벡터 구하기       # seqDatetime_byLength(startDate="2000-01-04", length=6, split=7)


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
sampleVec = getPartialData(dataVec, partialLength=96, stepSize=10)



source("plotAll.R")
plotAll(dataVec, datetime)

library(urca)
# lc.ct = ur.df(vec, lags=3, type='trend')		# ADF Test: Trend
# lc.ct = ur.df(vec, lags=3, type='drift')		# ADF Test: Drift
# lc.ct = ur.pp(vec, type='Z-tau', model='trend', lags='long')		# PP Test: Trend
# lc.ct = ur.pp(vec, type='Z-tau', model='constant', lags='long')   # PP Test: constant
# lc.ct = ur.ers(vec, type="DF-GLS", model="trend", lag.max=4)	# ERS Test: DF-GLS: Trend
# lc.ct = ur.ers(vec, type="P-test", model="trend")		# ERS Test: P-Test
# lc.ct = ur.sp(vec, type="tau", pol.deg=2, signif=0.05)		# SP Test: tau
# lc.ct = ur.sp(vec, type="rho", pol.deg=2, signif=0.05)		# SP Test: rho
# lc.ct = ur.sp(vec, type="rho", pol.deg=2, signif=0.05)		# KPSS Test: rho

# analysisRes = lapply(sampleVec$data, ur.df, lags=3, type='trend')                                         # ADF Test: Trend
analysisRes = lapply(sampleVec$data, ur.df, lags=3, type='drift')                                           # ADF Test: Drift
# analysisRes = lapply(sampleVec$data, ur.pp, type='Z-tau', model='trend', lags='long')             # PP Test: Trend
# analysisRes = lapply(sampleVec$data, ur.pp, type='Z-tau', model='constant', lags='long')        # PP Test: constant
# analysisRes = lapply(sampleVec$data, ur.ers, type='DF-GLS', model='trend', lag.max=4)          # ERS Test: DF-GLS: Trend
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





# A = lapply(sampleVec, ur.df, lags=3, type="trend")




testStat = lc.ct@testreg$fstatistic
resPVal = 1 - pf(testStat[1], testStat[2], testStat[3])





# install.packages("urca")
library(urca)

par(mfrow = c(5, 1))

temp = c(0.2, 0.4, 0.6, 0.8, 1)
for (k in 3:7)
{
	for (j in 1:5)
	{
		# Partial Len >= Step Size
		# partialLen = 95*3		# Partial Length - 1
		partialLen = 95*k		# Partial Length - 1
		# stepLen = ceiling(95*0.8)		# Step Size
		# stepLen = ceiling(partialLen*0.2)		# Step Size
		stepLen = ceiling(partialLen*temp[j])		# Step Size

		plotAll(main=paste("PP Test: Trend // Partial Len = ", partialLen, " / Step Size = ", stepLen, sep=""))

		for (i in seq((partialLen+1), length(dataVec), by=stepLen))
		{
			vec = dataVec[(i-partialLen):i]
			# lc.ct = ur.df(vec, lags=3, type='trend')		# ADF Test: Trend
			# lc.ct = ur.df(vec, lags=3, type='drift')		# ADF Test: Drift
			# lc.ct = ur.pp(vec, type='Z-tau', model='trend', lags='long')		# PP Test: Trend
			lc.ct = ur.pp(vec, type='Z-tau', model='constant', lags='long')		# PP Test: constant
			# lc.ct = ur.ers(vec, type="DF-GLS", model="trend", lag.max=4)	# ERS Test: DF-GLS: Trend
			# lc.ct = ur.ers(vec, type="P-test", model="trend")		# ERS Test: P-Test
			# lc.ct = ur.sp(vec, type="tau", pol.deg=2, signif=0.05)		# SP Test: tau
			# lc.ct = ur.sp(vec, type="rho", pol.deg=2, signif=0.05)		# SP Test: rho
			# lc.ct = ur.sp(vec, type="rho", pol.deg=2, signif=0.05)		# KPSS Test: rho
			
			# testStat = lc.ct@testreg$fstatistic
			# resPVal = 1 - pf(testStat[1], testStat[2], testStat[3])
			
			# print(paste("i:", i, "/", length(dataVec), "     ", resPVal))
			print(paste("i:", i, "/", length(dataVec), "     ", lc.ct@teststat[1] < lc.ct@cval[1,2]))
			if (lc.ct@teststat[1] < lc.ct@cval[1,2])
			{
				points((i-partialLen):i, vec, type="l", col="red");	
			}
		}
	}
}






df <- data.frame(Month=seq(1,12),
                 Value = rnorm(12,0,1),
                 Season = c('Winter', 'Winter', 'Spring',
                            'Spring', 'Spring', 'Summer',
                            'Summer', 'Summer', 'Fall',
                            'Fall', 'Fall', 'Winter'))
attach(df)
plot(Value~Month, type="n")
rect(df$Month-0.5, min(df$Value), df$Month+0.5, max(df$Value), col=df$Season, lty=0)
lines(Value~Month, data=df, type='l', col="orange", lwd=2)






cox.stuart.test = function(x) {
    method = "Cox-Stuart test for trend analysis"
    leng = length(x)
    apross = round(leng)%%2
    if (apross == 1) {
        delete = (length(x) + 1)/2
        x = x[-delete]
    }
    half = length(x)/2
    x1 = x[1:half]
    x2 = x[(half + 1):(length(x))]
    difference = x1 - x2
    signs = sign(difference)
    signcorr = signs[signs != 0]
    pos = signs[signs > 0]
    neg = signs[signs < 0]
    if (length(pos) < length(neg)) {
        prop = pbinom(length(pos), length(signcorr), 0.5)
        names(prop) = "Increasing trend, p-value"
        rval <- list(method = method, statistic = prop)
        class(rval) = "htest"
        return(rval)
    } else {
        prop = pbinom(length(neg), length(signcorr), 0.5)
        names(prop) = "Decreasing trend, p-value"
        rval <- list(method = method, statistic = prop)
        class(rval) = "htest"
        return(rval)
    }
}

cox.stuart.test_inc = function(x) {
    method = "Cox-Stuart test for trend analysis"
    leng = length(x)
    apross = round(leng)%%2
    if (apross == 1) {
        delete = (length(x) + 1)/2
        x = x[-delete]
    }
    half = length(x)/2
    x1 = x[1:half]
    x2 = x[(half + 1):(length(x))]
    difference = x1 - x2
    signs = sign(difference)
    signcorr = signs[signs != 0]
    pos = signs[signs > 0]
    neg = signs[signs < 0]
    prop = pbinom(length(pos), length(signcorr), 0.5)
    names(prop) = "Increasing trend, p-value"
    rval <- list(method = method, statistic = prop)
    class(rval) = "htest"
    return(rval)
}


cox.stuart.test_des = function(x) {
    method = "Cox-Stuart test for trend analysis"
    leng = length(x)
    apross = round(leng)%%2
    if (apross == 1) {
        delete = (length(x) + 1)/2
        x = x[-delete]
    }
    half = length(x)/2
    x1 = x[1:half]
    x2 = x[(half + 1):(length(x))]
    difference = x1 - x2
    signs = sign(difference)
    signcorr = signs[signs != 0]
    pos = signs[signs > 0]
    neg = signs[signs < 0]
    prop = pbinom(length(neg), length(signcorr), 0.5)
    names(prop) = "Decreasing trend, p-value"
    rval <- list(method = method, statistic = prop)
    class(rval) = "htest"
    return(rval)
}


vec  = dataVec[17473:26304] #20160301 ~ 20160531
# customers = c(5, 9, 12, 18, 17, 16, 19, 20, 4, 3, 18, 16, 17, 15, 14)
cox.stuart.test(vec)
cox.stuart.test_inc(vec)
cox.stuart.test_des(vec)


vec  = dataVec[26305:35136] #20160601 ~ 20160831
cox.stuart.test(vec)
cox.stuart.test_inc(vec)
cox.stuart.test_des(vec)



































########## 여기서부터는 진행 보류
########## Linear Regression

##### 총지출 = 소득항목
temp_data = data[,-c(1:4, 13:35)]


# 로그 변환
temp_data = log(temp_data)
for (i in 1:7)	{	temp_data[is.infinite(temp_data[,i]),i] = 0;	}


# plot(temp_data[,-1])


fit_lm1 = lm(X062가계지출~., data=temp_data)

round(coefficients(fit_lm1), 3)
summary(fit_lm1)	# 점추정
coef(fit_lm1)			
confint(fit_lm1)		# 구간추정
anova(fit_lm1)		# 기울기에 대한 가설검정


### 반응치(Mean Response) 구간추정
predict(fit_lm1, interval="confidence") 

### 예측구간
predict(fit_lm1, interval="predict")


### 선형회귀모델 가정 체크
par(mfrow = c(2, 2))
plot(fit_lm1)





##### 총지출 = 지출항목
temp_data = data[,-c(1:11, 13, 14)]


# 로그 변환
temp_data = log(temp_data)
for (i in 2:22)	{	temp_data[is.infinite(temp_data[,i]),i] = 0;	}


# plot(temp_data[,-1])


fit_lm2 = lm(X062가계지출~., data=temp_data)

round(coefficients(fit_lm2), 3)
summary(fit_lm2)	# 점추정
coef(fit_lm1)			
confint(fit_lm2)		# 구간추정
anova(fit_lm2)		# 기울기에 대한 가설검정



### 반응치(Mean Response) 구간추정
predict(fit_lm2, interval="confidence") 

### 예측구간
predict(fit_lm2, interval="predict")


### 선형회귀모델 가정 체크
# par(mfrow = c(2, 2))
# plot(fit_lm2)










########## Logistic Regression
temp_data = data[,-c(1:2, 4:11, 12:14)]


# 로그 변환
temp_data[,-1] = log(temp_data[,-1])
for (i in 2:22)	{	temp_data[is.infinite(temp_data[,i]),i] = 0;	}



jobcode = 1		### 직업코드 1

temp_data1 = temp_data[which(temp_data[,1] == jobcode), ];		temp_data1[,1] = 1
temp_data2 = temp_data[-which(temp_data[,1] == jobcode), ];		temp_data2[,1] = 0

new_temp_data = rbind(temp_data1, temp_data2)

fit_logistic = glm(X016가구주_직업코드~., data=new_temp_data, family="binomial")


summary(fit_logistic)
as.matrix(round(coefficients(fit_logistic), 3))
as.matrix(round(exp(coef(fit_logistic)), 3))
round(confint(fit_logistic),3)
round(exp(confint(fit_logistic)),3)











#####
table(data_기본정보[,16])


yearQuant_vec = sort(as.numeric(paste(merge(c(1990:2016), c(14, 24, 34, 44))[,1], merge(c(1990:2016), c(14, 24, 34, 44))[,2], sep="")))



yearly_data = NULL;
for (i in yearQuant_vec)
{
	print(i)
	where = which(data[,1]==i);
	yearly_data[[as.character(i)]] = data[where,]
}




each_job_data = function(input_yearly_data)
{
	result_data = NULL;
	
	job_codes = names(table(yearly_data[[1]][,3]))
	
	for (i in 1:length(job_codes))
	{
		result_data[[job_codes[i]]] = input_yearly_data[which(input_yearly_data[,3] == job_codes[i]),]
	}
	
	return(result_data)
}




timeseries = function(job_code = "3", val_name = "X084주류")
{
	res = NULL;
	for (i in yearQuant_vec)
	{
		# print(i)
		res = c(res, sum(each_job_data(yearly_data[[as.character(i)]])[[job_code]][, val_name]))
	}

	name_vec = sort(paste( merge(c(1990:2016), c(14, 24, 34, 44))[,1], merge(c(1990:2016), c(14, 24, 34, 44))[,2], sep=""))[1:106]
	names(res) = name_vec

	res = res[1:106]
	return(res)
}








idx = 33

val = as.matrix(dimnames(data)[[2]])[idx]		# 15부터
res = NULL;			res_vec = NULL;
for (i in 1:9)
{
	res[[i]] = timeseries(job_code = as.character(i), val_name = val)
	res_vec = c(res_vec, res[[i]])
}




plot(seq(min(res_vec), max(res_vec), length=length(res[[1]])),  type="n", main=val)
lwd_vec = rep(1, 9)
# lwd_vec = c(1, 5, 1, 1, 1, 1, 1, 1, 5)	# 16	"X152단체여행비"        
# lwd_vec = c(1, 1, 5, 1, 1, 1, 5, 1, 1)	# 19	"X133영상음향기기"      
# lwd_vec = c(1, 5, 1, 5, 1, 1, 1, 1, 1)	# 20	"X180이자비용"          
# lwd_vec = c(1, 5, 1, 5, 1, 1, 1, 1, 1)	# 22	"X176경상조세"          
# lwd_vec = c(1, 5, 1, 1, 1, 1, 5, 1, 1)	# 27	"X065곡물"              
# lwd_vec = c(5, 5, 1, 1, 1, 1, 1, 1, 1)	# 28	"X068육류"              
# lwd_vec = c(5, 5, 1, 1, 1, 1, 1, 1, 1)	# 29	"X141장난감및취미용품"  
# lwd_vec = c(5, 5, 1, 1, 1, 1, 1, 1, 1)	# 30	"X142캠핑및운동관련용품"
# lwd_vec = c(1, 5, 1, 1, 1, 1, 1, 1, 5)	# 32	"X109가사서비스"        
lwd_vec = c(5, 1, 1, 1, 1, 1, 1, 1, 5)	# 33	"X085담배"              
# lwd_vec = c(5, 1, 1, 1, 1, 1, 1, 1, 5)	# 34	"X084주류"              
# lwd_vec = c(1, 5, 1, 5, 1, 1, 1, 1, 1)	# 35	"X173기타금융"          


i = 1
points(res[[i]],  type="l", col=i, lwd=lwd_vec[i])


for (i in 1:9)
{
	points(res[[i]],  type="l", col=i, lwd=lwd_vec[i])
	
}
legend("topleft", legend=c(1:9), pch = rep(15, 9), lwd=lwd_vec, col=c(1:9))




# 1	 관리자
# 2	 전문가 및 관련 종사자
# 3	 사무 종사자
# 4	 서비스 종사자
# 5	 판매 종사자
# 6	 농림어업숙련 종사자
# 7	 기능원 및 관련기능 종사자
# 8	 장치,기계조작 및 조립 종사자
# 9	 단순노무 종사자




########## 

idx = 33		## 변수 코드를 바꿔가며 코드를 실행한다.

val = as.matrix(dimnames(data)[[2]])[idx]		# 15부터
res = NULL;			res_vec = NULL;
for (i in 1:9)
{
	res[[i]] = timeseries(job_code = as.character(i), val_name = val)
	res_vec = c(res_vec, res[[i]])
}
         

res1 = NULL; res2 = NULL
if (idx == 16)	{	res1 = res[[2]];	res2 = res[[9]]		}
if (idx == 19)	{	res1 = res[[3]];	res2 = res[[7]]		}	
if (idx == 20)	{	res1 = res[[2]];	res2 = res[[4]]		}	
if (idx == 22)	{	res1 = res[[2]];	res2 = res[[4]]		}	
if (idx == 27)	{	res1 = res[[2]];	res2 = res[[7]]		}	
if (idx == 28)	{	res1 = res[[1]];	res2 = res[[2]]		}	
if (idx == 29)	{	res1 = res[[1]];	res2 = res[[2]]		}	
if (idx == 30)	{	res1 = res[[1]];	res2 = res[[2]]		}	
if (idx == 32)	{	res1 = res[[2]];	res2 = res[[9]]		}	
if (idx == 33)	{	res1 = res[[1]];	res2 = res[[9]]		}	
if (idx == 34)	{	res1 = res[[1]];	res2 = res[[9]]		}	
if (idx == 35)	{	res1 = res[[2]];	res2 = res[[4]]		}	





par(mfrow = c(3, 1))

##### Dummy Variable
new_res1 = cbind(res1, rep(c(1:4), 12))
new_res2 = cbind(res2, rep(c(1:4), 12))


### lm 함수
fit = lm(new_res1[,1] ~ as.factor(new_res1[,2]))
summary(fit)
as.matrix(round(coef(fit), 3))

pred1 = predict(fit, interval="predict")
round(pred1[1:8,], 3)

sqrt(sum(fit$residuals^2))   # RMSE



# res2
fit = lm(new_res2[,1] ~ as.factor(new_res2[,2]))
summary(fit)
as.matrix(round(coef(fit), 3))

pred2 = predict(fit, interval="predict")
round(pred[1:8,], 3)

sqrt(sum(fit$residuals^2))   # RMSE

plot(c(1:(106+8)), ylim=c(min(c(new_res1, pred1[,1], new_res2, pred2[,1])), max(c(new_res1, pred1[,1], new_res2, pred2[,1]))), type="n", main="Dummy Variable")

points(c(new_res1[,1]), type="l", col="red")
points(c(106:114), c(new_res1[106,1], pred1[99:106, 1]), type="l", col="red", lwd=5)

points(c(new_res2[,1]), type="l", col="blue")
points(c(106:114), c(new_res2[106,1], pred2[99:106, 1]), type="l", col="blue", lwd=5)







##### Trigonometric models
L = 4

sine_res = function(times=times, pi_mult=1, L = L)	{	sin(2 * pi_mult * pi * times / L);	}
cosine_res = function(times=times, pi_mult=1, L = L)	{	cos(2 * pi_mult * pi * times / L)	}

timevec = 1:length(res1)

fit = lm(res1 ~ c(timevec) + sine_res(timevec, 1, L) + cosine_res(timevec, 1, L) )
summary(fit)

sqrt(sum(fit$residuals^2))   # RMSE

pred1 = predict(fit, interval="predict")
round(pred1[1:8,], 3)

sqrt(sum(fit$residuals^2))   # RMSE

fit = lm(res2 ~ c(timevec) + sine_res(timevec, 1, L) + cosine_res(timevec, 1, L) )
summary(fit)

pred2 = predict(fit, interval="predict")
round(pred2[1:8,], 3)

sqrt(sum(fit$residuals^2))   # RMSE


# plot(pred1[,1], ylim=c(min(c(pred1[,1], pred2[,1])), max(c(pred1[,1], pred2[,1]))), type="n", main="Trigonometric Model 1")
# points(pred1[,1], type="l", col="red")
# points(pred2[,1], type="l", col="blue")


plot(c(1:(106+8)), ylim=c(min(c(new_res1, pred1[,1], new_res2, pred2[,1])), max(c(new_res1, pred1[,1], new_res2, pred2[,1]))), type="n", main="Trigonometric Model 1")

points(c(new_res1[,1]), type="l", col="red")
points(c(106:114), c(new_res1[106,1], pred1[99:106, 1]), type="l", col="red", lwd=5)

points(c(new_res2[,1]), type="l", col="blue")
points(c(106:114), c(new_res2[106,1], pred2[99:106, 1]), type="l", col="blue", lwd=5)








fit = lm(res1 ~ c(timevec) + sine_res(timevec, 1, L) + cosine_res(timevec, 1, L) + sine_res(timevec, 2, L) + cosine_res(timevec, 2, L) )
summary(fit)

sqrt(sum(fit$residuals^2))   # RMSE

pred1 = predict(fit, interval="predict")
round(pred1[1:8,], 3)

fit = lm(res2 ~ c(timevec) + sine_res(timevec, 1, L) + cosine_res(timevec, 1, L) + sine_res(timevec, 2, L) + cosine_res(timevec, 2, L) )
summary(fit)

sqrt(sum(fit$residuals^2))   # RMSE

pred2 = predict(fit, interval="predict")
round(pred2[1:8,], 3)


# plot(pred1[,1], ylim=c(min(c(pred1[,1], pred2[,1])), max(c(pred1[,1], pred2[,1]))), type="n", main="Trigonometric Model 2")
# points(pred1[,1], type="l", col="red")
# points(pred2[,1], type="l", col="blue")

plot(c(1:(106+8)), ylim=c(min(c(new_res1, pred1[,1], new_res2, pred2[,1])), max(c(new_res1, pred1[,1], new_res2, pred2[,1]))), type="n", main="Trigonometric Model 2")

points(c(new_res1[,1]), type="l", col="red")
points(c(106:114), c(new_res1[106,1], pred1[99:106, 1]), type="l", col="red", lwd=5)

points(c(new_res2[,1]), type="l", col="blue")
points(c(106:114), c(new_res2[106,1], pred2[99:106, 1]), type="l", col="blue", lwd=5)










# par(mfrow = c(2, 2))


par(mfrow = c(4, 1))
# seasonal의 인자 =  "additive" 또는 "multiplicative"
# alpha, beta, gamma의 값을 변경하며 확인할 수 있다. 모두 NULL이면 최적값을 찾는다.
require(forecast)

tsdata = ts(res1, start=c(1990, 1), frequency=4)

hw = HoltWinters(tsdata,  alpha = NULL, beta = NULL, gamma = NULL, seasonal = c("additive"))
hw
pred1 <- forecast(hw, n.ahead = 8, prediction.interval = T, level = 0.95)
pred1

sqrt(sum(pred1$residuals^2))   # RMSE



tsdata = ts(res2, start=c(1990, 1), frequency=4)

hw = HoltWinters(tsdata,  alpha = NULL, beta = NULL, gamma = NULL, seasonal = c("additive"))
hw
pred2 <- forecast(hw, n.ahead = 8, prediction.interval = T, level = 0.95)
pred2

sqrt(sum(pred2$residuals^2))   # RMSE

# plot(pred1, col="red", main="Additive Holt Winters of 직업코드 2")
# plot(pred2, col="blue", main="Additive Holt Winters of 직업코드 9")


plot(forecast(pred1), col="red", main="Additive Holt Winters of 직업코드 2")
plot(forecast(pred2), col="blue", main="Additive Holt Winters of 직업코드 9")


tsdata = ts(res1, start=c(1990, 1), frequency=4)

hw = HoltWinters(tsdata,  alpha = NULL, beta = NULL, gamma = NULL, seasonal = c("multiplicative"))
hw
pred1 <- forecast(hw, n.ahead = 8, prediction.interval = T, level = 0.95)
pred1

sqrt(sum(pred1$residuals^2))   # RMSE


tsdata = ts(res2, start=c(1990, 1), frequency=4)

hw = HoltWinters(tsdata,  alpha = NULL, beta = NULL, gamma = NULL, seasonal = c("multiplicative"))
pred2 <- forecast(hw, n.ahead = 8, prediction.interval = T, level = 0.95)
pred2


sqrt(sum(pred2$residuals^2))   # RMSE


# plot(pred1, col="red", main="Multiplicative Holt Winters of 직업코드 2")
# plot(pred2, col="blue", main="Multiplicative Holt Winters of 직업코드 9")





plot(forecast(pred1), col="red", main="Multiplicative Holt Winters of 직업코드 2")
plot(forecast(pred2), col="blue", main="Multiplicative Holt Winters of 직업코드 9")


# par(mfrow = c(2, 2))


##### Exponential Smoothing
require(forecast)

testdata1 = as.numeric(res1)
testdata2 = as.numeric(res2)

# alpha, beta의 인자 값을 바꿔주며 확인한다.
pred1 = ses(testdata1, h=8, alpha = NULL, beta=NULL, initial="simple", level=95)   # Simple Exponential Smoothing
summary(pred1)
sqrt(sum(pred1$residual^2))     # RMSE

pred2 = ses(testdata2, h=8, alpha = NULL, beta=NULL, initial="simple", level=95)   # Simple Exponential Smoothing
summary(pred2)
sqrt(sum(pred2$residual^2))     # RMSE


par(mfrow = c(4, 1))

# plot(pred1, col="red", main="Simple Exponential Smoothing of 직업코드 2")
# plot(pred2, col="blue", main="Simple Exponential Smoothing of 직업코드 9")

plot(forecast(pred1), col="red", main="Single Exponential Smoothing of 직업코드 2")
plot(forecast(pred2), col="blue", main="Single Exponential Smoothing of 직업코드 9")




pred1 = ses(testdata1, h=8, alpha = NULL, beta=NULL, level=95) 			                 # Double Exponential Smoothing
summary(pred1)
sqrt(sum(pred1$residual^2))     # RMSE

pred2 = ses(testdata2, h=8, alpha = NULL, beta=NULL, level=95) 			                 # Double Exponential Smoothing
summary(pred2)
sqrt(sum(pred2$residual^2))     # RMSE

# plot(pred1, col="red", main="Double Exponential Smoothing of 직업코드 2")
# plot(pred2, col="blue", main="Double Exponential Smoothing of 직업코드 9")



plot(forecast(pred1), col="red", main="Double Exponential Smoothing of 직업코드 2")
plot(forecast(pred2), col="blue", main="Double Exponential Smoothing of 직업코드 9")




##### ARIMA
vec = NULL;
for (i in 1:5){	for (j in 0:1)	{		for (k in 1:1)		{			vec = rbind(vec, c(i, j, k))		}	}}
vec = vec[-c(3),]



result = NULL; result1 = NULL; 	result2 = NULL;
counter = 0
for (i in 1:nrow(vec))
{
	counter = counter + 1
	if (i == 4) { next }
	if (i == 5) { next }
	if (i == 6) { next }
	print("====================")
	print(i)
	print(vec[i,])
	
	fit1= result1[[counter]] = arima(res1, vec[i,])
	print(sqrt(sum(fit1$residuals^2)))
	fit2 = result2[[counter]] = arima(res2, vec[i,])
	print(sqrt(sum(fit2$residuals^2)))
	
	result = rbind(result, c( sqrt(sum(fit1$residual^2)), sqrt(sum(fit2$residual^2)) ) )
}


fit1= result1[[counter]] = arima(res1, c(0,0,0))
print(sqrt(sum(fit1$residuals^2)))
fit2 = result2[[counter]] = arima(res2, c(0,0,0))
print(sqrt(sum(fit2$residuals^2)))

fit1= result1[[counter]] = arima(res1, c(0,1,0))
print(sqrt(sum(fit1$residuals^2)))
fit2 = result2[[counter]] = arima(res2, c(0,1,0))
print(sqrt(sum(fit2$residuals^2)))


fit1= result1[[counter]] = arima(res1, c(0,0,1))
print(sqrt(sum(fit1$residuals^2)))
fit2 = result2[[counter]] = arima(res2, c(0,0,1))
print(sqrt(sum(fit2$residuals^2)))

fit1= result1[[counter]] = arima(res1, c(0,1,1))
print(sqrt(sum(fit1$residuals^2)))
fit2 = result2[[counter]] = arima(res2, c(0,1,1))
print(sqrt(sum(fit2$residuals^2)))

fit1= result1[[counter]] = arima(res1, c(1,0,0))
print(sqrt(sum(fit1$residuals^2)))
fit2 = result2[[counter]] = arima(res2, c(1,0,0))
print(sqrt(sum(fit2$residuals^2)))

fit1= result1[[counter]] = arima(res1, c(1,1,0))
print(sqrt(sum(fit1$residuals^2)))
fit2 = result2[[counter]] = arima(res2, c(1,1,0))
print(sqrt(sum(fit2$residuals^2)))


fit1= result1[[counter]] = arima(res1, c(1,0,1))
print(sqrt(sum(fit1$residuals^2)))
fit2 = result2[[counter]] = arima(res2, c(1,0,1))
print(sqrt(sum(fit2$residuals^2)))


i = 1;
vec[i,]
fit1= result1[[counter]] = arima(res1, vec[i,])
print(sqrt(sum(fit1$residuals^2)))
fit2 = result2[[counter]] = arima(res2, vec[i,])
print(sqrt(sum(fit2$residuals^2)))




i = 2;
vec[i,]
fit1= result1[[counter]] = arima(res1, vec[i,])
print(sqrt(sum(fit1$residuals^2)))
fit2 = result2[[counter]] = arima(res2, vec[i,])
print(sqrt(sum(fit2$residuals^2)))

i = 3;
vec[i,]
fit1= result1[[counter]] = arima(res1, vec[i,])
print(sqrt(sum(fit1$residuals^2)))
fit2 = result2[[counter]] = arima(res2, vec[i,])
print(sqrt(sum(fit2$residuals^2)))




fit1= result1[[counter]] = arima(res1, c(2,0,0))
print(sqrt(sum(fit1$residuals^2)))
fit2 = result2[[counter]] = arima(res2, c(2,0,0))
print(sqrt(sum(fit2$residuals^2)))

fit1= result1[[counter]] = arima(res1, c(2,1,0))
print(sqrt(sum(fit1$residuals^2)))
fit2 = result2[[counter]] = arima(res2, c(2,1,0))
print(sqrt(sum(fit2$residuals^2)))

fit1= result1[[counter]] = arima(res1, c(2,0,1))
print(sqrt(sum(fit1$residuals^2)))
fit2 = result2[[counter]] = arima(res2, c(2,0,1))
print(sqrt(sum(fit2$residuals^2)))



fit1= result1[[counter]] = arima(res1, c(3,0,0))
print(sqrt(sum(fit1$residuals^2)))
fit2 = result2[[counter]] = arima(res2, c(3,0,0))
print(sqrt(sum(fit2$residuals^2)))

fit1= result1[[counter]] = arima(res1, c(3,1,0))
print(sqrt(sum(fit1$residuals^2)))
fit2 = result2[[counter]] = arima(res2, c(3,1,0))
print(sqrt(sum(fit2$residuals^2)))


i = 4;
vec[i,]
fit1= result1[[counter]] = arima(res1, vec[i,])
print(sqrt(sum(fit1$residuals^2)))
fit2 = result2[[counter]] = arima(res2, vec[i,])
print(sqrt(sum(fit2$residuals^2)))

i = 5;
vec[i,]
fit1= result1[[counter]] = arima(res1, vec[i,])
print(sqrt(sum(fit1$residuals^2)))
fit2 = result2[[counter]] = arima(res2, vec[i,])
print(sqrt(sum(fit2$residuals^2)))




fit1= result1[[counter]] = arima(res1, c(4,0,0))
print(sqrt(sum(fit1$residuals^2)))
fit2 = result2[[counter]] = arima(res2, c(4,0,0))
print(sqrt(sum(fit2$residuals^2)))

fit1= result1[[counter]] = arima(res1, c(4,1,0))
print(sqrt(sum(fit1$residuals^2)))
fit2 = result2[[counter]] = arima(res2, c(4,1,0))
print(sqrt(sum(fit2$residuals^2)))

i = 6;
vec[i,]
fit1= result1[[counter]] = arima(res1, vec[i,])
print(sqrt(sum(fit1$residuals^2)))
fit2 = result2[[counter]] = arima(res2, vec[i,])
print(sqrt(sum(fit2$residuals^2)))

i = 7;
vec[i,]
fit1= result1[[counter]] = arima(res1, vec[i,])
print(sqrt(sum(fit1$residuals^2)))
fit2 = result2[[counter]] = arima(res2, vec[i,])
print(sqrt(sum(fit2$residuals^2)))


fit1= result1[[counter]] = arima(res1, c(5,0,0))
print(sqrt(sum(fit1$residuals^2)))
fit2 = result2[[counter]] = arima(res2, c(5,0,0))
print(sqrt(sum(fit2$residuals^2)))

fit1= result1[[counter]] = arima(res1, c(5,1,0))
print(sqrt(sum(fit1$residuals^2)))
fit2 = result2[[counter]] = arima(res2, c(5,1,0))
print(sqrt(sum(fit2$residuals^2)))


i = 8;
vec[i,]
fit1= result1[[counter]] = arima(res1, vec[i,])
print(sqrt(sum(fit1$residuals^2)))
fit2 = result2[[counter]] = arima(res2, vec[i,])
print(sqrt(sum(fit2$residuals^2)))

i = 9;
vec[i,]
fit1= result1[[counter]] = arima(res1, vec[i,])
print(sqrt(sum(fit1$residuals^2)))
fit2 = result2[[counter]] = arima(res2, vec[i,])
print(sqrt(sum(fit2$residuals^2)))








# 5, 0, 1
# 5, 1, 1

fit1 = result1[[counter]] = arima(res1, c(5, 0, 1))
fit2 = result2[[counter]] = arima(res2, c(5, 0, 1))

predict(fit1, 8)
predict(fit2, 8)

fit1 = result1[[counter]] = arima(res1, c(5, 1, 1))
fit2 = result2[[counter]] = arima(res2, c(5, 1, 1))

pred1 = predict(fit1, 8)
cbind(pred1$pred-pred1$se, pred1$pred, pred1$pred+pred1$se)

pred2 = predict(fit2, 8)
cbind(pred2$pred-pred2$se, pred2$pred, pred2$pred+pred2$se)


plot(pred1$pred, col="red", main="ARIMA(5,1,1) of 직업코드 2")
points(pred2$pred, col="blue", main="ARIMA(5,1,1) of 직업코드 9")

par(mfrow = c(4, 1))
plot(forecast(fit1), col="red", main="ARIMA(5,1,1) of 직업코드 2")
plot(forecast(fit2), col="blue", main="ARIMA(5,1,1) of 직업코드 9")
Acf(residuals(fit1), main="ARIMA(5,1,1) of 직업코드 2")
Acf(residuals(fit2), main="ARIMA(5,1,1) of 직업코드 9")
