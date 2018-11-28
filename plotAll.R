plotAll = function(dataVec, datetimeVec, main = "Main", xlab="", ylab="power consumption(kWh)")
{
	# init
    par(mfrow = c(1, 1))
	plot(dataVec, type="l", main=main, xlab=xlab, ylab=ylab, axes=FALSE)

	# y축 axis
	axis(2, dataVec, labels=seq(0, max(dataVec), by=50), at=seq(0, max(dataVec), by=50))

	# x축 axis  년도
    source("getIndexVec.R")  # dataVec의 unique 값이 위치하는 최소/최대 index를 출력한다.
    year = getUniqVec(datetime, index="YYYY")
    yearIndex = getIndexVec(year, func="min")
	
    axis(1, yearIndex, labels = unique(year), line=1)
	mtext("Year",1,line=1,at=0.2)

	# x축 axis: 월
	yearmonth = getUniqVec(datetime, index="YYYYMM")	
    yearmonthIndex = getIndexVec(yearmonth, func="min")
	
	monthVec = substr(unique(yearmonth), 6, 8)
	
	axis(1, yearmonthIndex, labels = monthVec, line=3)
	mtext("Month", 1, line=3, at=0.2)
}