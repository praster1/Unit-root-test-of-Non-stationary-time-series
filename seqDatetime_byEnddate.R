# 시작일(startDate)부터 종료일(endDate) 직전까지 날짜 벡터 구하기
seqDatetime_byEnddate = function(startDate = "2000-01-01", endDate = "2000-01-02", split = 96)
{
	# YYYY-MM-DD HH:MM:SS 타입으로 전환
	startDate = as.POSIXct(startDate)
	endDate = as.POSIXct(endDate)

	
	# endDate는 startDate보다 커야 한다.
	if (startDate >= endDate)	{	stop("endDate must be greater than startDate.");	}
	
	# split은 0보다 커야 한다.
	if (split <= 0)					{	stop("split must be greater than 0.")					}
	
	# seqData 생성
	splitDates = startDate;
	while(startDate <= endDate)
	{
		plusTime = seq(1, (60*60*24), length=split)
		splitDates = c(splitDates, startDate + plusTime)
		startDate = startDate + (60*60*24);
	}
	
	splitDates = unique(splitDates)
	splitDates = splitDates[which(splitDates < endDate)]
	return(sort(splitDates))
}
