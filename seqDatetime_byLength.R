# 시작일(startDate)부터 길이(length)만큼 날짜 벡터 구하기
seqDatetime_byLength = function(startDate = "2000-01-01", length = 100, split = 96)
{
	# split은 0보다 커야 한다.
	if (split <= 0)					{	stop("split must be greater than 0.")		}
	
	# length은 0보다 커야 한다.
	if (length <= 0)				{	stop("length must be greater than 0.")	}

	
	# YYYY-MM-DD HH:MM:SS 타입으로 전환
	startDate = as.POSIXct(startDate)

	# seqData 생성
	splitDates = as.POSIXct("0000-01-01")
	while(length(splitDates) <= length)
	{
		plusTime = seq(1, (60*60*24), length=split)
		splitDates = c(splitDates, startDate + plusTime)
		startDate = startDate + (60*60*24);
	}
	
	splitDates = unique(splitDates)
	splitDates = sort(splitDates)
    splitDates = splitDates[-1]
	return(splitDates[1:length])
}