# split의 시작값, 종료값, 평균값, 중앙값 등을 구하는 함수
getCalcVec = function(dataVec, datetimeVec, indexVec, calc="last")
{
    # unique index
    uniq = unique(indexVec)
    uniq_len = length(uniq)
    
    resVec = NULL;
    for (i in 1:uniq_len)
    {
        uniqIndex = which(indexVec == uniq[i])   # indexVec에 따라 일자 선택
        temp = dataVec[uniqIndex]   # 선택된 일자에 해당하는 dataVec
        
        if (calc == "first") {              # 시작값
            resVec = c(resVec, temp[1])
        } else if (calc == "last") {    # 마지막값
            resVec = c(resVec, temp[length(temp)])
        } else if (calc == "min") {
            resVec = c(resVec, min(temp))
        } else if (calc == "max") {
            resVec = c(resVec, max(temp))
        } else if (calc == "mean") {
            resVec = c(resVec, mean(temp))
        } else if (calc == "median") {
            resVec = c(resVec, median(temp))
        } else if (calc == "stddev") {
            resVec = c(resVec, sd(temp))
        }
    }

    return(resVec)
}