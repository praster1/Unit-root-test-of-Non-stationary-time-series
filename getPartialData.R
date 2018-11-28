# dataVec을 stepSize만큼 건너뛰면서 partialLength씩 자른다.
# return: data, index
getPartialData = function(dataVec, partialLength, stepSize)
{
    if (is.null(partialLength))         {        stop("partialLength is NULL.");    }
    if (is.null(stepSize))               {        stop("stepSize is NULL.");    }
    if (partialLength < stepSize)    {        stop("partialLength must greater than stepSize");    }
    if (partialLength > dataVec)    {        stop("dataVec must greater than partialLength");    }
    
    dataLen = length(dataVec)
    
    resData = NULL;
    resIndex = NULL;

    i = 1;
    resDataCounter = 1
    while(max(i+partialLength-1) < dataLen)
    {
        resIndex[[resDataCounter]] = i:(i+partialLength-1)
        resData[[resDataCounter]] = dataVec[resIndex[[resDataCounter]]]
        i = i + stepSize
        resDataCounter = resDataCounter + 1;
    }
    
    res = list(data=resData, index=resIndex)
    return(res)
}
