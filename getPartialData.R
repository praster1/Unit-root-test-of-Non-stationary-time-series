# dataVec을 stepSize만큼 건너뛰면서 partialLength씩 자른다.
getPartialData = function(dataVec, partialLength, stepSize)
{
    if (is.null(partialLength))         {        stop("partialLength is NULL.");    }
    if (is.null(stepSize))               {        stop("stepSize is NULL.");    }
    if (partialLength < stepSize)    {        stop("partialLength must greater than stepSize");    }
    if (partialLength > dataVec)    {        stop("dataVec must greater than partialLength");    }
    
    dataLen = length(dataVec)
    
    res = NULL;

    i = 1;  resCounter = 1
    while(max(i+partialLength-1) < dataLen)
    {
        res[[resCounter]] = dataVec[i:(i+partialLength-1)]
        i = i + stepSize
        resCounter = resCounter + 1;
    }
    
    return(res)
}
