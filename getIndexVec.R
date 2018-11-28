# dataVec의 unique 값이 위치하는 최소/최대 index를 출력한다.
getIndexVec = function(dataVec, func="min")
{
    uniq = unique(dataVec)
    uniq_len = length(uniq)
    
    resVec = NULL;
    for (i in 1:uniq_len)
    {
        if (func=="min") {
            resVec = c(resVec, min(which(dataVec == uniq[i])))
        } else if (func=="min") {
            resVec = c(resVec, max(which(dataVec == uniq[i])))
        }
    }
    
    return(resVec)
}