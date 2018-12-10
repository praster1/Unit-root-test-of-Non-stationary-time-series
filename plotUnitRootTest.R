plotUnitRootTest = function(dataVec, analysisResult, critVal = 3)
{
    len = length(dataVec$data)
    for (i in 1:len)
    {
        testStat = analysisResult[[i]]@teststat[1]
        cval = analysisResult[[i]]@cval[1,critVal]
        print(paste("i:", i, "/", len, "     ", testStat < cval))
        if (testStat < cval)
        {
            points(dataVec$index[[i]], dataVec$data[[i]], type="l", col="red", lwd=5);	
        }
        else
        {
            points(dataVec$index[[i]], dataVec$data[[i]], type="l", col="black");	
        }
    }
}
