plotUnitRootTest_urdf = function(dataVec, analysisResult, critVal = 3, print=FALSE)
{
	source("plotUnitRootTest_returnCval.R")
	source("plotUnitRootTest_returnTeststat.R")
	
	testStats = lapply(analysisResult, plotUnitRootTest_returnTeststat)
	cVals = lapply(analysisResult, plotUnitRootTest_returnCval, critVal=critVal)
	
	which_testStats = which(testStat < cVals)
	
    for (i in which_testStats)
    {
		points(dataVec$index[[i]], dataVec$data[[i]], type="l", col="red", lwd=5);	
    }
}
