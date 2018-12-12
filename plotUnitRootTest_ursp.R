plotUnitRootTest_ursp = function(dataVec, analysisResult, print=FALSE, lwd=5)
{
    source("plotUnitRootTest_returnCval.R")
	source("plotUnitRootTest_returnTeststat.R")
	
	testStats = as.numeric(lapply(analysisResult, plotUnitRootTest_returnTeststat))
	cVals = as.numeric(lapply(analysisResult, plotUnitRootTest_returnCval))
	
	which_testStats = which(testStats < cVals)
	
    for (i in which_testStats)
    {
		points(dataVec$index[[i]], dataVec$data[[i]], type="l", col="red", lwd=lwd);	
    }
}
