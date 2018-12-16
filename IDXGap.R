IDXGap = function(dataVecIndexs, analysisResult, outlierIndexs, critVal = 3, testReverse=FALSE, ursp=FALSE)
{
	source("plotUnitRootTest_returnCval.R")
	source("plotUnitRootTest_returnCval_ursp.R")
	source("plotUnitRootTest_returnTeststat.R")

	testStats = as.numeric(lapply(analysisResult, plotUnitRootTest_returnTeststat))
	cVals = NULL;
	if (ursp)
	{
		cVals = as.numeric(lapply(analysisResult, plotUnitRootTest_returnCval_ursp))
	} else {
		cVals = as.numeric(lapply(analysisResult, plotUnitRootTest_returnCval, critVal=critVal))
	}
	
	

	which_testStats = NULL
	if (!testReverse) {
		which_testStats = which(testStats < cVals)
	} else {
		which_testStats = which(testStats > cVals)
	}

	critValIDX = NULL
	for (i in 1:length(dataVecIndexs))
	{
		if (sum((dataVecIndexs[[i]] == outlierIndexs)*1) > 0)				{			critValIDX = c(critValIDX, i)				}
	}
	
	resIDXGap = 9999999
	for (i in 1:length(critValIDX))
	{
		temp = min(abs(which_testStats - critValIDX[i]))
		if (temp < resIDXGap)		{			resIDXGap = temp		}
	}
	
	return(resIDXGap)
}