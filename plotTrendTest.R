plotTrendTest = function(dataVec, type = "none", signIf = 0.05)
{
    ### Cox-Stuart Trend Test
    source("cox_stuart_test.R")
    source("cox_stuart_test_inc.R")
    source("cox_stuart_test_des.R")

	source("plotTrendTest_returnStatistic.R")
	
    unlistDataVec = unlist(dataVec)
    coxres = lapply(dataVec$data, cox_stuart_test)
	
	testStats = lapply(coxres, plotTrendTest_returnStatistic)
	which_testStats = which(temp < signIf)
	
    for (i in which_testStats)
    {
        if (as.numeric(coxres[[i]]$statistic) < signIf)
        {
            if (names(coxres[[i]]$statistic) == "Increasing trend, p-value")
            {
                if ( (type == "inc") || (type == "none") )
                {
                    rect(min(dataVec$index[[i]]), min(unlistDataVec), max(dataVec$index[[i]]), max(unlistDataVec), col="lightpink", lty=0)
                }
                else
                {
                    rect(min(dataVec$index[[i]]), min(unlistDataVec), max(dataVec$index[[i]]), max(unlistDataVec), col="white", lty=0)
                }
            }
            else
            {
                if ( (type == "des") || (type == "none") )
                {
                    rect(min(dataVec$index[[i]]), min(unlistDataVec), max(dataVec$index[[i]]), max(unlistDataVec), col="lightblue", lty=0)
                }
                else
                {
                    rect(min(dataVec$index[[i]]), min(unlistDataVec), max(dataVec$index[[i]]), max(unlistDataVec), col="white", lty=0)
                }
            }
        }
    }
}
