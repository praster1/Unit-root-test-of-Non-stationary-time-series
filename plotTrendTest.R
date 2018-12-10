plotTrendTest = function(dataVec, type = "none")
{
    ### Cox-Stuart Trend Test
    source("cox_stuart_test.R")
    source("cox_stuart_test_inc.R")
    source("cox_stuart_test_des.R")

    unlistDataVec = unlist(dataVec)
    len = length(dataVec$data)
    coxres = lapply(dataVec$data, cox_stuart_test)
    for (i in 1:len)
    {
        if (as.numeric(coxres[[i]]$statistic) < signif_Trend)
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
