rm(list = ls())



setwd("/home/lv999/Dropbox/Github/Unit-root-test-of-Non-stationary-time-series")
# setwd("E:/Dropbox/Github/Unit-root-test-of-Non-stationary-time-series")

IDXGap_ADFTrend = read.csv("./Results_20181217_IDXGap_Experiments/03_IDXGap_ADFTrend.csv")[,-1]
IDXGap_ADFDrift = read.csv("./Results_20181217_IDXGap_Experiments/03_IDXGap_ADFDrift.csv")[,-1]
IDXGap_PPTrend = read.csv("./Results_20181217_IDXGap_Experiments/03_IDXGap_PPTrend.csv")[,-1]
IDXGap_PPDrift = read.csv("./Results_20181217_IDXGap_Experiments/03_IDXGap_ADFTrend.csv")[,-1]
IDXGap_ERSDGTrend = read.csv("./Results_20181217_IDXGap_Experiments/03_IDXGap_PPDrift.csv")[,-1]
IDXGap_ERSDGConst = read.csv("./Results_20181217_IDXGap_Experiments/03_IDXGap_ERSDGConst.csv")[,-1]
IDXGap_ERSPTrend = read.csv("./Results_20181217_IDXGap_Experiments/03_IDXGap_ERSPTrend.csv")[,-1]
IDXGap_ERSPConst = read.csv("./Results_20181217_IDXGap_Experiments/03_IDXGap_ERSPConst.csv")[,-1]
IDXGap_SPTau = read.csv("./Results_20181217_IDXGap_Experiments/03_IDXGap_SPTau.csv")[,-1]
IDXGap_SPRho = read.csv("./Results_20181217_IDXGap_Experiments/03_IDXGap_SPRho.csv")[,-1]
IDXGap_KPSSTau = read.csv("./Results_20181217_IDXGap_Experiments/03_IDXGap_KPSSTau.csv")[,-1]


apply(IDXGap_ADFTrend, 2, sum)
apply(IDXGap_ADFDrift, 2, sum)
apply(IDXGap_PPTrend, 2, sum)
apply(IDXGap_PPDrift, 2, sum)
apply(IDXGap_ERSDGTrend, 2, sum)
apply(IDXGap_ERSDGConst, 2, sum)
apply(IDXGap_ERSPTrend, 2, sum)
apply(IDXGap_ERSPConst, 2, sum)
apply(IDXGap_SPTau, 2, sum)
apply(IDXGap_SPRho, 2, sum)
apply(IDXGap_KPSSTau, 2, sum)

round(apply(IDXGap_ADFTrend, 2, sd), 2)
round(apply(IDXGap_ADFDrift, 2, sd), 2)
round(apply(IDXGap_PPTrend, 2, sd), 2)
round(apply(IDXGap_PPDrift, 2, sd), 2)
round(apply(IDXGap_ERSDGTrend, 2, sd), 2)
round(apply(IDXGap_ERSDGConst, 2, sd), 2)
round(apply(IDXGap_ERSPTrend, 2, sd), 2)
round(apply(IDXGap_ERSPConst, 2, sd), 2)
round(apply(IDXGap_SPTau, 2, sd), 2)
round(apply(IDXGap_SPRho, 2, sd), 2)
round(apply(IDXGap_KPSSTau, 2, sd), 2)