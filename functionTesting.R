# library(hydroGOF)
# library(dplyr)
# library(smwrBase)
# 
# source("R/read_Statvar.R")
# source("R/calc_WYstats.R")
# x <- read_Statvar(file = "data/rghw_Daymet.statvar")
# wystats <- calc_WYstats(data = x, runoffIndex = 6, segOutflowIndex = 9,
#                         writeWYstats = T, outFile = "data/test.csv")
# ggof(sim = x$seg_outflow_7, obs = x$runoff_3)
