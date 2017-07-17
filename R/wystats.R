setwd("R:/prms/20161201_Colin_Penn_RioGrande/prms_win/projects/RioGrande_Daymet/output/")
library(smwrBase)
library(hydroGOF)
library(dplyr)

# read in statVar file. Will need to adjust skip depending on number of output variables and spinup time.
flow <- read.table(file = "rghw_Daymet.statvar", skip = 296, sep = " ")
names(flow) <- c("rec", "y", "m", "d", "hr", "min", "sec",
                 "seg_outflow9", "runoff6",
                 "seg_outflow5", "runoff1",
                 "seg_outflow7", "runoff3",
                 "seg_outflow13", "runoff4",
                 "seg_outflow14", "runoff5",
                 "basin_potet", "basin_actet",
                 "basin_snowcov", "basin_pweqv", "basin_snowevap", "basin_intcp_evap",
                 "basin_snow", "basin_rain", "basin_ppt", "basin_soil_moist", "basin_total_storage", "empty")
flow$Date <- as.Date(paste(flow$y, flow$m, flow$d, sep = "-"))
flow$WY <- waterYear(flow$Date, numeric = T)

# calculate model stats by WY
WYstats <- flow %>%
  group_by(WY) %>%
  summarise(nse = hydroGOF::NSE(seg_outflow9,runoff6), rmse = hydroGOF::rmse(seg_outflow9,runoff6), volE = hydroGOF::VE(seg_outflow9,runoff6))

write.csv(WYstats, file = "prms_wystats.csv", row.names = F, quote = F)

# calculate sub-basin model stats by WY
riomilco <- flow %>%
  group_by(WY) %>%
  summarise(nse = hydroGOF::NSE(seg_outflow5,runoff1), rmse = hydroGOF::rmse(seg_outflow5,runoff1), volE = hydroGOF::VE(seg_outflow5,runoff1))

riowagco <- flow %>%
  group_by(WY) %>%
  summarise(nse = hydroGOF::NSE(seg_outflow7,runoff3), rmse = hydroGOF::rmse(seg_outflow7,runoff3), volE = hydroGOF::VE(seg_outflow7,runoff3))

goowagco <- flow %>%
  group_by(WY) %>%
  summarise(nse = hydroGOF::NSE(seg_outflow13,runoff4), rmse = hydroGOF::rmse(seg_outflow13,runoff4), volE = hydroGOF::VE(seg_outflow13,runoff4))

riosfkco <- flow %>%
  group_by(WY) %>%
  summarise(nse = hydroGOF::NSE(seg_outflow14,runoff5), rmse = hydroGOF::rmse(seg_outflow14,runoff5), volE = hydroGOF::VE(seg_outflow14,runoff5))

# calculate annual flow and stat by WY
annFlow <- flow %>%
  group_by(WY) %>%
  summarise(obs_ft3 = (sum(runoff6)*60*60*24), sim_ft3 = (sum(seg_outflow9)*60*60*24))
annFlow$obs_acft <- annFlow$obs_ft3*2.29569e-5
annFlow$sim_acft <- annFlow$sim_ft3*2.29569e-5
annFlow$diff_acft <- annFlow$sim_acft - annFlow$obs_acft 
pb <- annFlow %>%
  group_by(WY) %>%
  summarise(perBias = hydroGOF::pbias(sim_acft,obs_acft))
annFlow <- left_join(annFlow,pb,by = "WY")

write.csv(annFlow, file = "prms_annFlow.csv", row.names = F, quote = F)

# calculate water supply (Apr-Sept) flows and stats by WY
seasonal <- flow %>%
  filter(m %in% c('4','5','6','7','8','9')) %>%
  group_by(WY) %>%
  summarise(nse = hydroGOF::NSE(seg_outflow9,runoff6), rmse = hydroGOF::rmse(seg_outflow9,runoff6), volE = hydroGOF::VE(seg_outflow9,runoff6), 
            obs_ft3 = (sum(runoff6)*60*60*24), sim_ft3 = (sum(seg_outflow9)*60*60*24))
seasonal$obs_acft <- seasonal$obs_ft3*2.29569e-5
seasonal$sim_acft <- seasonal$sim_ft3*2.29569e-5
seasonal$diff_acft <- seasonal$sim_acft - seasonal$obs_acft 
pb2 <- seasonal %>%
  group_by(WY) %>%
  summarise(perBias = hydroGOF::pbias(sim_acft,obs_acft))
seasonal <- left_join(seasonal,pb2,by = "WY")
    
write.csv(seasonal, file = "prms_seasonal.csv", row.names = F, quote = F)  


# graphical GOF plot
hydroGOF::ggof(flow$seg_outflow9, flow$runoff6, dates = flow$Date, xlab = c("RIODELCO"), ylab = c("Q, [ft3/s]"))
