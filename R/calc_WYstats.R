calc_WYstats <- function(data, runoffIndex, segOutflowIndex,
                         writeWYstats = FALSE, outFile)
{
  # add concatinated date and WY
  data$date <- as.Date(paste(data$year, data$month, data$day, sep = "-"))
  data$WY <- smwrBase::waterYear(data$date, numeric = TRUE)
  
  # extract observed (runoff parameter) and simulated (seg_outflow) data sets
  data <- data[c("date", "WY", 
                 paste("runoff", runoffIndex, sep = "_"),
                 paste("seg_outflow", segOutflowIndex, sep = "_"))]
  names(data) <- c("date", "WY", "obs", "sim")
  
  ### NOT WORKING####
  wystats <- dplyr::summarise(dplyr::group_by(data, WY), 
                              nse = hydroGOF::NSE(sim, obs), 
                              rmse = hydroGOF::rmse(sim, obs), 
                              volE = hydroGOF::VE(sim, obs))
  
  
}
