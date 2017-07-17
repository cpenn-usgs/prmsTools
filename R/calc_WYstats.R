calc_WYstats <- function(data, runoffIndex, segOutflowIndex)
{
  # add concatinated date and WY
  data$date <- as.Date(paste(data$year, data$month, data$day, sep = "-"))
  data$WY <- smwrBase::waterYear(data$date, numeric = TRUE)
  
  
  
  
  
  
  
}
