#' calc_WYstats
#' @description Function to calculate NSE, RMSE, and VE goodness-of-fit statistics by WY
#' @param data A \code{dataframe} output from \code{read_Statvar}
#' @param runoffIndex Integer, index number of observed gage flow in PRMS input data file (and subsequently variable name in statvar file)
#' @param segOutflowIndex Integer, index number of segment corresponding to observed gage location (and subsequently variable name in statvar file)
#' @param writeWYstats Logical, if \code{TRUE} write .csv file of WY stats.
#' @param outFile Character, pathname and filename of .csv file if \code{writeWYstats = TRUE} 
#' @details Uses gage data and segment data from imported statvar file. Note that segments and runoff data indexes
#' must be specified in the control file under statVar_element and statVar_names before PRMS is run,
#' and the statvar file read into R using the \code{read_Statvar} function for this function to work best.
#' If the data is in another format (ex. pre-formatted table with just obs/sim) it's best just to use the \code{hydroGOF} package 
#' @details Uses documentation in \code{hydroGOF} package for calculating the following statistics by WY:
#' @details NSE: Nash-Sutcliffe Efficiency
#' @details RMSE: Root Mean Square Error
#' @details VE: Volumetric Efficiency
#' @details See \code{hydroGOF} package for more documentation and references
#' @examples
#' exampleData <- read_Statvar(file = "data/rghw_Daymet.statvar")
#' basinStats <- calc_WYstats(data = exampleData, runoffIndex = 6, segOutflowIndex = 9)
#' @importFrom smwrBase waterYear
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @seealso \code{\link[prmsTools]{read_Statvar}}, \code{\link[hydroGOF]{NSE}},
#' \code{\link[hydroGOF]{rmse}}, \code{\link[hydroGOF]{VE}} 
#' @export
#' @return A dataframe containing goodness-of-fit statistics by water year

calc_WYstats <- function(data, runoffIndex, segOutflowIndex,
                         writeWYstats = FALSE, outFile = NULL)
{
  # add concatinated date and WY
  data$date <- as.Date(paste(data$year, data$month, data$day, sep = "-"))
  data$WY <- smwrBase::waterYear(data$date, numeric = TRUE)
  
  # extract observed (runoff parameter) and simulated (seg_outflow) data sets
  data <- data[c("date", "WY", 
                 paste("runoff", runoffIndex, sep = "_"),
                 paste("seg_outflow", segOutflowIndex, sep = "_"))]
  names(data) <- c("date", "WY", "obs", "sim")
  
  ### calc stats
  wystats <- dplyr::summarise(dplyr::group_by(data, WY),
                              nse = 1 - ( sum( (obs - sim)^2 ) / sum( (obs - mean(obs, na.rm = TRUE))^2 )),
                              rmse = sqrt( mean( (sim - obs)^2, na.rm = TRUE) ),
                              volE = 1 - ( sum( abs(obs - sim) ) / sum( obs ) ))
  # wystats <- dplyr::summarise(dplyr::group_by(data, WY), 
  #                             nse = hydroGOF::NSE(sim, obs), 
  #                             rmse = hydroGOF::rmse(sim, obs), 
  #                             volE = hydroGOF::VE(sim, obs))
  
  if(writeWYstats == TRUE)
  {
    write.table(x = wystats, file = outFile, quote = FALSE, sep = ",", 
                row.names = FALSE, col.names = TRUE)
  }
  
  return(wystats)
  
}
