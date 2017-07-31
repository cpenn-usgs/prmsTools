#' calc_WYstats
#' @description Function to calculate NSE, RMSE, and VE goodness-of-fit statistics by WY
#' @param data A \code{dataframe} output from \code{read_Statvar}
#' @param runoffIndex Integer, index number of observed gage flow in PRMS input data file
#' @param segOutflowIndex Integer, index number of segment corresponding to observed gage location
#' @param writeWYstats Logical, if \code{TRUE} write .csv file of WY stats.
#' @param outFile Character, pathname and filename of .csv file if \code{writeWYstats = TRUE} 
#' @details uses the \code{hydroGOF} package for calculating the following statistics by WY:
#' @details NSE: Nash-Sutcliffe Efficiency
#' RMSE: Root Mean Square Error
#' VE: Volumetric Efficiency
#' See \code{hydroGOF} package for more documentation and references
#' @examples
#' \dontrun{
#' data("exampleData",package="prmsTools")
#' x <- exampleData
#' basinStats <- calc_WYstats(data = x, runoffIndex = 6, segOutflowIndex = 9)}
#' @importFrom smwrBase waterYear
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @seealso \code{\link[prmsTools]{read_Statvar}}, 
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
