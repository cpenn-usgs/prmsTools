#' read_nhruOut
#' @description Function to read the nhru summary files output by PRMS
#' @param nhruOutBaseFileName Character, base file name for nhru summary files specified in the PRMS control file. Note
#' this should also contain the pathname to Output folder containing nhru summary .csv files.
#' @param nhruOutVar_names Character vector of variable names to read, based on variables specified in the PRMS control file.
#' @param nhru Integer, number of HRUs in PRMS model
#' @details A function for reading in nhru summary output files. Note that in the PRMS control file, nhruOutON_OFF must
#' be set to 1 for .csv files to be output.
#' @examples
#' exampleData <- read_nhruOut(nhruBaseFileName = "data/rghw_Daymet_nhru_summary_",
#'                             nhruOutVar_names = c("hru_rain", "hru_snow", 
#'                             nhru = 28))
#' rain <- exampleData$hru_rain
#' @export
#' @return A list of dataframes containing the nhru summary output. List elements are each variable specified in nhruOutVar_names

read_nhruOut <- function(nhruOutBaseFileName = "",
                         nhruOutVar_names,
                         nhru)
{
  # combine path and basename to variable names and create list
  files <- as.list(paste0(nhruOutBaseFileName, nhruOutVar_names,".csv"))
  
  # read nhru summary .csv files into list of dataframes
  colClass <- c("Date")
  colClass[2:nhru+1] <- NA
  colNames <- c("Date")
  colNames[1:nhru+1] <- paste("hru", seq_len(nhru), sep = "_")
  nhruSumm <- lapply(files, function(i){
    read.csv(i, header = TRUE, colClasses = colClass, col.names = colNames)})
  names(nhruSumm) <- nhruOutVar_names
  
  return(nhruSumm)
}
