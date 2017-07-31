#' read_Statvar
#' @description Function to read the statvar file output by PRMS
#' @param file Character, pathname to statvar file
#' @param NAval Integer, value of missing data in statvar/data file (usually -999) 
#' @details Variables and index numbers of the statvar file are determined by the user in the PRMS control file
#' prior to simulation. This function reads the statvar header with variables and index number, and outputs a \code{dataframe}
#' with column titles corresponding to the statvar elements
#' @examples 
#' exampleData <- read_Statvar(file = data/rghw_Daymet.statvar)
#' @seealso \code{\link[prmsTools]{calc_WYstats}}, 
#' @export
#' @return A dataframe containing the statvar output with proper column titles

read_Statvar <- function(file, NAval = -999)
{
  # number of variables is stored in first line of statvar file
  nvars <- as.numeric(read.table(file = file, sep = " ", nrows = 1))
  
  # names of variable and element number are stored in lines 2 to nvars+1 (nrows=nvars because of skip)
  varNames <- read.table(file = file, sep = " ", skip = 1, nrows = nvars)
  # create variable_elementNumber column
  varNames$vars <- paste(varNames$V1, varNames$V2, sep = "_")
  
  # read in statvar file and rename columns
  statvar <- read.table(file = file, sep = " ", skip = (nvars+1))
  names(statvar) <- c("id", "year", "month", "day", "hour", "min", "sec", varNames$vars)
  
  # replace no data values with NA
  statvar[statvar == NAval] <- NA
  
  return(statvar)
}

