#' read_inputData
#' @description Function to read the input data file used in a PRMS simulation
#' @param file Character, pathname to input data file
#' @param nheader Integer, number of header lines, usually containing created by and station metadata, in the file
#' @param nvars Integer, number of unique variables (ex. runoff, tmin, tmax; nvars=3) in the data file
#' @param NAval Integer, value of missing data in statvar/data file (usually -999) 
#' @details Variables and station numbers of the data file are determined by the \code{variable numberOfStations}
#' which is the format read by PRMS and setup by the user prior to a simulation. This function will assign each
#' timeseries the column title of variable_stationNumber in the order that they are listed.
#' @examples
#' exampleData <- read_inputData(file = "data/rghw_ALL.data")
#' @seealso \code{\link[prmsTools]{calc_WYstats}}, 
#' @export
#' @return A dataframe containing the input data file with varialbe_stationNumber column titles


read_inputData <- function(file,
                           nheader,
                           nvars,
                           NAval = -999)
{
  # names of variables and number of elements are stored after header to nvars+1 (nrows=nvars because of skip)
  varNames <- read.table(file = file, sep = " ", skip = nheader, nrows = nvars)
  
  # create variable_elementNumber char array
  varNamesA <- NULL
  indx <- 1
  for(i in 1:nvars)
  {
    for(j in 1:varNames$V2[i])
    {
      varNamesA[indx] <- paste(varNames$V1[i], j, sep = "_")
      indx <- indx +1
    }
  }
  
  # read in data file and rename columns
  datafile <- read.table(file = file, sep = " ", skip = (nheader+nvars+1))
  names(datafile) <- c("year", "month", "day", "hour", "min", "sec", varNamesA)
  
  # replace no data values with NA
  datafile[datafile == NAval] <- NA
  
  return(datafile)
}
