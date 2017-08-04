


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
