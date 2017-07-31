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

