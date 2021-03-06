% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_seasonalstats.R
\name{calc_seasonalstats}
\alias{calc_seasonalstats}
\title{calc_seasonalstats}
\usage{
calc_seasonalstats(data, seasMonths, runoffIndex, segOutflowIndex,
  writeWYstats = FALSE, outFile = NULL)
}
\arguments{
\item{data}{A \code{dataframe} output from \code{read_Statvar}}

\item{seasMonths}{An array of selected months (numerical format, eg. May is 5)}

\item{runoffIndex}{Integer, index number of observed gage flow in PRMS input data file (and subsequently variable name in statvar file)}

\item{segOutflowIndex}{Integer, index number of segment corresponding to observed gage location (and subsequently variable name in statvar file)}

\item{writeWYstats}{Logical, if \code{TRUE} write .csv file of WY stats.}

\item{outFile}{Character, pathname and filename of .csv file if \code{writeWYstats = TRUE}}
}
\value{
A dataframe containing goodness-of-fit statistics by water year
}
\description{
Function to calculate NSE, RMSE, and VE goodness-of-fit statistics over a period of selected months
}
\details{
Uses gage data and segment data from imported statvar file. Note that segments and runoff data indexes
must be specified in the control file under statVar_element and statVar_names before PRMS is run,
and the statvar file read into R using the \code{read_Statvar} function for this function to work best.
If the data is in another format (ex. pre-formatted table with just obs/sim) it's best just to use the \code{hydroGOF} package

Uses documentation in \code{hydroGOF} package for calculating the following statistics by WY:

NSE: Nash-Sutcliffe Efficiency

RMSE: Root Mean Square Error

VE: Volumetric Efficiency

pbias: Percent Bias

See \code{hydroGOF} package for more documentation and references
}
\examples{
exampleData <- read_Statvar(file = "data/rghw_Daymet.statvar")
apr_sept_Stats <- calc_seasonalstats(data = exampleData, seasMonths = c(4,5,6,7,8,9),
runoffIndex = 6, segOutflowIndex = 9)
}
\seealso{
\code{\link[prmsTools]{read_Statvar}}, \code{\link[hydroGOF]{NSE}},
\code{\link[hydroGOF]{rmse}}, \code{\link[hydroGOF]{VE}}, \code{\link[hydroGOF]{pbias}}
}
