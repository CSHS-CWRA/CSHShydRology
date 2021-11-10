#' Retrieve AHCCD data from EC datamart
#'
#' @param station Required. The station number - either as numeric or as a string.
#' @param province Required. Name of province/territory. Must one of AB, BC, MB, NB, NL, NS, NT, NU, ON, PE, QC, SK, YT.
#' @param variable Required. Must be one of 
#' \describe{
#'  \item{variable}{meaning}
#'  \item{PCP}{total precipitation}
#'  \item{RA}{rainfall}
#'  \item{SN}{snowfall}
#'  \item{TMAX}{max air temp}
#'  \item{TMEAN}{mean air temp}
#'  \item{TMIN}{max air temp}
#'  \item{PSFC}{surface air pressure}
#'  \item{SFCWND}{surface wind speed}
#'  \item{SLP}{sea level pressure}
#'  }
#' @param url Required. The default url currently works to access the data on the 
#' Environment Canada server. The url can be changed in case the site is moved.

#' @return Returns a data frame with the monthly values and associated variables.
#' @author Kevin Shook
#' @seealso  \code{\link{ch_read_AHCCD_daily}} \code{\link{ch_read_AHCCD_monthly}}
#' @references Use of the data must cite \cite{Mekis, E and L.A. Vincent, 2011: An overview of 
#' the second generation adjusted daily temperature and precipitation dataset for trend analysis in Canada. 
#' Atmosphere-Ocean, 49 (2), 163-177.}
#' 
#' @note Not all variables are available at all stations. Attempting to retrieve a non-existent variable 
#' will result in an error message being displayed.
#' 
#' @importFrom jsonlite fromJSON
#' @export
#'
#' @examples
#' stoon_monthly_precip <- ch_get_AHCCD_monthly("4057120", "SK", "PCP")
ch_get_AHCCD_monthly <- function(station, province, variable, url = "http://dd.weather.gc.ca/climate/ahccd/geojson/historical/monthly/") {
  
  provinces <- c("AB", "BC", "MB", "NB", "NL","NS", "NT", "NU", "ON", "PE", "QC", "SK", "YT")
  variables <- c("PCP", "RA", "SN", "TMAX", "TMIN", "TMEAN", "PSFC", "SFCWND", "SLP")
  
  # check that parameters are passed
  if (station == "" | is.null(station) ) {
    stop("station not specified")
  }
  
  if (province == "" | is.null(province)) {
    stop("province not specified")
  }
  
  if (variable == "" | is.null(variable) ) {
    stop("variable not specified")
  }
  
  if (url == "" | is.null(url) ) {
    stop("url not specified")
  }
  
  # check province and variable
  if (!province %in% provinces) {
    stop("province not recognised")
  }

  if (!variable %in% variables) {
    stop("variable not recognised")
  }
  
  # create station URL
  station_file <- paste(url, province, "/AHCCD_hist_monthly_", province,"_", 
                        station,"_", variable, "_P1M.json", sep = "")
  
  # make sure station name is valid, i.e. that the variable exists

    all <- fromJSON(station_file, flatten = FALSE)
    vals <- all$features$properties
    return(vals)
}