#functions to run the DCWBM

#' @title Distributed Climate Water Balance Model
#' 
#' @description Run the distributed climate water balance model on a data frame of points containing climate and landcover data. 
#'  The model is spatially uncoupled, so points can be of any spatial organization desired.
#'  Most development has occured using 400 m resolution gridded data as input.  Parameters can be adjusted to calibrate to observations. \cr
#'
#'  @references Moore, R.D., Trubilowicz, J.W. and Buttle, J.M. (2012), Prediction of Streamflow Regime and Annual Runoff for Ungauged Basins Using a Distributed Monthly Water Balance Model. Journal of the American Water Resources Association, 48: 32–42. DOI: 10.1111/j.1752–1688.2011.00595.x \cr
#' 
#' @param parameters 
#' A vector of seven model parameters for adjusting snowmelt rates from the base melt rate of 2mm/degree C and interception rates by landcover \cr
#'     1. Multiplier for snowmelt in areas with no treecover (e.g. alpine) \cr
#'     2. Multiplier for snowmelt in areas with partial treecover (e.g. subalpine) \cr
#'     3. Multiplier for snowmelt in areas with full treecover \cr
#'     4. Interception rate (snow and rain) in areas with no treecover \cr
#'     5. Interception rate (snow and rain) in areas with partial treecover \cr
#'     6. Interception rate (snow and rain) in areas with full treecover \cr
#'     7. Soil storage capacity multiplier \cr
#'     8. Temp at which precip is all rain \cr
#'     9. Temp at which precipitation is all snow \cr
#' @param data A data frame of inputs to the model containing 31 fields and any number of rows:
#'     ID: Mandatory to match inputs and outputs \cr
#'     Lat: Latitude \cr
#'     Lon: Longitude \cr
#'     Elev: Elevation \cr
#'     12 columns of monthly average temperature (degrees C) \cr
#'     12 columns of monthly total precipitation (mm) \cr
#'     12 columns of precip as snow (mm) Note that this can be left as NA if
#'     LC_class: Landcover classification, either (1) for no treecover, (2) for partial treecover, or (3) for full treecover \cr
#'     Water: Binary for surface water, either (0) no or (1) yes.  Overrides LC_class \cr
#'     Glacier: Binary for Glacier coverage, either (0) no or (1) yes. Overrides LC_class \cr
#' @param output Desired output form, either 'LUMPED' or 'DISTRIB' \cr
#' @param type Desired output source, either 'runoff' or 'snow' \cr
#' @param snowpart How to calculate precip as snow, either 'fromtemp' or 'fromdata' \cr
#' @return Monthly runoff in millimeters, either lumped for the whole area, or distributed by coordinates.
#' 
#' @export
DCWBM <- function(data, parameters=c(1.75,1.25,1,0,0.1,0.25,1,3,-5), output = 'LUMPED', type = 'runoff', snowpart = 'fromdata'){
  rundat <- as.matrix(data)
  runoff <- waterbalance(parameters, rundat, type, snowpart)
  
  if ( output == 'DISTRIB'){
    out <- data.frame(ID = data$ID, Lat = data$Lat, Long = data$Long)
    out <- cbind(out, runoff)
    names(out) <- c('ID', 'Lat', 'Long', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
  } else {
    out <- colMeans(runoff)
  }
  return(out)
}