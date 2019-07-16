#####################################################################
#' Streamflow data
#'
#' Daily river discharge for the station 01AD002 on
#' St-John River at Fort Kent, New Brunswick.
#' Data range from 1926 to 2014 and have a drainage area of 14700 sq km.
#' 
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#' @source \url{https://wateroffice.ec.gc.ca/}
#####################################################################
"flowStJohn"

#####################################################################
#' L-moments and catchment descriptors of hydrometric stations in Canada
#'
#' The L-moments of the annual maximums at 562 stations
#' were extracted from Water Survery of Canada (HYDAT).
#' Catchment descriptors are available for each station was
#' provided by Environment and Climate Change Canada (ECCC).
#' The best at-site distributions according to the AIC criterion are included.
#'
#'  \describe{
#'    \item{site}{Identification number of the station in HYDAT.}
#'    \item{area}{Drainage area (sq km).}
#'    \item{map}{Mean annual precipitation for the catchment (mm).}
#'    \item{wb}{Drainage area covered by waterbodies (pct).}
#'    \item{stream}{Stream density (km of streams per catchment sq km).}
#'    \item{lon,lat}{Longitude and latitude of the catchment center.}
#'    \item{l1, lcv, lsk}{Sample mean, L-coefficient of variation and
#'          L-coefficient of skewness of the annual maximum discharges.}
#'    \item{dstr}{Distribution selected for the annual maximum discharge based
#'          on the Akaike Information Criterion (AIC).}
#'  }
#'
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#' @source \url{https://wateroffice.ec.gc.ca/}
#####################################################################
"flowUngauged"

#####################################################################
#' Annual maximums from sites in the Atlantic region of Canada
#'
#' Contains the annual maximums of 45 hydrometric stations found in the
#' region '01' of Water Survery of Canada.
#' Additionaly to the annual maximums, the output list includes catchment
#' descriptors (longitude, latitude, drainage area, mean annual precipitation)
#' and the geographical distance between each station.
#'
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#' @source \url{https://wateroffice.ec.gc.ca/}
#####################################################################
"flowAtlantic"



