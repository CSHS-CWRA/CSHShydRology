#' List of Water Survey of Canada hydrometic stations.
#' 
#' @description A dataframe of station information, as extracted from HYDAT using ECDataExplorer.
#' 
#' @format A dateframe with a row for each station and 20 columns.
#' @source Water Survey of Canada
#' 
#' Variables: 
#' \itemize{
#' \item	 {Station}  { - StationID}
#' \item	 {StationName} { - Station Name}
#' \item	 {HYDStatus} { - Active or Discontinued}
#' \item	 {Prov} { - Province}
#' \item	 {Latitude}{}
#' \item	 {Longitude}{}
#' \item	 {DrainageArea} { - km2}
#' \item	 {Years} { - # of years with data}
#' \item	 {From} { - Start Year}
#' \item	 {To} { - End Year}
#' \item	 {Reg.} { - Regulated }
#' \item	 {Flow} { - if TRUE/Yes}
#' \item	 {Level} { = if TRUE/Yes}
#' \item	 {Sed} { - if TRUE/Yes}
#' \item	 {OperSched} { - Continuous or Seasonal}
#' \item	 {RealTime} { - if TRUE/Yes}
#' \item	 {RHBN} { - if TRUE/Yes the station is in the reference hydrologic basin network}
#' \item	 {Region} { - ECCC Region}
#' \item	 {Datum} { - reference datum}
#' \item	 {Operator} { - Operator}
#' }
#' 
"HYDAT_list"
NULL