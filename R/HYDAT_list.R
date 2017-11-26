#' Station information for HYDAT as of 2016-10-16 release of ECDataExplorer
#' 
#' @description The file contains 20 fields of metadata for each station from HYDAT
#' @format a dateframe with 7791 rows and 20 columns of metadata
#' \itemize{
#' \item	 {Station}  {StationID}
#' \item	 {StationName} {Station Name}
#' \item	 {HYDStatus} {Active or Discontinued}
#' \item	 {Prov} {Province}
#' \item	 {Latitude}{}
#' \item	 {Longitude}{}
#' \item	 {DrainageArea} {km2}
#' \item	 {Years} {# of years with data}
#' \item	 {From} {Start Year}
#' \item	 {To} {End Year}
#' \item	 {Reg.} {Regulated?} 
#' \item	 {Flow} {if TRUE/Yes}
#' \item	 {Level} {if TRUE/Yes}
#' \item	 {Sed} {if TRUE/Yes}
#' \item	 {OperSched} {Continuous or Seasonal}
#' \item	 {RealTime} {if TRUE/Yes}
#' \item	 {RHBN} {if TRUE/Yes is in the reference hydrologic basin network}
#' \item	 {Region} {if TRUE/Yes is in the reference hydrologic basin network}
#' \item	 {Datum} {if TRUE/Yes is in the reference hydrologic basin network}
#' \item	 {Operator} {if TRUE/Yes is in the reference hydrologic basin network}
#' }
#' @source 2016-10-16 release of ECDataExplorer
"HYDAT_list"
NULL