#' @name ch_tidyhydat_ECDE_meta
#' @title Creates an ECDE-like dataframe of metadata
#'
#' @description Extracts tombstone (meta) data for stations from \pkg{tidyhydat} in a 
#' format similar to that used by the Environment Canada Data Explorer (ECDE). The 
#' default does not capture all the fields in ECDE, which includes the most recent status 
#' of many fields such as operating schedule. Returning these values slows the function,
#' particularly when all WSC stations are selected.
#' 
#'
#' @param stations  A vector of WSC station IDs, i.e. c("05BB001", "05BB003", "05BB004", "05BB005"). If 
#' \code{stations = "all"} then values are returned for all stations.
#' @param all_ECDE Should all ECDE values be returned? If \code{FALSE} the default, then
#' values of \code{Flow}, \code{Sed}, \code{OperSched}, \code{Region}, \code{Datum}, and
#' \code{Operator} are omitted or will differ from the ECDE values. If \code{all_ECDE = TRUE},
#' then the function will return values identical to ECDE.
#' 
#' @author Paul Whitfield <paul.h.whitfield@gmail.com>
#' 
#' @export
#'
#' @return Returns a list with three items:
#' \itemize{
#' \item {\code{meta} - a dataframe of metadata from \pkg{tidyhydat} in ECDE form (not all ECDE fields are reproduced in this summary)}
#' \item {\code{H_version} - version information, and }
#' \item {\code{th_meta} - a dataframe with all \pkg{tidyhdat} fields including:}
#' \itemize{
#'   \item {Station - StationID}
#'   \item {StationName - Station Name}
#'   \item {HYDStatus - Active or Discontinued}
#'   \item {Prov - Province}
#'   \item {Latitude}
#'   \item {Longitude}
#'   \item {DrainageArea - km\eqn{^2}{^2}}
#'   \item {Years - number of years with data}
#'   \item {From - Start Year}
#'   \item {To - End Year}
#'   \item {Reg. - Regulated?}
#'   \item {Flow - not captured (differs from ECDE), unless \code{all_ECDE = TRUE}}
#'   \item {Level - not captured (differs from ECDE), unless \code{all_ECDE = TRUE}}
#'   \item {Sed - not captured (differs from ECDE), unless \code{all_ECDE = TRUE}}
#'   \item {OperSched - not captured (differs from ECDE), unless \code{all_ECDE = TRUE}}
#'   \item {RealTime - if TRUE/Yes}
#'   \item {RHBN - if TRUE/Yes is in the reference hydrologic basin network}
#'   \item {Region - number of region instead of name (differs from ECDE), unless \code{all_ECDE = TRUE}}
#'   \item {Datum - reference number (differs from ECDE), unless \code{all_ECDE = TRUE}}
#'   \item {Operator - reference number (differs from ECDE), unless \code{all_ECDE = TRUE}}
#' }
#' }
#' 
#' @importFrom tidyhydat hy_version hy_stations hy_stn_regulation hy_stn_data_range hy_daily allstations
#' @importFrom stringr str_detect
#' @importFrom dplyr left_join
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @seealso \code{\link{ch_get_ECDE_metadata}} \code{\link{ch_tidyhydat_ECDE}}
#' @examples {
#' stations <- c("05BB001", "05BB003", "05BB004", "05BB005")
#' result <- ch_tidyhydat_ECDE_meta(stations)
#' metadata <- result[[1]]
#' version <- result[[2]]}
#' 

ch_tidyhydat_ECDE_meta <- function(stations, all_ECDE = FALSE){

  H_version <- hy_version()  
  H_version <- data.frame(H_version)
  print(H_version)
  
  if (length(stations) == 1) {
    if (stations == "all") {
      data(allstations, package = "tidyhydat", verbose = FALSE)
      allstations <- allstations
      tc <- allstations
      stations <- allstations$STATION_NUMBER
    } 
  }
  # extract difference parts of metadata using tidyhydat
  tc <- hy_stations(station_number = stations)
  tc <- data.frame(tc)

  td <- hy_stn_regulation(station_number = stations)
  td <- data.frame(td)
  
  te <- hy_stn_data_range(station_number = stations)
  te <- data.frame(te)
  te <- te[te[,2] == "Q",]
  
  colnmc <- c("Station", "StationName","Prov", "Region", "HydStatus", 
              "SedStatus", "Latitude", "Longitude", "DrainageAreaG", 
              "DrainageAreaE","RHBN", "RealTime", "Contributor", 
              "Operator", "Datum")
  colnmd <- c("Station", "From", "To", "Reg.")
  colnme <- c("Station", "DATA_TYPE", "SED_DATA_TYPE", "From", "To", "Years")
  colmeta <- c("Station", "StationName","HydStatus","Prov", "Latitude", 
               "Longitude", "DrainageArea", "Years", "From", "To", "Reg.",
               "Flow", "Level", "Sed", "Opersched", "RealTime", "RHBN", "Region",
               "Datum", "Operator")
  

  names(tc) <- colnmc
  names(td) <- colnmd
  names(te) <- colnme


  if (all_ECDE) {
    t1 <- merge(tc, td, by.x = "Station", by.y = "Station", all.x = TRUE)
    t2 <- merge(t1, te, by.x = "Station", by.y = "Station", all.x = TRUE)
    t3 <- rep.int(NA, length(t2[,1]))
    th_meta <- t2
    
    meta <- data.frame(t2[,c(1:2,5,3,7:9,23,21:22,18)],t3,t3,t3,t3,t2[,c(12,12,4,15,14)])
    names(meta) <- colmeta
    
    if (nrow(meta) > 1)
    # convert code numbers to strings
    # get dataframes of codes and strings
    regions <- hy_reg_office_list()
    datums <- hy_datum_list()
    operators <- hy_agency_list()
    
    # lookup values
    region_names <- left_join(meta, regions, by = c("Region" = "REGIONAL_OFFICE_ID"))
    datum_names <- left_join(meta, datums, by = c("Datum" = "DATUM_ID"))
    QC_locations <- meta$Prov == "QC"

    operator_names <- left_join(meta, operators, by = c("Operator" = "AGENCY_ID"))

    
    meta$Region <- region_names$REGIONAL_OFFICE_NAME_EN
    meta$Datum <- datum_names$DATUM_EN
    French_datums <- !is.na(datum_names$DATUM_FR)
    meta$Datum[QC_locations & French_datums] <- 
      datum_names$DATUM_FR[QC_locations & French_datums]
    meta$Operator <- operator_names$AGENCY_EN
    French_operators <- !is.na(operators$AGENCY_FR)
    meta$Operator[QC_locations & French_operators] <- 
      operator_names$AGENCY_FR[QC_locations & French_operators]
    
    # set missing values to ""
    meta$Datum[is.na(meta$Datum)] <- ""
    meta$Operator[is.na(meta$Operator)] <- ""
    
    # loop through all stations to get all ECDE variables
    pb <- txtProgressBar(min = 0, max = nrow(meta), style = 2)
    
    # temporarily disable warnings
    oldwarn <- getOption("warn")
    options(warn = -1)
    
    for (i in 1:nrow(meta)) {
      if (nrow(meta) > 1)
        setTxtProgressBar(pb, value = i)
      start_date <-  paste(meta$To[i], "-01-01", sep = "")
      end_date <- paste(meta$To[i], "-12-31", sep = "")
      end_year <- as.numeric(meta$To[i])
      # flow and stage

 
      daily <- try(hy_daily(meta$Station[i], 
                        start_date = start_date,
                        end_date = end_date), silent = TRUE)


      if (length(class(daily)) > 1) {
        if (str_detect(string = daily[1], "Error")) {
          meta$Flow[i] <- FALSE
          meta$Level[i] <- FALSE
        } else {
          if (any(daily$Parameter == "Flow"))
            meta$Flow[i] <- TRUE
          else
            meta$Flow[i] <- FALSE
        
          if (any(daily$Parameter == "Level"))
            meta$Level[i] <- TRUE
          else
            meta$Level[i] <- FALSE
          } 
        } else {
          meta$Flow[i] <- FALSE
          meta$Level[i] <- FALSE  
      }  

        
      # sediment
      sed <- FALSE
      sed <- try(hy_sed_daily_loads(meta$Station[i], 
                                start_date = start_date,
                                end_date = end_date), silent = TRUE)
      
      if (length(class(sed)) > 1) {
        if (str_detect(string = sed[1], "Error"))
          meta$Sed[i] <- FALSE
        else
          if (any(sed$Parameter == "Load"))
            meta$Sed[i] <- TRUE
          else
            meta$Sed[i] <- FALSE
        
      } else {
        meta$Sed[i] <-  FALSE
      }
      # operator schedule
      oper <- ""
      oper <- try(hy_stn_data_coll(meta$Station[i]))
      #browser()
      if (nrow(oper) == 0) {
        meta$OperSched[i] <- ""
      }
      else {
        # get last value
        meta$OperSched[i] <- oper$OPERATION[nrow(oper)]  
      }
      
     }  # for
    # enable warnings
    options(warn = oldwarn)
   }
    else {  
      t1 <- merge(tc, td, by.x = "Station", by.y = "Station")
      t2 <- merge(t1, te, by.x = "Station", by.y = "Station")
      t3 <- rep.int(NA, length(t2[,1]))
      th_meta <- t2
      
      meta <- data.frame(t2[,c(1:2,5,3,7:9,23,21:22,18)],t3,t3,t3,t3,t2[,c(12,12,4,15,14)])
      names(meta) <- colmeta
      
      result <- list(meta = meta, H_version = H_version, th_meta = th_meta )
      
     print("Result is a list that contains [1] metadata from tidyhydat in ECDE form, [2] H_version information, and [3] th_meta  ")
     print("not all ECDE fields are reproduced in this summary")
    }
  
  return(result)
}