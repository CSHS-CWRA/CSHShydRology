#' Extracting Single Station ECCC meteorological data using an API
#'
#' This function performs the API request to pull meteorological data for single stations from
#' Environment and Climate Change Canada (ECCC) using the OGC API for a single station.
#' 
#' These function pull data by the stations ClimateID number. The climate ID number can be found
#' by searchin the station on the historical data page on the ECCC website (https://climate.weather.gc.ca/historical_data/search_historic_data_e.html) 
#' and searching for your station. The resulting page will display the Climate ID of the station. 
#' 

#' 
#' 
#' @param station_ClimateID The Climate id type used to define the station.
#' @param res Resolution of data options -> "hourly", "daily", "monthly".
#' @param dt_start Starting datetime of data to extract.
#'      Options -> string with format YYYY-MM-DDThh:mm:ss OR NULL to signify start of record.
#' @param dt_end Ending datetime of data to extract. 
#'      Options -> string with format YYYY-MM-DDThh:mm:ss OR NULL to signify current date time.
#' @param sortby The variable you want to sort the results by. 
#'        It must match a column name that is returned in the data set.
#' @return
#' A data frame containing all records and feature values from the specified station and dates.
#'
#' @examples
#' not run here

request_eccc_data <- function(station_ClimateID, res,offset,limit,dt_start,dt_end, on_failure = NULL){
  #create URL
  url <- paste0("https://api.weather.gc.ca/collections/", 
                "climate-", res, "/items?f=csv&",
                "limit=", format(limit, scientific = FALSE),
                "&offset=", format(offset, scientific = FALSE),
                "&lang=en-CA&skipGeometry=false",
                "&sortby=LOCAL_DATE",
                "&datetime=",dt_start, "/", dt_end,
                "&CLIMATE_IDENTIFIER=", station_ClimateID)
  ec_data <- try(read.csv(url))
  
  # if error is received on URL call
  if (inherits(ec_data, "try-error")) {
    if (is.null(on_failure)){
      return (list(NULL,0))
    } else {
    stop(paste0("API call failed; ",url))
    }
  }
  
  return(list(ec_data, nrow(ec_data)))
}

#'Format date for API Call
#'
#'Formats the date to make an appropriate API call. If daily resolution,
#'the date entered will be set to the zero-th hour for the day. If monthly resolution, the
#'date entered will be set transformed to the first day of the month. If hourly resolution, the 
#'datetime entered will be set to the top of the hour. 
#'
#' @param dt The datetime object to be transformed. Can be in POSIXt format or a string. 
#' @param res The resolution of the data to be extracted from the API
#' 
#'  @return 
#'  A string formatted to use in the API call 

format_date <- function(dt, res) {
  # API syntax for no start and/or end date
  if (is.null(dt)){
    return (NULL)
    
  # if data is read is POSIXt
  }else if (inherits(dt, "POSIXt")){
    if (res == 'daily'){
      dt = format(dt, format ="%Y-%m-%dT00:00:00")
    } else if (res == 'monthly'){
      dt= format(dt, format = "%Y-%m-01T00:00:00")
    } else if (res == 'hourly'){
      dt = format(dt, format = "%Y-%m-%dT%H:00:00")
    }
    
  # if date is string  
  } else if (inherits(dt, "character")){
    
    # check string is in correct structure
    parsed <- as.POSIXct(as.POSIXlt(dt, tryFormats = c("%Y-%m-%dT%H:%M:%S", "%Y-%m-%dT%H:%M", "%Y-%m-%dT%H", "%Y-%m-%d"), tz = "UTC"))
    # check the format of a sting input
    if (is.na(parsed)){
      stop(
        structure(
          list(message = "The inputs dt_start and/or dt_end are not in a related format to '%Y-%m-%dT%H:%M:%S'", call = NULL),
          class = c("invalid_formatting", "error", "condition")
        )
      )
    }
    
    # update based in data resolution
    if (res == 'daily'){
      dt = format(parsed, format ="%Y-%m-%dT00:00:00")
    } else if (res == 'monthly'){
      dt= format(parsed, format = "%Y-%m-01T00:00:00")
    } else if (res == 'hourly'){
      dt = format(parsed, format = "%Y-%m-%dT%H:00:00")
    }
    
  }
  
  return (dt)
}

#' Extracting Single Station ECCC meteorological data using an API
#'
#' This function performs the API request to pull meteorological data for single stations from
#' Environment and Climate Change Canada (ECCC) using the OGC API for a single station.
#' 
#' #' API Swagger Page: https://api.weather.gc.ca/openapi?f=html#/climate-daily/getClimate-dailyFeatures
#' 

#' @param station_ClimateID The Climate ID  used to define the station.The climate ID number can be found
#' by searchin the station on the historical data page on the ECCC website (https://climate.weather.gc.ca/historical_data/search_historic_data_e.html) 
#' and searching for your station. The resulting page will display the Climate ID of the station. 
#' @param res Resolution of data options -> "hourly", "daily", "monthly". 
#'   \itemize{
#'     \item `"hourly"`: Data extracted at hourly resolution
#'     \item `"daily"`: Data extracted at daily resolution
#'     \item `"monthly"`: Data extracted at monthly resolution
#'   }
#' @param dt_start Starting date-time for the data to extract. Must be either a character string in ISO 8601 format (e.g., "YYYY-MM-DDThh:mm:ss", with partial formats such as "YYYY-MM" or "YYYY-MM-DD" also accepted) or an object of class POSIXt. 
#' At least one of dt_start or dt_end must be non-NULL.
#' @param dt_end Ending date-time for the data to extract. Must be either a character string in ISO 8601 format (e.g., "YYYY-MM-DDThh:mm:ss", with partial formats such as "YYYY-MM" or "YYYY-MM-DD" also accepted) or an object of class POSIXt. 
#' At least one of dt_start or dt_end must be non-NULL.
#' @param verbose Logical value indicating if progress messages are displayed. Note that the API call may still print progress statements even if 
#' verbose is set to FALSE.
#'
#' @return
#' A data frame containing all records and feature values from the specified station and dates.
#'
#' @examples
#' Not run here
#'
#' @export
get_eccc_single_station <- function(station_ClimateID, 
                         res = c("daily","hourly","monthly"),
                         dt_start = "1840-01-01T00:00:00",
                         dt_end = NULL,
                         sortby = 'LOCAL_DATE',
                         API_limit = 10000,
                         verbose = FALSE,
                         on_failure = NULL){
  # seconds until operation times out (4mins). Ensures it doesn't get stuck on bad API call
  options(timeout = 240) 
  
  # set API call limit (as of Aug 2026, limit is 10,000)
  API_offset = 0
  API_limit = 8000
  
  res <- match.arg(res)
  
  # both the start and end date cannot be null (API does not allow for this)
  if (is.null(dt_start) && is.null(dt_end)){
    stop(
      structure(
        list(message = "Both arguments dt_start and dt_end are NULL. Please define one or both.", call = NULL),
        class = c("invalid_formatting", "error", "condition")
      )
    )
  } 
  
  
  # Check verbose is logical
  if (!inherits(verbose, "logical")) {
    stop(
      structure(
        list(message = "ERROR: Argument verbose is not a logical value. Must be TRUE or FALSE", call = NULL),
        class = c("not_logical", "error", "condition")
      )
    )
  }
  
  # format date for API call
  dt_start <- format_date(dt_start,res)
  dt_end <- format_date(dt_end,res)

  # make sure start date comes before end date  
  if (!is.null(dt_start) && !is.null(dt_end)){
    if (dt_start > dt_end){
      stop(
        structure(
          list(message = "Argument dt_start must be less than dt_end.", call = NULL),
          class = c("invalid_range", "error", "condition")
        )
      )
    }
  }
  
  # Format null start/end date to '..' (what the API formats Null as)
  if (is.null(dt_start)){
    dt_start = ".."
  }
  if (is.null(dt_end)){
    dt_end = ".."
  }

  # print statement if verbose is true
  if (verbose){
    print("All variables pass validity checks")
  }
  
  # make first request to ECCC using API
  returned_data <- request_eccc_data(station_ClimateID,res,API_offset,API_limit,dt_start,dt_end)
  ec_data <- dplyr::bind_rows(returned_data[1])
  number_records_returned <- returned_data[2]

  # print statement if verbose is true
  if (verbose){
    print(paste0("Request 1 Complete:",number_records_returned," records were returned"))
  }
  if (number_records_returned == 0){
    print("No records where returned. If you expected records, please check the station ID value is the Climate ID number and that data spans the time range of interest.")
  }
  count =1 
  
  # If the API response limit is reached, we add an offset and pull the next records, appending the response to the dataframe
  while (number_records_returned == API_limit){
    API_offset = API_offset+API_limit
    returned_data <- request_eccc_data(station_ClimateID,res,API_offset,API_limit,dt_start,dt_end)
    ec_data_additional <- returned_data[1]
    number_records_returned <- returned_data[2]
    ec_data <- dplyr::bind_rows(ec_data, ec_data_additional)
    count = count + 1
    print(paste0("Request ",count," Complete:",number_records_returned," records were returned"))
  }
  return(ec_data)
}
