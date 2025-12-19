#' Extracting Single Station ECCC meteorological data using an API
#'
#' This function performs the API request to pull meteorological data for single stations from
#' Environment and Climate Change Canada (ECCC) using the OGC API for a single station 
#'
#' @param station_ClimateID The Climate id type used to define the station
#' @param res Resolution of data options -> "hourly", "daily", "monthly"
#'   \itemize{
#'     \item `"hourly"`: Data extracted at hourly resolution
#'     \item `"daily"`: Data extracted at daily resolution
#'     \item `"monthly"`: Data extracted at monthly resolution
#'   }
#' @param dt_start Starting datetime of data to extract
#'      Options -> string with format YYYY-MM-DDThh:mm:ss OR '..' to signify start of record
#' @param dt_end Ending datetime of data to; 
#'      Options -> string with format YYYY-MM-DDThh:mm:ss OR '..' to signify current date time
#' @param sortby The variable you want to sort the results by. 
#'        It must match a column name that is returned in the data set
#' @param API_limit The number of records a single API call will pull. Must be less than 10001
#' @param API_offset The number of records to offset in a single pull. To pull all records, set this to 0
#' @param verbose Logical value indicating if progress messages are displayed
#'
#' @return
#' A data frame containing all records and feature values from the specified station and dates
#'
#' @examples
#' get_eccc_single_station(station_ClimateID = "1026271", dt_start = "2024-01-01T00:00:00", dt_end= "..")
#'
#' @export

get_eccc_single_station <- function(station_ClimateID, 
                         res = "daily", 
                         dt_start = "1840-01-01T00:00:00",
                         dt_end = "..",
                         sortby = 'LOCAL_DATE',
                         API_limit = 10000,
                         API_offset = 0,
                         verbose = TRUE){
  
  # This performs the API request
  request_eccc_data <- function(station_ClimateID,res,offset,limit,id_type,dt_start,dt_end){
    url <- paste0("https://api.weather.gc.ca/collections/", 
                  "climate-", res, "/items?f=csv&",
                  "limit=", format(limit, scientific = FALSE),
                  "&offset=", format(offset, scientific = FALSE),
                  "&lang=en-CA&skipGeometry=false",
                  "&sortby=",sortby,
                  "&datetime=",dt_start, "/", dt_end,
                  "&CLIMATE_IDENTIFIER=", station_ClimateID)
    ec_data <- try(fread(url))
    if (inherits(ec_data, "try-error")) {
      stop(paste0("API call failed; ",url))
    }
    return(list(ec_data, nrow(ec_data)))
  }
    
  options(timeout = 240) # seconds until operation times out (4mins)
  
  # 1. Check dt_start and dt_end are character
  if (!inherits(dt_start, "character") || !inherits(dt_end, "character")) {
    stop(
      structure(
        list(message = "ERROR: dt_start and/or dt_end not character variables. Must be a string", call = NULL),
        class = c("not_character", "error", "condition")
      )
    )
  }
  
  # 2. Check verbose is logical
  if (!inherits(verbose, "logical")) {
    stop(
      structure(
        list(message = "ERROR: Argument verbose is not a logical value. Must be TRUE or FALSE", call = NULL),
        class = c("not_logical", "error", "condition")
      )
    )
  }
  
  # 3. Check res is valid
  if (length(res) != 1 || !(res %in% c("hourly", "daily", "monthly"))) {
    stop(
      structure(
        list(message = "Argument 'res' must be one of: 'hourly', 'daily', or 'monthly'", call = NULL),
        class = c("invalid_argument", "error", "condition")
      )
    )
  }
  
  # 4. Check dt_start and dt_end are not both ".."
  dt_format <- "%Y-%m-%dT%H:%M:%S"
  if ((length(dt_start) != 1 || length(dt_end) != 1) || (dt_start == ".." && dt_end == "..")) {
    stop(
      structure(
        list(message = "Both arguments dt_start and dt_end are '..'. Please define one or both.", call = NULL),
        class = c("invalid_formatting", "error", "condition")
      )
    )
  }
  
  # 5. Parse dates
  dt_start_a <- strptime(dt_start, format = dt_format)
  dt_end_a <- strptime(dt_end, format = dt_format)
  
  # 6. Check both dates are not NA
  if ((length(dt_start_a) != 1 || length(dt_end_a) != 1) || (is.na(dt_start_a) && is.na(dt_end_a))) {
    stop(
      structure(
        list(message = "Both arguments dt_start and dt_end have invalid formatting. Ensure they follow '%Y-%m-%dT%H:%M:%S'.", call = NULL),
        class = c("invalid_formatting", "error", "condition")
      )
    )
  }
  
  dotdot <- 0
  
  # 7. Check dt_start_a NA but dt_end_a valid
  if (is.na(dt_start_a) && !is.na(dt_end_a)) {
    if (dt_start != "..") {
      stop(
        structure(
          list(message = "ERROR: Argument dt_start has invalid formatting. Ensure it follows the format %Y-%m-%dT%H:%M:%S", call = NULL),
          class = c("invalid_formatting", "error", "condition")
        )
      )
    } else {
      dotdot <- 1
    }
  }
  
  # 8. Check dt_end_a NA but dt_start_a valid
  if (is.na(dt_end_a) && !is.na(dt_start_a)) {
    if (dt_end != "..") {
      stop(
        structure(
          list(message = "ERROR: Argument dt_end has invalid formatting. Ensure it follows the format %Y-%m-%dT%H:%M:%S", call = NULL),
          class = c("invalid_formatting", "error", "condition")
        )
      )
    } else {
      dotdot <- 2
    }
  }
  
  # 9. Check dt_start_a < dt_end_a
  if (dotdot == 0 && !is.na(dt_start_a) && !is.na(dt_end_a) && dt_start_a > dt_end_a) {
    stop(
      structure(
        list(message = "Argument dt_start must be less than dt_end.", call = NULL),
        class = c("invalid_range", "error", "condition")
      )
    )
  }
  
  # 10. Check API_limit
  if (!inherits(API_limit, "numeric") || length(API_limit) != 1 || API_limit > 10000 || API_limit < 1) {
    stop(
      structure(
        list(message = "Argument API_limit must be between 1 and 10000.", call = NULL),
        class = c("invalid_range", "error", "condition")
      )
    )
  }

  if (verbose == TRUE){
    print("All variables pass validity checks")
  }
  
  returned_data <- request_eccc_data(station_ClimateID,res,API_offset,API_limit,id_type,dt_start,dt_end)
  ec_data <- bind_rows(returned_data[1])
  number_records_returned <- returned_data[2]

  if (verbose == TRUE){
    print(paste0("Request 1 Complete:",number_records_returned," records were returned"))
  }
  if (number_records_returned == 0){
    print("No records where returned. If you expected records, please check the station ID value is the Climate ID number and that data spans the time range of interest.")
  }
  count =1 
  
  # If the API response limit is reached, we add an offset and pull the next records, appending the response to the dataframe
  while (number_records_returned == API_limit){
    API_offset = API_offset+API_limit
    returned_data <- request_eccc_data(station_ClimateID,res,API_offset,API_limit,id_type,dt_start,dt_end)
    ec_data_additional <- returned_data[1]
    number_records_returned <- returned_data[2]
    ec_data <- bind_rows(ec_data, ec_data_additional)
    count = count + 1
    print(paste0("Request ",count," Complete:",number_records_returned," records were returned"))
  }
  
  return(ec_data)
}
