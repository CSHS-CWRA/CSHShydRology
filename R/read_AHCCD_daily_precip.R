#' Reads AHCCD daily precipitation file
#'
#' @description This program reads Adjusted and Homogenized Canadian Climate Data (AHCCD) 
#' file of rainfall, snowfall or total precipitation. The values are arranged as
#' month x day, which makes them difficult to read using standard \R functions.
#' @param precip_file Required. Name of the file to be read.
#' @return If successful, returns the values in a data frame, consisting of the \code{date}, 
#' the precipitation variable and the data \code{code}. The name of the precipitation variable, \code{rain}, 
#' \code{snow} or \code{total} is inferred from the file name.
#' If unsuccessful, returns the value \code{FALSE}.
#' @author Kevin Shook
#' @seealso  \code{\link{read_AHCCD_daily_temp}} \code{\link{read_AHCCD_monthly_precip}} 
#' @references
#'Monthly AHCCD data are available from \url{http://www.ec.gc.ca/dccha-ahccd}. 
#'Daily values must be requested. Any use of the precipitation data must cite 
#'\cite{Mekis, E and L.A. Vincent, 2011: An overview of the second generation 
#'adjusted daily precipitation dataset for trend analysis in Canada. 
#'Atmosphere-Ocean, 49 (2), 163-177.}
#' @examples
#' \dontrun{
#'stoon_rainfall <- read_AHCCD_daily_precip("dr_4057120.txt")}
#' @export

read_AHCCD_daily_precip <- function(precip_file){
  # check parameter
  if (precip_file == "" | is.null(precip_file)){
    cat('Error: precip_file missing\n')
    return(FALSE)
  }
  
  # strip off file name from path and extension to figure out data type
  base <- basename(precip_file) # remove path
  split <- stringr::str_split_fixed(base, stringr::fixed('.'), 2)
  filename <- split[1]
  # check for precip type
  if (stringr::str_detect(tolower(filename), 'dt')){
    precip_type <- 'total'
  }
  else if (stringr::str_detect(tolower(filename), 'dr')){
    precip_type <- 'rain'
  }
  else if (stringr::str_detect(tolower(filename), 'ds')){
    precip_type <- 'snow'
  }
  else{
    cat('Error: wrong file type\n')
    return(FALSE)
  }
  
  # set up homes for data
  value <- c(0)
  code <- c(0)
  date <- c(0)
  
  # set up constants
  twodigitnums <- c('01','02','03','04','05','06','07','08','09','10','11','12',
                    '13','14','15','16','17','18','19','20','21','22','23','24',
                    '25','26','27','28','29','30','31')
  
  
  # figure out header info
  # read 1st 10 lines
  con <- file(precip_file, "r", blocking = FALSE, encoding="ISO_8859-2")
  input <- readLines(con, n=10)
  close(con)
  
  # find header lines
  # first find number of lines containing file info
  input <- tolower(input)
  LineNum <- stringr::str_detect(input, stringr::fixed(','))
  englishHeaderNum <- stringr::str_detect(input, stringr::fixed("updated", ignore_case=TRUE))
  englishHeaderCount <- sum(englishHeaderNum)
  frenchHeaderNum <- stringr::str_detect(input, stringr::fixed("jusqu", ignore_case=TRUE))
  frenchHeaderCount <- sum(frenchHeaderNum)
  fileHeaderLines <- englishHeaderCount + frenchHeaderCount
  
  # now find number of lines containing column titles
  englishLineNum <- stringr::str_detect(input,stringr::fixed("year", ignore_case=TRUE))
  englishLineCount <- sum(englishLineNum)
  frenchLineNum <- stringr::str_detect(input,stringr::fixed("annee", ignore_case=TRUE))
  frenchLineCount <- sum(frenchLineNum)
  columnHeaderLines <- sum(englishLineCount) + sum(frenchLineCount)
  
  totalSkipLines <- fileHeaderLines + columnHeaderLines
  
  # check for with of first field - is there a leading space
  firstChar <- substr(input[totalSkipLines + 1], 1, 1)
  
  if (firstChar == ' ')
    yearWidth <- 5
  else
    yearWidth <- 4
  
  # set up widths to read
  header <- c(yearWidth,3,1)
  header_classes <- c('numeric','numeric','character')
  
  cols <- rep.int(c(8,1),31)
  cols_classes <- rep.int(c('numeric', 'character'), 31)
  all <- c(header,cols)
  all_classes <- c(header_classes, cols_classes)
  
  # read data
  raw <- utils::read.fwf(file=precip_file, widths=all, header=FALSE, 
                         colClasses=all_classes, skip=totalSkipLines)
  row_count <- nrow(raw)
  # now unstack data
  data_row <- 0
  data_cols  <- seq(4,64,2)
  code_cols <- data_cols + 1
  
  year_num <- as.numeric(raw[,1])
  month_num <- as.numeric(raw[,2])
  
  data_values <- raw[,data_cols]
  data_codes <- raw[,code_cols]
  data_values[data_values<=-999] <- NA_real_
  
  month_str <- twodigitnums[month_num]
  
  
  # now unstack data
  data_row <- 0
  
  # transpose data
  data_values_t <- t(data_values)
  data_codes_t <- t(data_codes)
  
  # now stack data frames to vectors
  data_values_vector <- as.vector(data_values_t, mode='numeric')
  data_codes_vector <- as.character(as.vector(data_codes_t, mode='character'))
  
  # replicate months
  all_days <- rep.int(twodigitnums, row_count)
  
  # replicate days
  all_months <- rep(month_str, each=31)
  
  # replicate years
  all_years <- rep(year_num, each=31)
  
  # create dates
  datestrings <- paste(all_years,'-', all_months,'-', all_days, sep='')
  date <- as.Date(datestrings, format = '%Y-%m-%d')
  
  # find bad date values
  bad_date_loc <- is.na(date)
  good_date_loc <- !bad_date_loc
  
  # assemble data sets
  all_data <- data.frame(date, data_values_vector, data_codes_vector)
  
  # get good dates only
  good_data <- all_data[good_date_loc,]
  names(good_data) <- c('date', precip_type, 'code')
  
  return(good_data)
}
