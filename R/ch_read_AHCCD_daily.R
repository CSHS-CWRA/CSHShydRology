#' Reads AHCCD daily file
#'
#' @description This program reads an Adjusted and Homogenized Canadian Climate Data (AHCCD) of daily 
#' precipitation or temperatures. The values are arranged as
#' month x day, which makes them difficult to read using standard \R functions.
#' @param daily_file Required. Name of the file to be read.
#' @return If successful, returns the values in a data frame, consisting of the date, 
#' the value and the data code.
#' @author Kevin Shook
#' @seealso  \code{\link{ch_read_AHCCD_monthly}} \code{\link{ch_get_AHCCD_monthly}}
#' @references Daily AHCCD data are available from \url{http://crd-data-donnees-rdc.ec.gc.ca/CDAS/products/EC_data/AHCCD_daily/}. 
#' Any use of the data must cite 
#'\cite{Mekis, E and L.A. Vincent, 2011: An overview of the second generation 
#'adjusted daily precipitation dataset for trend analysis in Canada. 
#'Atmosphere-Ocean, 49 (2), 163-177.}
#' 
#' @examples
#' \donttest{
#' # Not tested automatically as requires a file
#'stoon_daily_tmax <- ch_read_AHCCD_daily("dx40657120.txt")}
#' @importFrom stringr str_split_fixed str_detect fixed
#' @importFrom utils read.fwf
#' @export

ch_read_AHCCD_daily <- function(daily_file){
  # check parameter
  if (daily_file == "" | is.null(daily_file)) {
    stop("File not specified")
  }
  
  if (!file.exists(daily_file)) {
    stop("File not found")
  }
  
  # strip off file name from path and extension to figure out data type
  base <- basename(daily_file) # remove path
  split <- str_split_fixed(base, fixed('.'), 2)
  filename <- split[1]
  # check for variable type
  if (str_detect(tolower(filename), 'dx')) {
    val_type <- 'tmax'
  }
  else if (str_detect(tolower(filename), 'dm')) {
    val_type <- 'tmean'
  }
  else if (str_detect(tolower(filename), 'dn')) {
    val_type <- 'tmin'
  } 
  else if (str_detect(tolower(filename), 'dt')) {
    val_type <- 'precip'
  }
  else if (str_detect(tolower(filename), 'dr')) {
    val_type <- 'rain'
  }
  else if (str_detect(tolower(filename), 'ds')) {
    val_type <- 'snow'
  }
  else {
    stop("Unrecognised file type")
  }
    
  # set up homes for data
  value <- c(0)
  code <- c(0)
  date <- c(0)
  
  # set up constants
  monthdays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  monthdays_leapyear <- c(31,29,31,30,31,30,31,31,30,31,30,31)
  twodigitnums <- c('01','02','03','04','05','06','07','08','09','10','11','12',
                    '13','14','15','16','17','18','19','20','21','22','23','24',
                    '25','26','27','28','29','30','31')
  
  
  # figure out header info
  # read 1st 10 lines
  con <- file(daily_file, "r", blocking = FALSE, encoding = "ISO_8859-2")
  input <- readLines(con, n = 10)
  close(con)
  
  # first find number of lines containing file info
  input <- tolower(input)
  LineNum <- str_detect(input, fixed(','))
  englishHeaderNum <- str_detect(input, fixed("updated", ignore_case = TRUE))
  englishHeaderCount <- sum(englishHeaderNum)
  frenchHeaderNum <- str_detect(input, fixed("jusqu", ignore_case = TRUE))
  frenchHeaderCount <- sum(frenchHeaderNum)
  fileHeaderLines <- englishHeaderCount + frenchHeaderCount
  
  # now find number of lines containing column titles
  englishLineNum <- str_detect(input,fixed("year", ignore_case = TRUE))
  englishLineCount <- sum(englishLineNum)
  frenchLineNum <- str_detect(input,fixed("annee", ignore_case = TRUE))
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
  

  # set columns widths depending on data type
  if (val_type == "tmax" | val_type == "tmin" | val_type == "tmean")
    cols <- rep.int(c(7,1),31)
  else  
    cols <- rep.int(c(8,1),31)
  
  cols_classes <- rep.int(c('numeric', 'character'), 31)
  all <- c(header,cols)
  all_classes <- c(header_classes, cols_classes)
  
  
  # read data from file without parsing
  raw <- read.fwf(file = daily_file, widths = all, header = FALSE, 
                         colClasses = all_classes, skip = totalSkipLines)
  row_count <- nrow(raw)
  
  # parse the lines into data and quality codes
  data_cols  <- seq(4,64,2)
  code_cols <- data_cols + 1
  
  year_num <- as.numeric(raw[,1])
  month_num <- as.numeric(raw[,2])
  
  data_values <- raw[,data_cols]
  data_codes <- raw[,code_cols]
  data_values[data_values <= -999] <- NA_real_
  
  month_str <- twodigitnums[month_num]
  
  # stack the data frames to vectors
  stacked <- ch_stack_EC(data_values, data_codes)
  
  # replicate months
  all_days <- rep.int(twodigitnums, row_count)
  
  # replicate days
  all_months <- rep(month_str, each = 31)
  
  # replicate years
  all_years <- rep(year_num, each = 31)
  
  # create dates
  datestrings <- paste(all_years,'-', all_months,'-', all_days, sep = '')
  date <- as.Date(datestrings, format = '%Y-%m-%d')
  
  # find bad date values
  bad_date_loc <- is.na(date)
  good_date_loc <- !bad_date_loc
  
  # assemble data sets
  all_data <- cbind(date, stacked)
  
  # get good dates only
  good_data <- all_data[good_date_loc,]
  names(good_data) <- c('date', val_type, 'code')
  return(good_data)
}
