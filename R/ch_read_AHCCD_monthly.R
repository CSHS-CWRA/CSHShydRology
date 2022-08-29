#' Reads AHCCD monthly file
#'
#' @description This program reads an Adjusted and Homogenized Canadian Climate Data (AHCCD) data of 
#' precipitation or temperatures. The values are arranged as year x month, 
#' which makes them difficult to read using standard \R functions.
#' @param monthly_file Required. Name of the file to be read.
#' @return If successful, returns the values in a dataframe, consisting of the \code{year},
#' the \code{month}, the value and the data \code{code}. 
#' @author Kevin Shook
#' @seealso  \code{\link{ch_read_AHCCD_daily}} 
#' @references
#' Any use of the data must cite \cite{Mekis, E and L.A. Vincent, 2011: An overview of 
#' the second generation adjusted daily temperature and precipitation dataset for trend analysis in Canada. 
#' Atmosphere-Ocean, 49 (2), 163-177.}
#' @examples \dontrun{
#' # Don't run these examples as use of the dummy
#' # files will cause error messages
#' 
#' Stoon_monthly_precip <- ch_read_AHCCD_monthly("mt4057120.txt")
#' NB_monthly_tmean <- ch_read_AHCCD_monthly("mm4045695.txt") }
#' @importFrom stringr str_split_fixed str_detect str_to_lower fixed
#' @export

ch_read_AHCCD_monthly <- function(monthly_file = NULL) {
  # check parameter
  if (monthly_file == "" | is.null(monthly_file)) {
    stop("File not specified")
  }
  
  if (!file.exists(monthly_file)) {
    stop("File not found")
  }
  
  # set up home for dates
  date <- c(0)

  # set up constants
  twodigitnums <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

  # strip off file name from path and extension to figure out data type
  base <- basename(monthly_file) # remove path
  split <- str_split_fixed(base, fixed("."), 2)
  filename <- split[1]
  
  # check for type of values
  if (str_detect(tolower(filename), 'mx')) {
    val_type <- 'tmax'
  }
  else if (str_detect(tolower(filename), 'mm')) {
    val_type <- 'tmean'
  }
  else if (str_detect(tolower(filename), 'mn')) {
    vals_type <- 'tmin'
  } 
  else if (str_detect(str_to_lower(filename), "mt")) {
    val_type <- "precip"
  }
  else if (str_detect(str_to_lower(filename), "mr")) {
    val_type <- "rain"
  }
  else if (str_detect(str_to_lower(filename), "ms")) {
    val_type <- "snow"
  }
  else {
    stop("Unrecognised file type")
  }
  
  # figure out header info
  # read 1st 10 lines
  con <- file(monthly_file, "r", blocking = FALSE, encoding = "ISO_8859-2")
  input <- readLines(con, n = 10)
  close(con)
  
  # find number of lines containing file info
  # headerlines may be in English and/or French
  
  input <- tolower(input)
  LineNum <- str_detect(input, fixed(','))
  englishHeaderNum <- str_detect(input, fixed("updated", ignore_case = TRUE))
  englishHeaderCount <- sum(englishHeaderNum)
  frenchHeaderNum <- str_detect(input, fixed("jusqu", ignore_case = TRUE))
  frenchHeaderCount <- sum(frenchHeaderNum)
  fileHeaderLines <- englishHeaderCount + frenchHeaderCount
  
  # find number of lines containing column titles
  englishLineNum <- str_detect(input,fixed("year", ignore_case = TRUE))
  englishLineCount <- sum(englishLineNum)
  frenchLineNum <- str_detect(input,fixed("hiver", ignore_case = TRUE))
  frenchLineCount <- sum(frenchLineNum)
  columnHeaderLines <- sum(englishLineCount) + sum(frenchLineCount)
  
  totalSkipLines <- fileHeaderLines + columnHeaderLines
  
  # read data from file without parsing
  raw <- read.csv(file = monthly_file, header = FALSE, skip = totalSkipLines)
  row_count <- nrow(raw)
  
  # parse the lines into data and quality codes
  data_cols <- seq(2, 24, 2)
  code_cols <- data_cols + 1

  year_num <- as.numeric(raw[, 1])

  data_values <- raw[, data_cols]
  data_codes <- raw[, code_cols]
  data_values[data_values <= -999] <- NA_real_

  # stack the data frames to vectors
  stacked <- ch_stack_EC(data_values, data_codes)

  # replicate months
  all_months <- rep.int(twodigitnums, row_count)
  # replicate years
  all_years <- rep(year_num, each = 12)

  # find bad date values
  bad_date_loc <- is.na(date)
  good_date_loc <- !bad_date_loc

  # assemble data sets
  all_data <- cbind(all_years, all_months, stacked)

  # get good dates only
  good_data <- all_data[good_date_loc, ]
  names(good_data) <- c("year", "month", val_type, "code")
  return(good_data)
}
