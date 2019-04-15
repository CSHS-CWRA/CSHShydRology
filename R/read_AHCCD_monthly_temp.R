#' Reads AHCCD monthly temp file
#'
#' @description This program reads Adjusted and Homogenized Canadian Climate Data (AHCCD) data of 
#' min, max or mean air temperatures. The values are arranged as year x month, 
#' which makes them difficult to read using standard \R functions.
#' @param temp_file Required. Name of the file to be read.
#' @return If successful, returns the values in a dataframe, consisting of the \code{year},
#' the \code{month}, the value and the data \code{code}. If unsuccessful, returns the value 
#' \code{FALSE}.
#' @author Kevin Shook
#' @seealso  \code{\link{read_AHCCD_monthly_precip}} \code{\link{read_AHCCD_daily_temp}}
#' @references
#' Monthly AHCCD data are available from \url{http://www.ec.gc.ca/dccha-ahccd}. Any use of 
#' the tempitation data must cite \cite{Mekis, E and L.A. Vincent, 2011: An overview of 
#' the second generation adjusted daily tempitation dataset for trend analysis in Canada. 
#' Atmosphere-Ocean, 49 (2), 163-177.}
#' @examples
#' \dontrun{
#' stoon_monthly_total <- read_AHCCD_monthly_temp("mt4057120.txt")}
#' @export

read_AHCCD_monthly_temp <- function(temp_file) {
  # check parameter
  if (temp_file == "" | is.null(temp_file)){
    cat('Error: temp_file missing\n')
    return(FALSE)
  }
  
  # set up home for data
  date <- c(0)

  # set up constants
  twodigitnums <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

  if (temp_file == "") {
    cat("Error: temp_file missing\n")
    return(FALSE)
  }

  # strip off file name from path and extension to figure out data type
  base <- basename(temp_file) # remove path
  split <- stringr::str_split_fixed(base, stringr::fixed("."), 2)
  filename <- split[1]
  # check for temp type
  if (stringr::str_detect(tolower(filename), 'mx')){
    temp_type <- 'tmax'
  }
  else if (stringr::str_detect(tolower(filename), 'mm')){
    temp_type <- 'tmean'
  }
  else if (stringr::str_detect(tolower(filename), 'mn')){
    temp_type <- 'tmin'
  }
  else {
    cat("Error: wrong file type\n")
    return(FALSE)
  }
  
  # figure out header info
  # read 1st 10 lines
  con <- file(temp_file, "r", blocking = FALSE, encoding="ISO_8859-2")
  input <- readLines(con, n=10)
  close(con)
  
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
  
  # read data
  raw <- utils::read.csv(file = temp_file, header = FALSE, skip = totalSkipLines)
  row_count <- nrow(raw)
  # now unstack data
  data_row <- 0
  data_cols <- seq(2, 24, 2)
  code_cols <- data_cols + 1

  year_num <- as.numeric(raw[, 1])

  data_values <- raw[, data_cols]
  data_codes <- raw[, code_cols]
  data_values[data_values <= -999] <- NA_real_

  # now unstack data
  data_row <- 0

  # transpose data
  data_values_t <- t(data_values)
  data_codes_t <- t(data_codes)

  # now stack data frames to vectors
  data_values_vector <- as.vector(data_values_t, mode = "numeric")
  data_codes_vector <- as.character(as.vector(data_codes_t, mode = "character"))

  # replicate months
  all_months <- rep.int(twodigitnums, row_count)
  # replicate years
  all_years <- rep(year_num, each = 12)

  # find bad date values
  bad_date_loc <- is.na(date)
  good_date_loc <- !bad_date_loc

  # assemble data sets
  all_data <- data.frame(all_years, all_months, data_values_vector, data_codes_vector)

  # get good dates only
  good_data <- all_data[good_date_loc, ]
  names(good_data) <- c("year", "month", temp_type, "code")
  return(good_data)
}
