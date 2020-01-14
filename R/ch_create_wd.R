#' Create SAGA Working Directory
#' 
#' This function creates a SAGA working directory
#' @param wd working directory file path

ch_create_wd <- function(wd) {
  # creates working directory for saga files
  if (dir.exists(wd)) {
    # print warning if directory already exists
    print(paste("Warning: a directory named", wd, "exists"))
  } else {
    # create directory
    dir.create(wd)
  }
} # end function ch_create_wd
