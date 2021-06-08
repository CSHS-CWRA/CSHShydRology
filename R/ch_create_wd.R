#' @title Create SAGA Working Directory
#' 
#' This function creates a SAGA working directory.
#' 
#' @param wd working directory file path
#' @return 
#' \item{TRUE}{returns TRUE upon successful execution}
#' 
#' @author Dan Moore <dan.moore@ubc.ca>
#' @seealso \code{\link{ch_clear_wd}} to clear the working SAGA directory
#' @export
#' @examples
#' \dontrun{
#' ch_create_wd()
#' 
#' }
#' 
ch_create_wd <- function(wd) {
  # creates working directory for saga files
  if (dir.exists(wd)) {
    # print warning if directory already exists
    print(paste("Warning: a directory named", wd, "exists"))
  } else {
    # create directory
    dir.create(wd)
  }
  return(TRUE)
}
