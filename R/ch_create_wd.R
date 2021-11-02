#' @title Create SAGA Working Directory
#' 
#' @description 
#' This function creates a SAGA working directory.
#' 
#' @param wd working directory file path
#' @return 
#' \item{\code{TRUE}}{returns \code{TRUE} upon successful execution}
#' 
#' @author Dan Moore 
#' @seealso \code{\link{ch_clear_wd}} to clear the working SAGA directory
#' @export
#' @examples
#' ch_create_wd(tempdir())
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
