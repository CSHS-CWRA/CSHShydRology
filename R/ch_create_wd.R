#' @title Create working directory
#' 
#' @description 
#' Creates a working directory.
#' 
#' @param wd name of a directory in which to store files created by WhiteboxTools functions
#' @return 
#' \item{\code{TRUE}}{returns \code{TRUE} upon successful execution}
#' 
#' @author Dan Moore 
#' @seealso \code{\link{ch_clear_wd}} to clear the working directory
#' @export
#' @examples \donttest{
#' # not tested automatically as will return a warning
#' ch_create_wd(tempdir())
#' }
ch_create_wd <- function(wd) {
  # creates working directory
  if (dir.exists(wd)) {
    # print warning if directory already exists
    warning(paste("A directory named", wd, "exists"))
  } else {
    # create directory
    dir.create(wd, recursive = TRUE)
    message(paste("A directory named", wd, "has been created"))
  }
  return(TRUE)
}
