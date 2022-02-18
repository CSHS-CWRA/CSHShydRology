#' Clear Working Directory
#'
#' @description 
#' Empties and removes a working directory. 
#' 
#' @details
#' The data for raster layers read in as Whitebox
#' files are held on disk rather than in memory
#'
#' @param wd working directory file path
#' @param do_check If \code{TRUE}, the default, the user is asked to confirm the
#' deletion of the working directory. If \code{TRUE}, the directory is deleted
#' without confirmation.
#' 
#' @return
#'  \item{result}{returns TRUE upon successful execution}
#' 
#' @author Dan Moore 
#' @seealso \code{\link{ch_create_wd}} to create working directory
#' @export
#' @examples \donttest{
#' # not tested as clearing all files in a given directory cannot be tested in CRAN
#' 
#' # create an empty working directory
#' my_wd <- tempdir()
#' ch_create_wd(my_wd) # confirm creation
#' 
#' # clear the working directory
#' ch_clear_wd(my_wd)
#' }
#' 
ch_clear_wd <- function(wd, do_check = TRUE) {
  if (do_check) {
    prompt <- paste(
      "Are you certain you want to remove",
      wd,
      " (y/n): "
    )
    response <- readline(prompt)
    if (response == "n") return(paste(wd, "not removed"))
  }
  filelist <- list.files(wd)
  file.remove(paste0(wd, "/", filelist))
  unlink(wd, recursive = TRUE)
  return(paste(wd, "removed"))
}
