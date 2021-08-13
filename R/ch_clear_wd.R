#' Clear SAGA Working Directory
#'
#' This function empties and removes a SAGA working directory. Note: the data for raster layers read in as SAGA sdat 
#' files (e.g. the contributing area, carea) are held on disk rather than in memory; if you clear the working directory you
#' will get an error if you try to use the raster layer.
#'
#' @param wd working directory file path
#' 
#' @return
#'  \item{result}{returns TRUE upon successful execution}
#' 
#' @author Dan Moore <dan.moore@ubc.ca>
#' @seealso \code{\link{ch_create_wd}} to create working SAGA directory
#' @export
#' @examples \dontrun{
#' 
#' # not run as clearing all files in a given directory cannot be tested in CRAN
#' 
#' # create a saga working directory
#' saga_wd <- tempdir()
#' ch_create_wd(saga_wd) # confirm creation
#' 
#' # clear the saga working directory
#' ch_clear_wd(saga_wd)
#' }
#' 
ch_clear_wd <- function(wd) {
  # empty and remove saga working directory
  # note: the data for raster layers read in as sdat files
  # (e.g., carea) are held on disk rather than in memory; if you
  # clear the working directory you will get an error if you try
  # to use the raster layer
  filelist = list.files(wd)
  file.remove(paste0(wd, "/", filelist))
  unlink(wd, recursive = TRUE)
  return(TRUE)
}
