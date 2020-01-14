#' Clear SAGA Working Directory
#'
#' This function empties and removes a SAGA working directory
#' @param wd working directory file path
#' ch_clear_wd()

ch_clear_wd <- function(wd) {
  # empty and remove saga working directory
  # note: the data for raster layers read in as sdat files
  # (e.g., carea) are held on disk rather than in memory; if you
  # clear the working directory you will get an error if you try
  # to use the raster layer
  filelist = list.files(wd)
  file.remove(paste0(wd, "/", filelist))
  unlink(wd, recursive = TRUE)
} # end function ch_clear_wd
