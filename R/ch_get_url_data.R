#' @title Gets remote data sets
#' 
#' @description 
#' Accesses data sets, via a url the first time,
#' saves them locally, then accesses them locally after the first time the script is executed.
#'
#' @param gd_url url for accessing data set
#' @param gd_filename name of file on local drive, including full path
#' @param quiet Optional. If \code{FALSE} (the default) error/warning messages are printed if the data cannot be found.
#' @author Dan Moore
#'
#' @importFrom httr GET write_disk
#' @importFrom sf st_read
#' @importFrom raster raster
#'
#' @return Returns a data frame (from a .csv file), a \code{raster} object (from a .tif file), 
#'or an \code{sf} object (from a GeoJSON file).
#'
#' @examples \donttest{
#' # Example not tested automatically as multiple large data files are downloaded which is slow
#' 
#' # Tested using files in the Upper Penticton Creek
#' # zenodo repository https://zenodo.org/record/4781469
#' library(ggplot2)
#' library(raster)
#' 
#' # create directory to store data sets
#' dir_name <- tempdir(check = FALSE)
#' if (!dir.exists(dir_name)) {
#'   dir.create(dir_name)
#' }
#'  
#' # test with soil moisture data in csv format
#' sm_fn <- file.path(dir_name, "sm_data.csv")
#' sm_url <- "https://zenodo.org/record/4781469/files/sm_data.csv"
#' sm_data <- ch_get_url_data(sm_url, sm_fn)
#' head(sm_data)
#' 
#' # test with tif/tiff file containing a dem
#' ra_fn <- file.path(dir_name, "gs_dem25.tif")
#' ra_url <- "https://zenodo.org/record/4781469/files/gs_dem25.tif"
#' ra_data <- ch_get_url_data(ra_url, ra_fn)
#' plot(ra_data)
#' 
#' # test with GeoJSON
#' gs_fn <- file.path(dir_name, "gs_soilmaps.GeoJSON")
#' gs_url <- "https://zenodo.org/record/4781469/files/gs_soilmaps.GeoJSON"
#' gs_data <- ch_get_url_data(gs_url, gs_fn)
#' 
#' ggplot(gs_data) +
#'   geom_sf(aes(fill = new_key)) +
#'   labs(fill = "Soil class",
#'        x = "UTM Easting (m)",
#'        y = "UTM Northing (m)") +
#'   coord_sf(datum = 32611) +
#'   theme_bw()
#' }
#' @export
#' 
ch_get_url_data <- function(gd_url, gd_filename, quiet = FALSE) {
  file_ext <- strsplit(x = gd_filename, split = "[.]")[[1]][2]
  
  # csv file - returns data frame
  if (file_ext == "csv") {
    if (!file.exists(gd_filename)) {
      # check to see if url file exists
      result <- ch_test_url_file(gd_url, quiet)
      if (result == "error" | result == "warning") {
        stop("URL does not exist")
      } 
      
      da <- read.csv(gd_url)
      write.csv(da, gd_filename)

    } else {
      da <- read.csv(gd_filename)
    }     
    return(da) 
  }
  
  # tiff file - returns raster object
  if (file_ext %in% c("tif", "tiff")) {
    if (!file.exists(gd_filename)) {
      # check to see if url file exists
      result <- ch_test_url_file(gd_url, quiet)
      if (result == "error" | result == "warning") {
        stop("URL does not exist")
      } 

      GET(gd_url, write_disk(gd_filename))
    }
    da <- raster::raster(gd_filename)
    return(da)
  }
  
  # GeoJSON - returns sf object
  if (file_ext == "GeoJSON") {
    if (!file.exists(gd_filename)) {
      # check to see if url file exists
      result <- ch_test_url_file(gd_url, quiet)
      if (result == "error" | result == "warning") {
        stop("URL does not exist")
      } 
      
      GET(gd_url, write_disk(gd_filename))
      da <- st_read(gd_filename)
    } else {
      da <- st_read(gd_filename)
    }     
    return(da) 
  }
}

