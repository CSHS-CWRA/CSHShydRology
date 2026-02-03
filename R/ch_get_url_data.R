#' @title Gets remote data sets
#' 
#' @description 
#' Accesses data sets, via a url the first time,
#' saves them locally, then accesses them locally after the first time the script is executed.
#'
#' @param gd_url url for accessing data set
#' @param gd_filename name of file on local drive, including full path
#' @param stop_on_error Optional. If there is an error in accessing the file and 
#' \code{stop_on_error = TRUE} (the default) error/warning 
#' messages cause a \code{stop}. If \code{stop_on_error = FALSE}, then the error message
#' string is returned.
#' 
#' @importFrom httr GET write_disk
#' @importFrom terra rast
#' @author Dan Moore Kevin Shook
#' @return Returns a data frame (from a .csv file), a \pkg{terra} \code{SpatRaster} object (from a .tif file), 
#'or a \pkg{terra} \code{SpatVector} object (from a GeoJSON file).
#'
#' @examples \donttest{
#' # Example not tested automatically as multiple large data files are downloaded which is slow
#' 
#' # Tested using files in the Upper Penticton Creek
#' # zenodo repository https://zenodo.org/record/4781469
#' library(ggplot2)
#' library(terra)
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
#' sm_data <- ch_get_url_data(sm_url, sm_fn, stop_on_error = FALSE)
#' head(sm_data)
#' 
#' # test with tif/tiff file containing a dem
#' ra_fn <- file.path(dir_name, "gs_dem25.tif")
#' ra_url <- "https://zenodo.org/record/4781469/files/gs_dem25.tif"
#' ra_data <- ch_get_url_data(ra_url, ra_fn, stop_on_error = FALSE)
#' terra::plot(ra_data)
#' 
#' # test with GeoJSON
#' gs_fn <- file.path(dir_name, "gs_soilmaps.GeoJSON")
#' gs_url <- "https://zenodo.org/record/4781469/files/gs_soilmaps.GeoJSON"
#' gs_data <- ch_get_url_data(gs_url, gs_fn, stop_on_error = FALSE)
#' 
#' ggplot() +
#'   tidyterra::geom_spatvector(data = gs_data, aes(fill = new_key)) +
#'   labs(fill = "Soil class",
#'        x = "UTM Easting (m)",
#'        y = "UTM Northing (m)") +
#'   coord_sf(crs = 32611) +
#'   theme_bw()
#' }
#' @export
#' 
#' 
ch_get_url_data <- function(gd_url, gd_filename, stop_on_error = TRUE) {
  file_ext <- strsplit(x = gd_filename, split = "[.]")[[1]][2]
  
  # csv file - returns data frame
  if (file_ext == "csv") {
    if (!file.exists(gd_filename)) {
      # check to see if url file exists
      result <- ch_safe_GET(gd_url, gd_filename)
      
      if (result == "OK") {
        da <- read.csv(gd_filename)
        return(da)
      } else {
        if (stop_on_error)
          stop("Error in accessing ", gd_url)
        else
          return(result)
      }
      
    } else {
      da <- read.csv(gd_filename)
    }     
    return(da) 
  }
  
  # tiff file - returns raster object
  if (file_ext %in% c("tif", "tiff")) {
    if (!file.exists(gd_filename)) {
      result <- ch_safe_GET(gd_url, gd_filename)
    }
    if (result == "OK") {
      da <- raster::raster(gd_filename)
      return(da)
    } else {
      if (stop_on_error)
        stop("Error in accessing ", gd_url)
      else
        return(result)
    }

  }
  
  # GeoJSON - returns sf object
  if (file_ext == "GeoJSON") {
    if (!file.exists(gd_filename)) {
      # check to see if url file exists
       result <- ch_safe_GET(gd_url, gd_filename)
       if (result == "OK") {
         da <- st_read(gd_filename)
         return(da)
       } else {
         if (stop_on_error)
           stop("Error in accessing ", gd_url)
         else
           return(result)
       }
       
    } else {
      da <- st_read(gd_filename)
    }     
    return(da) 
  }
}

 