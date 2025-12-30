# 
# CSHS terra function testing...
# 


library(devtools)
library(githubinstall)
library(terra)
library(tidyterra)
library(CSHShydRology)
library(whitebox)

# install 'tidyterra' branch from Billy (based on a rebased branch of Joels pull request) 

# notes:
#   - 'terra' branch is just updated functions to use terra for Raster data 
#      (SpatRaster only).
#   - 'tidyterra' branch is updated functions for using both Raster and Vector
#      data. The use of the 'sf' package has been removed as a result. 
#      tidyterra package is used with ggplot2 when using SpatVector.

# INSERT REF ID OF BRANCH HERE
# githubinstall::gh_install_packages('CSHShydRology', ref = "")


# installing local based CSHShydRology package of the tidyterra branch


remove.packages('CSHShydRology')

install.packages(
  "C:/Users/bbrowning/OneDrive - NHC/Documents/GitHub/CSHShydRology",
  repos = NULL,  type = "source")


#______________________________________________________________________________
# ch_volcano_raster() ----
#______________________________________________________________________________
#  create test raster and plot
test_raster <- ch_volcano_raster()
plot(test_raster)
message(paste('Spat Raster output?... =', class(test_raster) == 'SpatRaster'))


#______________________________________________________________________________
# ch_volcano_pourpoints() ---- 
#______________________________________________________________________________
# create file of pour points from volcano DEM
pourpoint_file <- tempfile("volcano_pourpoints", fileext = c(".shp"))
pourpoints <- ch_volcano_pourpoints(pourpoint_file)
plot(pourpoints)
message(paste('Spat Vector output?... =', class(pourpoints) == 'SpatVector'))


#______________________________________________________________________________
#  ch_wbt_catchment() ----
#______________________________________________________________________________
test_raster <- ch_volcano_raster()
dem_raster_file <- tempfile(fileext = ".tif")
no_sink_raster_file <- tempfile("no_sinks", fileext = ".tif")

# write test raster to file
terra::writeRaster(test_raster, dem_raster_file)

# remove sinks
removed_sinks <- ch_wbt_removesinks(dem_raster_file, no_sink_raster_file, 
                                    method = "fill")

message(paste('Spat Raster output?... =', class(removed_sinks) == 'SpatRaster'))


# get flow accumulations
flow_acc_file <- tempfile("flow_acc", fileext = ".tif")
flow_acc <- ch_wbt_flow_accumulation(no_sink_raster_file, flow_acc_file)

# get pour points
pourpoint_file <- tempfile("volcano_pourpoints", fileext = ".shp")
pourpoints <- ch_volcano_pourpoints(pourpoint_file)
snapped_pourpoint_file <- tempfile("snapped_pourpoints", fileext = ".shp")
snapped_pourpoints <- ch_wbt_pourpoints(pourpoints, flow_acc_file, pourpoint_file,
                                        snapped_pourpoint_file, snap_dist = 10)

# get flow directions
flow_dir_file <- tempfile("flow_dir", fileext = ".tif")
flow_dir <- ch_wbt_flow_direction(no_sink_raster_file, flow_dir_file)
fn_catchment_ras <- tempfile("catchment", fileext = ".tif")
fn_catchment_vec <- tempfile("catchment", fileext = ".shp")
catchments <- ch_wbt_catchment(snapped_pourpoint_file, flow_dir_file, 
                               fn_catchment_ras, fn_catchment_vec)
plot(test_raster)
plot(catchments, add = TRUE)

message(paste('Spat Raster output?... =', class(test_raster) == 'SpatRaster'))
message(paste('Spat Vector output?... =', class(catchments) == 'SpatVector'))


#______________________________________________________________________________
# ch_catchment_hyps() ----
#______________________________________________________________________________

# Note: example not tested automatically as it is very slow to execute due to the downloading
# change the following line to specify a directory to hold the data

dir_name <- tempdir(check = FALSE)
# create directory to store data sets
if (!dir.exists(dir_name)) {
  dir.create(dir_name, recursive = TRUE)
}
# get 25-m dem
dem_fn <- file.path(dir_name, "gs_dem25.tif")
dem_url <- "https://zenodo.org/record/4781469/files/gs_dem25.tif"
dem_upc <- ch_get_url_data(dem_url, dem_fn)
dem_upc

message(paste('Spat Raster output?... =', class(dem_upc) == 'SpatRaster'))

plot(dem_upc)

# get catchment boundaries
cb_fn <- file.path(dir_name, "gs_catchments.GeoJSON")
cb_url <- "https://zenodo.org/record/4781469/files/gs_catchments.GeoJSON"
cb <- ch_get_url_data(cb_url, cb_fn)

message(paste('Spat Vector output?... =', class(cb) == 'SpatVector'))

# quick check plot - all catchments
terra::plot(dem_upc)
plot(cb, add = TRUE, col = NA)

# subset 240 catchment
cb_240 <- cb |> tidyterra::filter(wsc_name == "240")
plot(cb_240, col = NA)

## test function 

# test different combinations of arguments
ch_catchment_hyps(cb_240, dem_upc, quantiles = seq(0, 1, 0.1))
ch_catchment_hyps(cb_240, dem_upc, z_levels = seq(1600, 2050, 50))
ch_catchment_hyps(cb_240, dem_upc, n_levels = 6)
ch_catchment_hyps(cb_240, dem_upc)
ch_catchment_hyps(cb_240, dem_upc, zmin = 1600, zmax = 2050)
ch_catchment_hyps(cb_240, dem_upc, zmin = 1600, zmax = 2050, n_levels = 6)

# generate a graph
ch_catchment_hyps(cb_240, dem_upc, hypso_plot = TRUE)
ch_catchment_hyps(cb_240, dem_upc, hypso_plot = TRUE, 
                  col = "blue", type = "l", ylim = c(1500, 2200))
ch_catchment_hyps(cb_240, dem_upc, hypso_plot = TRUE, 
                  add_grid = TRUE, quantiles = seq(0, 1, 0.1))
ch_catchment_hyps(cb_240, dem_upc, hypso_plot = TRUE,
                  ylab = expression("z ("*10^{-3} ~ "km)"))

# extract specific quantiles (e.g., median and 90%)
ch_catchment_hyps(cb_240, dem_upc, quantiles = c(0.5,0.9))


#______________________________________________________________________________
# ch_checkchannels() ----
#______________________________________________________________________________
# Only proceed if Whitebox executable is installed
test_raster <- ch_volcano_raster()
dem_raster_file <- tempfile(fileext = c(".tif"))
no_sink_raster_file <- tempfile("no_sinks", fileext = c(".tif"))

# write test raster to file
terra::writeRaster(test_raster, dem_raster_file)

# remove sinks
removed_sinks <- ch_wbt_removesinks(dem_raster_file, no_sink_raster_file, method = "fill")

# get flow accumulations
flow_acc_file <- tempfile("flow_acc", fileext = c(".tif"))
flow_acc <- ch_wbt_flow_accumulation(no_sink_raster_file, flow_acc_file)

# get flow directions
flow_dir_file <- tempfile("flow_dir", fileext = c(".tif"))
flow_dir <- ch_wbt_flow_direction(no_sink_raster_file, flow_dir_file)
channel_raster_file <- tempfile("channels", fileext = c(".tif"))
channel_vector_file <- tempfile("channels", fileext = c(".shp"))
channels <- ch_wbt_channels(flow_acc_file, flow_dir_file, channel_raster_file,
                            channel_vector_file, 1)

message(paste('Spat Vector output?... =', class(channels) == 'SpatVector'))


# get pour points
pourpoint_file <- tempfile("volcano_pourpoints", fileext = ".shp")
pourpoints <- ch_volcano_pourpoints(pourpoint_file)
snapped_pourpoint_file <- tempfile("snapped_pourpoints", fileext = ".shp")
snapped_pourpoints <- ch_wbt_pourpoints(pourpoints, flow_acc_file, pourpoint_file,
                                        snapped_pourpoint_file, snap_dist = 10)
ch_checkchannels(test_raster, channels, snapped_pourpoints)


#______________________________________________________________________________
# ch_checkcatchment() ----
#______________________________________________________________________________

test_raster <- ch_volcano_raster()
dem_raster_file <- tempfile(fileext = ".tif")
no_sink_raster_file <- tempfile("no_sinks", fileext = ".tif")

# write test raster to file
terra::writeRaster(test_raster, dem_raster_file)

# remove sinks
removed_sinks <- ch_wbt_removesinks(dem_raster_file, no_sink_raster_file, 
                                    method = "fill")

# get flow accumulations
flow_acc_file <- tempfile("flow_acc", fileext = ".tif")
flow_acc <- ch_wbt_flow_accumulation(no_sink_raster_file, flow_acc_file)

# get pour points
pourpoint_file <- tempfile("volcano_pourpoints", fileext = ".shp")
pourpoints <- ch_volcano_pourpoints(pourpoint_file)
snapped_pourpoint_file <- tempfile("snapped_pourpoints", fileext = ".shp")
snapped_pourpoints <- ch_wbt_pourpoints(pourpoints, flow_acc_file, pourpoint_file,
                                        snapped_pourpoint_file, snap_dist = 10)

# get flow directions
flow_dir_file <- tempfile("flow_dir", fileext = ".tif")
flow_dir <- ch_wbt_flow_direction(no_sink_raster_file, flow_dir_file)
fn_catchment_ras <- tempfile("catchment", fileext = ".tif")
fn_catchment_vec <- tempfile("catchment", fileext = ".shp")
catchments <- ch_wbt_catchment(snapped_pourpoint_file, flow_dir_file, 
                               fn_catchment_ras, fn_catchment_vec)

# check results
ch_checkcatchment(test_raster, catchments, snapped_pourpoints)


#______________________________________________________________________________
# ch_contours() ----
#______________________________________________________________________________

# use volcano DEM
dem <- ch_volcano_raster()
# generate contours
contours <- ch_contours(dem)

# plot contours map
plot(contours)

message(paste('Spat Vector output?... =', class(contours) == 'SpatVector'))

#______________________________________________________________________________
# ch_get_url_data() ----
#______________________________________________________________________________

# Tested using files in the Upper Penticton Creek
# zenodo repository https://zenodo.org/record/4781469
library(ggplot2)

# create directory to store data sets
dir_name <- tempdir(check = FALSE)
if (!dir.exists(dir_name)) {
  dir.create(dir_name)
}

# test with soil moisture data in csv format
sm_fn <- file.path(dir_name, "sm_data.csv")
sm_url <- "https://zenodo.org/record/4781469/files/sm_data.csv"
sm_data <- ch_get_url_data(sm_url, sm_fn)
head(sm_data)

# test with tif/tiff file containing a dem
ra_fn <- file.path(dir_name, "gs_dem25.tif")
ra_url <- "https://zenodo.org/record/4781469/files/gs_dem25.tif"
ra_data <- ch_get_url_data(ra_url, ra_fn)
terra::plot(ra_data)

message(paste('Spat Raster output?... =', class(ra_data) == 'SpatRaster'))

# test with GeoJSON
gs_fn <- file.path(dir_name, "gs_soilmaps.GeoJSON")
gs_url <- "https://zenodo.org/record/4781469/files/gs_soilmaps.GeoJSON"
gs_data <- ch_get_url_data(gs_url, gs_fn)

message(paste('Spat Vector output?... =', class(gs_data) == 'SpatVector'))

ggplot() +
  tidyterra::geom_spatvector(data = gs_data, aes(fill = new_key)) +
  labs(fill = "Soil class",
       x = "UTM Easting (m)",
       y = "UTM Northing (m)") +
  coord_sf(datum = terra::crs(gs_data)) +
  theme_bw()


#______________________________________________________________________________
# ch_wbt_channels() ----
#______________________________________________________________________________

# Only proceed if Whitebox executable is installed
test_raster <- ch_volcano_raster()
dem_raster_file <- tempfile(fileext = c(".tif"))
no_sink_raster_file <- tempfile("no_sinks", fileext = c(".tif"))

# write test raster to file
terra::writeRaster(test_raster, dem_raster_file)

# remove sinks
removed_sinks <- ch_wbt_removesinks(dem_raster_file, no_sink_raster_file, method = "fill")

# get flow accumulations
flow_acc_file <- tempfile("flow_acc", fileext = c(".tif"))
flow_acc <- ch_wbt_flow_accumulation(no_sink_raster_file, flow_acc_file)

# get flow directions
flow_dir_file <- tempfile("flow_dir", fileext = c(".tif"))
flow_dir <- ch_wbt_flow_direction(no_sink_raster_file, flow_dir_file)
channel_raster_file <- tempfile("channels", fileext = c(".tif"))
channel_vector_file <- tempfile("channels", fileext = c(".shp"))
channels <- ch_wbt_channels(flow_acc_file, flow_dir_file, channel_raster_file,
                            channel_vector_file, 1)
plot(channels)


#______________________________________________________________________________
# ch_wbt_flow_accumulation() ----
#______________________________________________________________________________

test_raster <- ch_volcano_raster()
dem_raster_file <- tempfile(fileext = c(".tif"))
no_sink_raster_file <- tempfile("no_sinks", fileext = c(".tif"))

# write test raster to file
terra::writeRaster(test_raster, dem_raster_file)

# remove sinks
removed_sinks <- ch_wbt_removesinks(dem_raster_file, no_sink_raster_file, method = "fill")

# get flow accumulations
flow_acc_file <- tempfile("flow_acc", fileext = c(".tif"))
flow_acc <- ch_wbt_flow_accumulation(no_sink_raster_file, flow_acc_file)
plot(flow_acc)


#______________________________________________________________________________
# ch_wbt_flow_direction() ----
#______________________________________________________________________________

test_raster <- ch_volcano_raster()
dem_raster_file <- tempfile(fileext = c(".tif"))
no_sink_raster_file <- tempfile("no_sinks", fileext = c(".tif"))

# write test raster to file
terra::writeRaster(test_raster, dem_raster_file)

# remove sinks
removed_sinks <- ch_wbt_removesinks(dem_raster_file, no_sink_raster_file, method = "fill")

# get flow directions
flow_dir_file <- tempfile("flow_dir", fileext = c(".tif"))
flow_dir <- ch_wbt_flow_direction(no_sink_raster_file, flow_dir_file)
plot(flow_dir)


#______________________________________________________________________________
# ch_wbt_catchment_onestep() ----
#______________________________________________________________________________

test_raster <- ch_volcano_raster()
dem_raster_file <- tempfile(fileext = c(".tif"))
# write test raster to file
terra::writeRaster(test_raster, dem_raster_file)
wd <- tempdir()
pourpoint_file <- tempfile("volcano_pourpoints", fileext = ".shp")
pourpoints <- ch_volcano_pourpoints(pourpoint_file)
catchment <- ch_wbt_catchment_onestep(wd = wd, in_dem = dem_raster_file, 
                                      pp_sv = pourpoints, sink_method = "fill", 
                                      threshold = 1, snap_dist = 10)

message(paste('Spat Vector output?... =', class(catchment) == 'SpatVector'))














      