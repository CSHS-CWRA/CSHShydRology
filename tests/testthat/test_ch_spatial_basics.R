# Tests for the spatial functions that do NOT need WhiteboxTools:
# ch_volcano_raster, ch_volcano_pourpoints, ch_contours, ch_catchment_hyps,
# ch_create_wd, ch_clear_wd and ch_wbt_filenames.

test_that("ch_volcano_raster returns a projected SpatRaster", {
  dem <- ch_volcano_raster()

  expect_s4_class(dem, "SpatRaster")
  # built from the base volcano matrix, 87 x 61 cells at 10 m
  expect_equal(dim(dem)[1:2], c(nrow(datasets::volcano), ncol(datasets::volcano)))
  expect_equal(unname(terra::res(dem)), c(10, 10))
  expect_false(is.na(terra::crs(dem)))
  expect_true(terra::crs(dem) != "")
})

test_that("ch_volcano_raster elevations match the volcano matrix", {
  dem <- ch_volcano_raster()
  vals <- terra::values(dem)

  expect_equal(range(vals, na.rm = TRUE), range(datasets::volcano))
  expect_equal(sum(!is.na(vals)), length(datasets::volcano))
})

test_that("ch_volcano_raster is deterministic", {
  expect_equal(terra::values(ch_volcano_raster()),
               terra::values(ch_volcano_raster()))
})


test_that("ch_volcano_pourpoints returns two points and writes them out", {
  shp <- tempfile("volcano_pourpoints", fileext = ".shp")

  pp <- ch_volcano_pourpoints(shp)

  expect_s4_class(pp, "SpatVector")
  expect_equal(nrow(pp), 2)
  expect_equal(terra::geomtype(pp), "points")
  expect_true(file.exists(shp))
  expect_true("test_label" %in% names(pp))
})

test_that("ch_volcano_pourpoints writes a file that reads back the same", {
  shp <- tempfile("volcano_pourpoints", fileext = ".shp")
  pp <- ch_volcano_pourpoints(shp)

  from_disk <- terra::vect(shp)
  expect_equal(nrow(from_disk), nrow(pp))
  expect_equal(terra::crds(from_disk), terra::crds(pp))
})

test_that("ch_volcano_pourpoints can overwrite an existing file", {
  shp <- tempfile("volcano_pourpoints", fileext = ".shp")
  ch_volcano_pourpoints(shp)

  expect_no_error(ch_volcano_pourpoints(shp))
  expect_equal(nrow(terra::vect(shp)), 2)
})

test_that("ch_volcano_pourpoints requires a file name", {
  expect_error(ch_volcano_pourpoints(), "must be specified")
})

test_that("ch_volcano_pourpoints shares the DEM's coordinate system", {
  # The pour points have to line up with the DEM for the whitebox pipeline to
  # work, so this guards against a silent projection mismatch.
  #
  # The comparison is against the DEM *as written to a GeoTIFF*, not the
  # in-memory object. ch_volcano_raster() sets its crs from a proj4 string,
  # which terra cannot map back to an EPSG code, so
  # terra::crs(dem, describe = TRUE)$code is NA in memory; GDAL resolves it to
  # 32760 on write. Since every ch_wbt_* function works through files, the
  # written form is the one that matters -- and it is what ch_wbt_pourpoints
  # compares when check_crs = TRUE.
  shp <- tempfile("volcano_pourpoints", fileext = ".shp")
  pp <- ch_volcano_pourpoints(shp)

  dem_file <- tempfile(fileext = ".tif")
  terra::writeRaster(ch_volcano_raster(), dem_file, overwrite = TRUE)
  dem <- terra::rast(dem_file)

  expect_equal(
    as.integer(terra::crs(pp, describe = TRUE)$code),
    as.integer(terra::crs(dem, describe = TRUE)$code)
  )
  # and the points must fall inside the DEM's extent
  xy <- terra::crds(pp)
  ext <- terra::ext(dem)
  expect_true(all(xy[, 1] >= ext[1] & xy[, 1] <= ext[2]))
  expect_true(all(xy[, 2] >= ext[3] & xy[, 2] <= ext[4]))
})


test_that("ch_contours returns contour lines from a DEM", {
  dem <- ch_volcano_raster()

  contours <- ch_contours(dem)

  expect_s4_class(contours, "SpatVector")
  expect_equal(terra::geomtype(contours), "lines")
  expect_gt(nrow(contours), 0)
  expect_equal(terra::crs(contours), terra::crs(dem))
})

test_that("ch_contours honours n_levels", {
  dem <- ch_volcano_raster()

  few  <- ch_contours(dem, n_levels = 3)
  many <- ch_contours(dem, n_levels = 20)

  expect_lt(nrow(few), nrow(many))
})

test_that("ch_contours honours explicit z_levels", {
  dem <- ch_volcano_raster()
  levels <- c(100, 120, 140, 160, 180)

  contours <- ch_contours(dem, z_levels = levels)

  expect_s4_class(contours, "SpatVector")
  expect_lte(nrow(contours), length(levels))
})

test_that("ch_contours honours zmin and zmax", {
  dem <- ch_volcano_raster()

  contours <- ch_contours(dem, zmin = 120, zmax = 160, n_levels = 5)

  expect_s4_class(contours, "SpatVector")
  expect_gt(nrow(contours), 0)
})

test_that("ch_contours lifts a sea-level contour off zero", {
  # Documented behaviour: a zero lowest level is nudged to 0.1 m so it acts as a
  # coastline. Shifting the DEM down makes the lowest level negative.
  dem <- ch_volcano_raster() - 100

  expect_no_error(ch_contours(dem, n_levels = 5))
})

test_that("ch_contours requires a DEM", {
  expect_error(ch_contours(), "requires a raster dem")
})


test_that("ch_wbt_filenames builds paths under the working directory", {
  wd <- tempfile("wbt_names")
  dir.create(wd)

  fn <- ch_wbt_filenames(wd)

  expect_type(fn, "list")
  expect_named(fn, c("dem", "dem_fsc", "dem_ns", "flowacc", "flowdir",
                     "channel_ras", "catchment_ras", "channel_vec",
                     "catchment_vec", "pp", "pp_snap"))
  expect_true(all(startsWith(unlist(fn), wd)))
  expect_equal(basename(fn$dem), "dem.tif")
  expect_equal(basename(fn$pp_snap), "pp_snap.shp")
})

test_that("ch_wbt_filenames uses the documented extensions", {
  wd <- tempfile("wbt_names")
  dir.create(wd)
  fn <- ch_wbt_filenames(wd)

  # rasters are TIFF, vectors are shapefiles
  expect_true(all(endsWith(unlist(fn[c("dem", "dem_fsc", "dem_ns", "flowacc",
                                       "flowdir", "channel_ras",
                                       "catchment_ras")]), ".tif")))
  expect_true(all(endsWith(unlist(fn[c("channel_vec", "catchment_vec", "pp",
                                       "pp_snap")]), ".shp")))
})

test_that("ch_wbt_filenames accepts custom names", {
  wd <- tempfile("wbt_names")
  dir.create(wd)

  fn <- ch_wbt_filenames(wd, fn_dem = "mydem.tif", fn_pp = "outlets.shp")

  expect_equal(basename(fn$dem), "mydem.tif")
  expect_equal(basename(fn$pp), "outlets.shp")
})

test_that("ch_wbt_filenames rejects a missing working directory", {
  expect_error(ch_wbt_filenames(), "not specified")
  expect_error(ch_wbt_filenames(NULL), "not specified")
  expect_error(ch_wbt_filenames(file.path(tempdir(), "no-such-dir-here")),
               "does not exist")
})


test_that("ch_create_wd creates a directory and reports it", {
  wd <- tempfile("created_wd")

  expect_message(result <- ch_create_wd(wd), "has been created")
  expect_true(result)
  expect_true(dir.exists(wd))

  unlink(wd, recursive = TRUE)
})

test_that("ch_create_wd warns if the directory already exists", {
  wd <- tempfile("existing_wd")
  dir.create(wd)

  expect_warning(result <- ch_create_wd(wd), "exists")
  expect_true(result)

  unlink(wd, recursive = TRUE)
})

test_that("ch_clear_wd removes a directory and its contents", {
  wd <- tempfile("clear_wd")
  dir.create(wd)
  writeLines("scratch", file.path(wd, "a.txt"))
  writeLines("scratch", file.path(wd, "b.txt"))

  # do_check = FALSE so nothing tries to read from the console
  result <- ch_clear_wd(wd, do_check = FALSE)

  expect_match(result, "removed")
  expect_false(dir.exists(wd))
})

test_that("ch_clear_wd handles an already empty directory", {
  wd <- tempfile("clear_empty")
  dir.create(wd)

  expect_match(ch_clear_wd(wd, do_check = FALSE), "removed")
  expect_false(dir.exists(wd))
})

test_that("ch_clear_wd with do_check reaches the confirmation prompt", {
  # do_check = TRUE prompts with readline(). In a non-interactive session
  # readline() returns "" immediately, which is not "n", so the directory is
  # removed. That is the branch exercised here; the "n" (keep it) branch needs
  # an interactive console and so is not covered.
  wd <- tempfile("clear_prompt")
  dir.create(wd)
  writeLines("scratch", file.path(wd, "a.txt"))

  # capture.output keeps the readline prompt out of the test log
  invisible(utils::capture.output(result <- ch_clear_wd(wd, do_check = TRUE)))

  expect_match(result, "removed")
  expect_false(dir.exists(wd))
})
