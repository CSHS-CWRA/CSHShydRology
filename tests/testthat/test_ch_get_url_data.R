# Tests for ch_get_url_data.
#
# The function decides how to read a file from its extension and, when the file
# is already on disk, reads it without going near the network. That local-cache
# path is the interesting logic and is what these tests exercise; a genuine
# download is covered by one network test that skips on CRAN and when offline.

test_that("ch_get_url_data reads a cached csv without downloading", {
  csv <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(a = 1:3, b = c("x", "y", "z")), csv,
                   row.names = FALSE)

  # The url is deliberately unusable: if it were contacted, this would fail.
  out <- ch_get_url_data("http://localhost:1/never-fetched.csv", csv)

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 3)
  expect_named(out, c("a", "b"))
})

test_that("ch_get_url_data reads a cached tif without downloading", {
  # Regression test: the tif branch only assigned `result` inside the
  # "file does not exist" block, so reading an already-downloaded raster failed
  # with "object 'result' not found" -- defeating the caching the function
  # documents.
  tif <- tempfile(fileext = ".tif")
  terra::writeRaster(ch_volcano_raster(), tif, overwrite = TRUE)

  out <- ch_get_url_data("http://localhost:1/never-fetched.tif", tif)

  expect_s4_class(out, "SpatRaster")
  expect_equal(terra::ncell(out), terra::ncell(ch_volcano_raster()))
})

test_that("ch_get_url_data reads a cached GeoJSON without downloading", {
  gj <- tempfile(fileext = ".GeoJSON")
  pts <- terra::vect(data.frame(x = c(1, 2), y = c(3, 4)),
                     geom = c("x", "y"), crs = "EPSG:4326")
  terra::writeVector(pts, gj, filetype = "GeoJSON", overwrite = TRUE)

  out <- ch_get_url_data("http://localhost:1/never-fetched.GeoJSON", gj)

  expect_s4_class(out, "SpatVector")
  expect_equal(nrow(out), 2)
})

test_that("ch_get_url_data returns the error string when stop_on_error is FALSE", {
  missing_csv <- tempfile(fileext = ".csv")

  out <- ch_get_url_data("http://localhost:1/nothing-here.csv", missing_csv,
                         stop_on_error = FALSE)

  expect_type(out, "character")
  expect_length(out, 1)
  expect_false(out == "OK")
})

test_that("ch_get_url_data stops on a failed download by default", {
  missing_csv <- tempfile(fileext = ".csv")

  expect_error(
    ch_get_url_data("http://localhost:1/nothing-here.csv", missing_csv),
    "Error in accessing"
  )
})

test_that("ch_get_url_data reports failures for tif and GeoJSON too", {
  expect_error(
    ch_get_url_data("http://localhost:1/nope.tif", tempfile(fileext = ".tif")),
    "Error in accessing"
  )
  expect_error(
    ch_get_url_data("http://localhost:1/nope.GeoJSON",
                    tempfile(fileext = ".GeoJSON")),
    "Error in accessing"
  )

  expect_type(
    ch_get_url_data("http://localhost:1/nope.tif", tempfile(fileext = ".tif"),
                    stop_on_error = FALSE),
    "character"
  )
})

test_that("ch_get_url_data returns NULL for an unhandled extension", {
  # Only csv, tif/tiff and GeoJSON are recognised; anything else falls through.
  other <- tempfile(fileext = ".nc")
  writeLines("not really netcdf", other)

  expect_null(ch_get_url_data("http://localhost:1/x.nc", other))
})

test_that("ch_get_url_data downloads a real file", {
  skip_on_cran()
  skip_if_offline()

  dest <- tempfile(fileext = ".csv")
  out <- ch_get_url_data(
    "https://zenodo.org/record/4781469/files/sm_data.csv",
    dest,
    stop_on_error = FALSE
  )

  # The remote host may be unavailable, which is a fact about the world rather
  # than a bug here, so treat a returned error string as a skip.
  if (is.character(out)) {
    skip(paste("remote data set unavailable:", out))
  }

  expect_s3_class(out, "data.frame")
  expect_gt(nrow(out), 0)
  expect_true(file.exists(dest))

  # A second call must read the file from disk rather than fetch it again
  again <- ch_get_url_data("http://localhost:1/not-used.csv", dest)
  expect_equal(again, out)
})
