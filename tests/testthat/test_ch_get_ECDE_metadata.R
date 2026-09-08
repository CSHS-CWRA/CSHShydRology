test_that("ch_get_ECDE_metadata reads a favourite-stations export", {
  meta <- ch_get_ECDE_metadata(fixture_path("ecde_metadata_sample.tb0"))

  expect_s3_class(meta, "data.frame")
  # The "Fav" column is read but dropped, leaving 20 of the 21 fields
  expect_equal(ncol(meta), 20)
  expect_equal(nrow(meta), 3)
  expect_equal(names(meta)[1:4], c("Station", "StationName", "HydStatus", "Prov"))
  expect_false("Fav" %in% names(meta))
})

test_that("ch_get_ECDE_metadata keeps the station identifiers and coordinates", {
  meta <- ch_get_ECDE_metadata(fixture_path("ecde_metadata_sample.tb0"))

  expect_equal(meta$Station, c("05AA008", "05BB001", "05BA001"))
  expect_type(meta$Latitude, "double")
  expect_type(meta$Longitude, "double")
  # Canadian stations: northern hemisphere, western longitudes
  expect_true(all(meta$Latitude > 40 & meta$Latitude < 85))
  expect_true(all(meta$Longitude < 0))
})

test_that("ch_get_ECDE_metadata can write the result to a csv", {
  out <- tempfile(fileext = ".csv")
  meta <- ch_get_ECDE_metadata(fixture_path("ecde_metadata_sample.tb0"),
                               writefile = out)

  expect_true(file.exists(out))
  round_trip <- utils::read.csv(out)
  expect_equal(nrow(round_trip), nrow(meta))
  expect_equal(ncol(round_trip), ncol(meta))
  expect_equal(round_trip$Station, meta$Station)
})

test_that("ch_get_ECDE_metadata rejects a missing or unspecified file", {
  expect_error(ch_get_ECDE_metadata(""), "not specified")
  expect_error(
    ch_get_ECDE_metadata(file.path(tempdir(), "no-such-favourites.tb0")),
    "not found"
  )
})
