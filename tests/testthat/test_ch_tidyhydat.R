# Tests for the tidyhydat adapters.
#
# These use the small test database that ships with tidyhydat rather than the
# user's real HYDAT install, so they need no download and touch nothing outside
# the session. They skip if tidyhydat is not installed.

test_that("ch_tidyhydat_ECDE converts a single station to an ECDE data frame", {
  local_hydat_test_db()

  flows <- tidyhydat::hy_daily_flows(station_number = "05AA008")
  out <- suppressMessages(ch_tidyhydat_ECDE(flows))

  expect_s3_class(out, "data.frame")
  # a plain data frame, not a tibble, and with the ECDE names
  expect_false(inherits(out, "tbl_df"))
  expect_named(out, c("ID", "PARAM", "Date", "Flow", "SYM"))
  expect_equal(nrow(out), nrow(flows))
})

test_that("ch_tidyhydat_ECDE recodes Parameter to the ECDE integer codes", {
  local_hydat_test_db()

  flows <- tidyhydat::hy_daily_flows(station_number = "05AA008")
  out <- suppressMessages(ch_tidyhydat_ECDE(flows))

  expect_type(out$PARAM, "integer")
  # 1 for Flow, 2 for Level
  expect_true(all(out$PARAM %in% c(1L, 2L)))
  expect_equal(sum(out$PARAM == 1L), sum(flows$Parameter == "Flow"))
})

test_that("ch_tidyhydat_ECDE replaces missing symbols with empty strings", {
  local_hydat_test_db()

  flows <- tidyhydat::hy_daily_flows(station_number = "05AA008")
  out <- suppressMessages(ch_tidyhydat_ECDE(flows))

  # ECDE files use "" rather than NA for an absent quality flag
  expect_false(anyNA(out$SYM))
})

test_that("ch_tidyhydat_ECDE returns a list of frames for several stations", {
  local_hydat_test_db()

  stations <- c("05AA008", "08MF005")
  flows <- tidyhydat::hy_daily_flows(station_number = stations)
  out <- suppressMessages(ch_tidyhydat_ECDE(flows))

  expect_type(out, "list")
  expect_false(inherits(out, "data.frame"))
  expect_length(out, length(stations))
  # split() names the elements by station, in increasing alphabetical order
  expect_equal(names(out), sort(stations))
  for (el in out) {
    expect_s3_class(el, "data.frame")
    expect_named(el, c("ID", "PARAM", "Date", "Flow", "SYM"))
    expect_equal(length(unique(el$ID)), 1)
  }
})

test_that("ch_tidyhydat_ECDE says how many stations it split", {
  local_hydat_test_db()

  flows <- tidyhydat::hy_daily_flows(station_number = c("05AA008", "08MF005"))
  expect_message(ch_tidyhydat_ECDE(flows), "A list of dataframes is returned")
})

test_that("ch_tidyhydat_ECDE output can be fed to the ECDE-shaped functions", {
  local_hydat_test_db()

  flows <- suppressMessages(
    ch_tidyhydat_ECDE(tidyhydat::hy_daily_flows(station_number = "05AA008"))
  )
  # The point of the adapter is that downstream functions accept the result
  amax <- ch_sh_get_amax(flows)

  expect_s3_class(amax, "data.frame")
  expect_named(amax, c("Year", "amax", "maxdate", "doy", "days"))
})


test_that("ch_tidyhydat_ECDE_meta returns the documented three-part list", {
  local_hydat_test_db()

  out <- suppressMessages(
    ch_tidyhydat_ECDE_meta(c("05AA008", "08MF005", "05HD008"))
  )

  expect_type(out, "list")
  expect_named(out, c("meta", "H_version", "th_meta"))
  expect_s3_class(out$meta, "data.frame")
  expect_s3_class(out$th_meta, "data.frame")
})

test_that("ch_tidyhydat_ECDE_meta produces ECDE-style column names", {
  local_hydat_test_db()

  out <- suppressMessages(ch_tidyhydat_ECDE_meta(c("05AA008", "08MF005")))
  meta <- out$meta

  expect_equal(
    names(meta),
    c("Station", "StationName", "HydStatus", "Prov", "Latitude", "Longitude",
      "DrainageArea", "EffectiveDrainageArea", "Years", "From", "To", "Reg.",
      "Flow", "Level", "Sed", "OperSched", "RealTime", "RHBN", "Region",
      "Datum", "Agency")
  )
  expect_equal(sort(meta$Station), c("05AA008", "08MF005"))
})

test_that("ch_tidyhydat_ECDE_meta reports the HYDAT version", {
  local_hydat_test_db()

  expect_message(ch_tidyhydat_ECDE_meta("05AA008"), "HYDAT version")
})

test_that("ch_tidyhydat_ECDE_meta works for a single station with all_ECDE", {
  # Regression test. A dangling if (nrow(meta) > 1) guarded only the first of
  # the three lookup-table assignments, so for one station `regions` was never
  # created and the function failed with "object 'regions' not found".
  skip_on_cran()
  local_hydat_test_db()

  out <- suppressMessages(ch_tidyhydat_ECDE_meta("05AA008", all_ECDE = TRUE))

  expect_s3_class(out$meta, "data.frame")
  expect_equal(nrow(out$meta), 1)
  expect_equal(out$meta$Station, "05AA008")
})

test_that("ch_tidyhydat_ECDE_meta resolves codes to names when all_ECDE", {
  skip_on_cran()
  local_hydat_test_db()

  plain <- suppressMessages(ch_tidyhydat_ECDE_meta("05AA008"))
  full  <- suppressMessages(ch_tidyhydat_ECDE_meta("05AA008", all_ECDE = TRUE))

  # Without all_ECDE the Region is a numeric office id; with it, a name
  expect_type(full$meta$Region, "character")
  expect_false(identical(plain$meta$Region, full$meta$Region))
  # and the logical data-availability flags get filled in
  expect_type(full$meta$Flow, "logical")
})

test_that("ch_tidyhydat_ECDE_meta output works as ch_get_wscstation metadata", {
  local_hydat_test_db()

  meta <- suppressMessages(ch_tidyhydat_ECDE_meta(c("05AA008", "08MF005")))$meta
  info <- ch_get_wscstation("05AA008", metadata = meta)

  expect_equal(nrow(info), 1)
  expect_true(grepl("05AA008", info$Station_lname, fixed = TRUE))
})


test_that("ch_gg_hydrographs returns a ggplot for daily flows", {
  skip_on_cran()
  local_hydat_test_db()

  p <- suppressWarnings(suppressMessages(
    ch_gg_hydrographs("05AA008", daily = TRUE, instantaneous = FALSE)
  ))

  expect_s3_class(p, "ggplot")
})

test_that("ch_gg_hydrographs facets several stations", {
  skip_on_cran()
  local_hydat_test_db()

  p <- suppressWarnings(suppressMessages(
    ch_gg_hydrographs(c("05AA008", "08MF005"), daily = TRUE,
                      instantaneous = FALSE, facets = TRUE)
  ))

  expect_s3_class(p, "ggplot")
})

test_that("ch_gg_hydrographs colours several stations when not facetted", {
  skip_on_cran()
  local_hydat_test_db()

  p <- suppressWarnings(suppressMessages(
    ch_gg_hydrographs(c("05AA008", "08MF005"), daily = TRUE,
                      instantaneous = FALSE, facets = FALSE)
  ))

  expect_s3_class(p, "ggplot")
})

test_that("ch_gg_hydrographs honours a date range", {
  skip_on_cran()
  local_hydat_test_db()

  p <- suppressWarnings(suppressMessages(
    ch_gg_hydrographs("05AA008", daily = TRUE, instantaneous = FALSE,
                      start_date = "1980-01-01", end_date = "1980-12-31")
  ))

  expect_s3_class(p, "ggplot")
})

test_that("ch_gg_hydrographs rejects impossible requests", {
  skip_if_not_installed("tidyhydat")

  expect_error(
    ch_gg_hydrographs("05AA008", daily = FALSE, instantaneous = FALSE),
    "No plots selected"
  )
  expect_error(
    ch_gg_hydrographs(character(0), daily = TRUE),
    "No stations selected"
  )
})

test_that("ch_gg_hydrographs plots annual instantaneous peaks alone", {
  skip_on_cran()
  local_hydat_test_db()

  p <- suppressWarnings(suppressMessages(
    ch_gg_hydrographs("05AA008", daily = FALSE, instantaneous = TRUE)
  ))

  expect_s3_class(p, "ggplot")
})

test_that("ch_gg_hydrographs plots instantaneous peaks for several stations", {
  skip_on_cran()
  local_hydat_test_db()

  facetted <- suppressWarnings(suppressMessages(
    ch_gg_hydrographs(c("05AA008", "08MF005"), daily = FALSE,
                      instantaneous = TRUE, facets = TRUE)
  ))
  coloured <- suppressWarnings(suppressMessages(
    ch_gg_hydrographs(c("05AA008", "08MF005"), daily = FALSE,
                      instantaneous = TRUE, facets = FALSE)
  ))

  expect_s3_class(facetted, "ggplot")
  expect_s3_class(coloured, "ggplot")
})

test_that("ch_gg_hydrographs plots daily flows and peaks together", {
  skip_on_cran()
  local_hydat_test_db()

  single <- suppressWarnings(suppressMessages(
    ch_gg_hydrographs("05AA008", daily = TRUE, instantaneous = TRUE,
                      facets = FALSE)
  ))
  multi <- suppressWarnings(suppressMessages(
    ch_gg_hydrographs(c("05AA008", "08MF005"), daily = TRUE,
                      instantaneous = TRUE, facets = FALSE)
  ))
  facetted <- suppressWarnings(suppressMessages(
    ch_gg_hydrographs(c("05AA008", "08MF005"), daily = TRUE,
                      instantaneous = TRUE, facets = TRUE)
  ))

  expect_s3_class(single, "ggplot")
  expect_s3_class(multi, "ggplot")
  expect_s3_class(facetted, "ggplot")
})

test_that("ch_gg_hydrographs can restrict series to a common date range", {
  skip_on_cran()
  local_hydat_test_db()

  stations <- c("05AA008", "08MF005")

  daily_only <- suppressWarnings(suppressMessages(
    ch_gg_hydrographs(stations, daily = TRUE, instantaneous = FALSE,
                      common_dates = TRUE)
  ))
  inst_only <- suppressWarnings(suppressMessages(
    ch_gg_hydrographs(stations, daily = FALSE, instantaneous = TRUE,
                      common_dates = TRUE)
  ))
  both <- suppressWarnings(suppressMessages(
    ch_gg_hydrographs(stations, daily = TRUE, instantaneous = TRUE,
                      common_dates = TRUE)
  ))

  expect_s3_class(daily_only, "ggplot")
  expect_s3_class(inst_only, "ggplot")
  expect_s3_class(both, "ggplot")
})

test_that("ch_gg_hydrographs returns a plot that actually renders", {
  # Building the plot is a stronger check than inspecting the object: it is what
  # catches a missing aesthetic or an empty layer, which a bare ggplot object
  # will happily carry around until someone tries to draw it.
  skip_on_cran()
  local_hydat_test_db()

  p <- suppressWarnings(suppressMessages(
    ch_gg_hydrographs("05AA008", daily = TRUE, instantaneous = FALSE)
  ))

  built <- suppressWarnings(ggplot2::ggplot_build(p))
  expect_s3_class(built, "ggplot_built")
  expect_gt(nrow(built$data[[1]]), 0)
  expect_equal(p$labels$x, "")
})

test_that("ch_gg_hydrographs treats NULL colours as black", {
  skip_on_cran()
  local_hydat_test_db()

  p <- suppressWarnings(suppressMessages(
    ch_gg_hydrographs("05AA008", daily = TRUE, instantaneous = FALSE,
                      daily_colour = NULL, inst_colour = NULL)
  ))

  expect_s3_class(p, "ggplot")
})
