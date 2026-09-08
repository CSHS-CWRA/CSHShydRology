# Shared helpers for the CSHShydRology test suite.
#
# testthat loads every helper-*.R file before running the tests, so everything
# defined here is available to all test files.


# --- Locating the sample data files ----------------------------------------
#
# The fixtures live in tests/testthat/fixtures/. testthat sets the working
# directory to tests/testthat/ when the tests run, so a relative path works
# both under devtools::test() and under R CMD check.

fixture_path <- function(...) {
  testthat::test_path("fixtures", ...)
}


# --- Skip helpers -----------------------------------------------------------
#
# Two different things can be missing, and they are worth distinguishing:
#
#   * the whitebox R package, which is what DESCRIPTION can declare; and
#   * the WhiteboxTools executable, which whitebox merely wraps and which has
#     to be downloaded separately with whitebox::install_whitebox().
#
# A machine can easily have the first without the second, in which case every
# ch_wbt_* function stops immediately. Gate on the executable, not just the
# package, or these tests fail rather than skip.

skip_if_no_whitebox <- function() {
  testthat::skip_if_not_installed("whitebox")
  found <- tryCatch(
    whitebox::check_whitebox_binary(silent = TRUE),
    error = function(e) FALSE
  )
  if (!isTRUE(found)) {
    testthat::skip("WhiteboxTools executable not found (see whitebox::install_whitebox())")
  }

  # Run WhiteboxTools single-threaded during the tests. Each ch_wbt_* call
  # launches the WhiteboxTools binary as a subprocess, and by default it spawns
  # one worker thread per core. A full test run makes dozens of those calls back
  # to back, and under that load the binary intermittently panics
  # (see the breach_leastcost test for the details). Capping it at one process
  # is much steadier and costs nothing on a DEM this small.
  whitebox::wbt_options(max_procs = 1)

  invisible(TRUE)
}

# tidyhydat ships a small test database, which is enough for the functions that
# only need HYDAT's shape rather than its full contents. Using it keeps these
# tests off the network and out of the user's real HYDAT install.

skip_if_no_hydat_test_db <- function() {
  testthat::skip_if_not_installed("tidyhydat")
  db <- tryCatch(tidyhydat::hy_test_db(), error = function(e) NA_character_)
  if (is.na(db) || !file.exists(db)) {
    testthat::skip("tidyhydat test database not available")
  }
  invisible(db)
}

# Point tidyhydat at its bundled test database for the duration of one test,
# restoring whatever was set before. `env` should be the calling test's
# environment, which withr uses to decide when to undo the change.

local_hydat_test_db <- function(env = parent.frame()) {
  db <- skip_if_no_hydat_test_db()
  old <- Sys.getenv("hydat", unset = NA)
  tidyhydat::hy_set_default_db(db)
  withr::defer(
    {
      if (is.na(old)) {
        tidyhydat::hy_set_default_db(NULL)
      } else {
        Sys.setenv(hydat = old)
      }
    },
    envir = env
  )
  invisible(db)
}


# --- Testing functions that draw ------------------------------------------
#
# Many CSHShydRology functions exist to produce a base-graphics plot and are
# called for that side effect. The useful thing to assert is that they run to
# completion on real data and return what they claim to, so these helpers send
# the output to a throwaway device and tidy up afterwards.
#
# A file-backed pdf() device is used rather than pdf(NULL) because several of
# these functions call layout(), frame() and grconvertX(), which need a device
# with real dimensions.

local_null_device <- function(env = parent.frame()) {
  grDevices::pdf(file = tempfile(fileext = ".pdf"), width = 9, height = 7)
  withr::defer(grDevices::dev.off(), envir = env)
  invisible(TRUE)
}

# Run `code` on a throwaway device and return its value. Messages are silenced
# because most of these functions are chatty by design.
draw_quietly <- function(code) {
  grDevices::pdf(file = tempfile(fileext = ".pdf"), width = 9, height = 7)
  on.exit(grDevices::dev.off(), add = TRUE)
  suppressMessages(force(code))
}

# Assert that `code` draws something without error, and return its value
# invisibly so a test can make further assertions about it.
expect_draws <- function(code) {
  value <- NULL
  testthat::expect_no_error(value <- draw_quietly(code))
  invisible(value)
}


# --- Small shared test data -------------------------------------------------

# A short slice of the bundled Crowsnest River record. Most tests do not need
# all 25,252 rows, and a decade keeps them fast while still spanning whole
# years, freshets and quality flags.
test_flows <- function(from = "1990-01-01", to = "1999-12-31") {
  df <- CSHShydRology::CAN05AA008
  df[df$Date >= as.Date(from) & df$Date <= as.Date(to), ]
}

# A DEM plus pour points written to a fresh working directory, which is what
# the whitebox pipeline needs as a starting point. Returns the file names list
# used by the ch_wbt_* functions, with the DEM already on disk.
local_wbt_project <- function(env = parent.frame()) {
  wd <- tempfile("wbt_")
  dir.create(wd, recursive = TRUE, showWarnings = FALSE)
  withr::defer(unlink(wd, recursive = TRUE), envir = env)

  fn <- ch_wbt_filenames(wd)
  terra::writeRaster(ch_volcano_raster(), fn$dem, overwrite = TRUE)
  fn$wd <- wd
  fn
}
