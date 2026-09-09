# ch_ffa_screen_plot draws a Gumbel flood-frequency plot and returns the outlier
# codes. It is slow (MGBT's low-outlier test dominates), so the heavier cases are
# kept off CRAN.

test_that("ch_ffa_screen_plot returns the documented list", {
  skip_on_cran()

  amax <- ch_sh_get_amax(test_flows())
  out <- expect_draws(ch_ffa_screen_plot(amax))

  expect_type(out, "list")
  expect_named(out, c("Station", "n_events", "amax", "outlier_index"))
  expect_equal(out$Station, "unspecified")
  expect_equal(out$n_events, nrow(amax))
  expect_equal(out$amax, amax$amax)
})

test_that("ch_ffa_screen_plot codes every event as low, normal or high", {
  skip_on_cran()

  amax <- ch_sh_get_amax(test_flows())
  out <- expect_draws(ch_ffa_screen_plot(amax))

  expect_length(out$outlier_index, nrow(amax))
  # 1 = low outlier, 2 = not an outlier, 3 = high outlier
  expect_true(all(out$outlier_index %in% c(1, 2, 3)))
})

test_that("ch_ffa_screen_plot records the station and title it was given", {
  skip_on_cran()

  amax <- ch_sh_get_amax(test_flows())
  out <- expect_draws(
    ch_ffa_screen_plot(amax, stn = "05AA008", mtitle = "Crowsnest River")
  )

  expect_equal(out$Station, "05AA008")
})

test_that("ch_ffa_screen_plot handles a series with no outliers", {
  skip_on_cran()

  # A tidy, gently increasing series: nothing should be flagged either way.
  amax <- data.frame(
    amax = seq(100, 133, by = 3),
    maxdate = as.Date(paste0(1990:2001, "-06-15")),
    doy = 166
  )

  out <- expect_draws(ch_ffa_screen_plot(amax))

  expect_true(all(out$outlier_index == 2))
})

test_that("ch_ffa_screen_plot flags an obvious high outlier", {
  skip_on_cran()

  set.seed(11)
  amax <- data.frame(
    amax = c(runif(25, 90, 110), 900),
    maxdate = as.Date(paste0(1990:2015, "-06-15")),
    doy = 166
  )

  out <- expect_draws(ch_ffa_screen_plot(amax))

  expect_equal(out$outlier_index[26], 3)
})

test_that("ch_ffa_screen_plot uses the alternative legend layout for small flows", {
  skip_on_cran()

  # The legend is flipped when max(Q) <= 350, which is a separate code path.
  set.seed(12)
  amax <- data.frame(
    amax = runif(25, 5, 40),
    maxdate = as.Date(paste0(1990:2014, "-06-15")),
    doy = 166
  )

  out <- expect_draws(ch_ffa_screen_plot(amax))
  expect_length(out$outlier_index, nrow(amax))
})

test_that("ch_ffa_screen_plot spreads event months around the circle", {
  skip_on_cran()

  # Events in every month exercise the circular colour lookup for all 12 slots.
  amax <- data.frame(
    amax = seq(400, 400 + 11 * 20, by = 20),
    maxdate = as.Date(sprintf("199%d-%02d-15", 0:11 %/% 10, 1:12)),
    doy = as.numeric(format(as.Date(sprintf("199%d-%02d-15", 0:11 %/% 10, 1:12)), "%j"))
  )

  out <- expect_draws(ch_ffa_screen_plot(amax))
  expect_length(out$outlier_index, 12)
})
