# Tests for the base-graphics plotting functions.
#
# These functions exist for their side effect -- a plot -- so the assertions
# here are deliberately modest: that each runs to completion on real data,
# returns what it documents, and rejects arguments it says it rejects. Drawing
# happens on a throwaway pdf device (see helper-CSHShydRology.R); nothing here
# compares pixels, which would be brittle across R and graphics-device versions.


# --- ch_axis_doy ------------------------------------------------------------

test_that("ch_axis_doy draws a calendar-year axis", {
  expect_draws({
    plot(1:365, seq_len(365), type = "p", xlab = "", xaxt = "n")
    ch_axis_doy(wyear = 1)
  })
})

test_that("ch_axis_doy draws a water-year axis", {
  expect_draws({
    plot(1:365, seq_len(365), type = "p", xlab = "", xaxt = "n")
    ch_axis_doy(wyear = 10)
  })
})

test_that("ch_axis_doy accepts every month as a water year start", {
  for (m in 1:12) {
    expect_draws({
      plot(1:366, seq_len(366), type = "p", xlab = "", xaxt = "n")
      ch_axis_doy(wyear = m)
    })
  }
})


# --- ch_fdcurve -------------------------------------------------------------

test_that("ch_fdcurve returns exceedance probabilities and flows", {
  out <- expect_draws(ch_fdcurve(test_flows(), normal = FALSE, gust = TRUE))

  expect_s3_class(out, "data.frame")
  expect_named(out, c("exceedance_prob", "flow"))
  expect_equal(nrow(out), nrow(test_flows()))
})

test_that("ch_fdcurve exceedance probabilities are a decreasing 0-1 series", {
  out <- expect_draws(ch_fdcurve(test_flows()))

  expect_true(all(out$exceedance_prob >= 0 & out$exceedance_prob <= 1))
  expect_true(all(diff(out$exceedance_prob) <= 0))
})

test_that("ch_fdcurve flow is a non-decreasing percentage of the mean", {
  flows <- test_flows()
  out <- expect_draws(ch_fdcurve(flows))

  # values are 100 * flow / mean(flow), sorted increasing
  expect_true(all(diff(out$flow) >= 0))
  expect_equal(min(out$flow), 100 * min(flows$Flow) / mean(flows$Flow))
  expect_equal(max(out$flow), 100 * max(flows$Flow) / mean(flows$Flow))
})

test_that("ch_fdcurve works with and without the Gustard curves", {
  a <- expect_draws(ch_fdcurve(test_flows(), gust = TRUE))
  b <- expect_draws(ch_fdcurve(test_flows(), gust = FALSE))

  # the curves are an overlay, so the returned values must be identical
  expect_equal(a, b)
})

test_that("ch_fdcurve works on the normalised probability axis", {
  out <- expect_draws(ch_fdcurve(test_flows(), normal = TRUE, gust = TRUE))

  expect_s3_class(out, "data.frame")
  expect_named(out, c("exceedance_prob", "flow"))
})

test_that("ch_fdcurve reproduces its historical values", {
  # Retained from the original test suite.
  out <- expect_draws(ch_fdcurve(CAN05AA008, normal = FALSE, gust = TRUE))
  expect_true(out$flow[1] <= 9.78 && out$flow[25] >= 11.9)
})


# --- ch_regime_plot ---------------------------------------------------------

test_that("ch_regime_plot draws for a calendar year", {
  expect_draws(ch_regime_plot(test_flows(), colour = TRUE, wyear = 1))
})

test_that("ch_regime_plot draws for a water year and in greyscale", {
  expect_draws(ch_regime_plot(test_flows(), colour = FALSE, wyear = 10))
})

test_that("ch_regime_plot accepts a fixed y maximum", {
  expect_draws(ch_regime_plot(test_flows(), mx = 200))
})

test_that("ch_regime_plot accepts custom quantiles", {
  expect_draws(
    ch_regime_plot(test_flows(),
                   quant = c(0.99, 0.95, 0.8, 0.5, 0.2, 0.05, 0.01))
  )
})


# --- ch_booth_plot ----------------------------------------------------------

test_that("ch_booth_plot draws magnitude and volume plots", {
  flows <- test_flows()
  threshold <- 0.1 * max(flows$Flow)
  events <- suppressMessages(ch_get_peaks(flows, threshold))$POTevents

  expect_draws(ch_booth_plot(events, threshold, title = "05AA008", type = "mag"))
  expect_draws(ch_booth_plot(events, threshold, title = "05AA008", type = "vol"))
})

test_that("ch_booth_plot accepts twelve-colour vectors", {
  flows <- test_flows()
  threshold <- 0.1 * max(flows$Flow)
  events <- suppressMessages(ch_get_peaks(flows, threshold))$POTevents

  expect_draws(
    ch_booth_plot(events, threshold, title = "custom", type = "mag",
                  colour1 = rep("black", 12), colour2 = rep("grey", 12))
  )
})

test_that("ch_booth_plot warns when a colour vector is the wrong length", {
  flows <- test_flows()
  threshold <- 0.1 * max(flows$Flow)
  events <- suppressMessages(ch_get_peaks(flows, threshold))$POTevents

  local_null_device()
  expect_message(
    ch_booth_plot(events, threshold, title = "t", colour1 = c("red", "blue")),
    "must be of length 12"
  )
})


# --- ch_decades_plot --------------------------------------------------------

test_that("ch_decades_plot draws from a ch_binned_MannWhitney result", {
  b <- suppressMessages(
    ch_binned_MannWhitney(CAN05AA008, step = 5,
                          range1 = c(1970, 1979), range2 = c(1990, 1999))
  )

  expect_draws(ch_decades_plot(b))
})

test_that("ch_decades_plot draws for a coarser bin width", {
  b <- suppressMessages(
    ch_binned_MannWhitney(CAN05AA008, step = 30,
                          range1 = c(1970, 1979), range2 = c(1990, 1999))
  )

  expect_draws(ch_decades_plot(b))
})


# --- ch_qa_hydrograph -------------------------------------------------------

test_that("ch_qa_hydrograph returns the documented summary", {
  out <- expect_draws(ch_qa_hydrograph(test_flows()))

  expect_type(out, "list")
  expect_named(out, c("Station", "start_date", "end_date", "points",
                      "SYM_count", "missing_observations"))
  expect_equal(out$points, nrow(test_flows()))
})

test_that("ch_qa_hydrograph counts the quality symbols", {
  flows <- test_flows()
  out <- expect_draws(ch_qa_hydrograph(flows))
  counts <- out$SYM_count

  expect_named(counts, c("Default", "A", "B", "C", "D", "E"))
  # every observation falls into exactly one category
  expect_equal(sum(counts), nrow(flows))
  # and the counts must match the input
  expect_equal(unname(counts["B"]), sum(flows$SYM == "B", na.rm = TRUE))
  expect_equal(unname(counts["E"]), sum(flows$SYM == "E", na.rm = TRUE))
})

test_that("ch_qa_hydrograph honours a date range", {
  out <- expect_draws(
    ch_qa_hydrograph(CAN05AA008, st_date = "1980-01-01", end_date = "1989-12-31")
  )

  expect_equal(out$start_date, as.Date("1980-01-01"))
  expect_equal(out$end_date, as.Date("1989-12-31"))
  expect_lt(out$points, nrow(CAN05AA008))
})

test_that("ch_qa_hydrograph can omit the counts from the legend", {
  # cts = FALSE selects a different legend, which is worth executing
  expect_draws(ch_qa_hydrograph(test_flows(), cts = FALSE))
})

test_that("ch_qa_hydrograph accepts rescale", {
  expect_draws(ch_qa_hydrograph(test_flows(), rescale = TRUE))
})


# --- ch_flow_raster and friends --------------------------------------------

test_that("ch_flow_raster draws a raster plot", {
  expect_draws(ch_flow_raster(test_flows()))
})

test_that("ch_flow_raster accepts a custom palette", {
  expect_draws(ch_flow_raster(test_flows(),
                              rastercolours = c("white", "grey50", "black")))
})

test_that("ch_flow_raster_qa returns TRUE and draws", {
  out <- expect_draws(ch_flow_raster_qa(test_flows()))
  expect_true(out)
})

test_that("ch_flow_raster_trend returns the documented list", {
  out <- expect_draws(ch_flow_raster_trend(test_flows("1970-01-01", "1999-12-31"),
                                           step = 5))

  expect_type(out, "list")
  expect_named(out, c("sID", "na.rm =", "step", "periods", "bins", "period",
                      "med_period", "max_period", "min_period", "tau_period",
                      "prob_period", "year", "median_year", "max_year",
                      "min_year", "tau_median_year", "tau_maximum_year",
                      "tau_minimum_year"))
  expect_equal(out$step, 5)
  expect_equal(out$periods, 73)
  expect_length(out$period, 73)
})

test_that("ch_flow_raster_trend statistics are the right length and range", {
  out <- expect_draws(ch_flow_raster_trend(test_flows("1970-01-01", "1999-12-31"),
                                           step = 5))

  expect_length(out$med_period, out$periods)
  expect_length(out$tau_period, out$periods)
  expect_length(out$prob_period, out$periods)
  expect_true(all(out$tau_period >= -1 & out$tau_period <= 1, na.rm = TRUE))
  expect_true(all(out$prob_period >= 0 & out$prob_period <= 1, na.rm = TRUE))
  # the annual series has one value per year
  expect_length(out$median_year, length(out$year))
})

test_that("ch_flow_raster_trend caps an oversized step", {
  local_null_device()
  expect_message(
    ch_flow_raster_trend(test_flows("1970-01-01", "1999-12-31"), step = 40),
    "has been reset to the maximum allowed"
  )
})


# --- ch_polar_plot and friends ---------------------------------------------

test_that("ch_polar_plot draws from a ch_binned_MannWhitney result", {
  b <- suppressMessages(
    ch_binned_MannWhitney(CAN05AA008, step = 5,
                          range1 = c(1970, 1979), range2 = c(1990, 1999))
  )

  expect_draws(ch_polar_plot(b))
})

test_that("ch_polar_plot_prep assembles the structure ch_polar_plot expects", {
  n <- 73
  prep <- ch_polar_plot_prep(
    station = "05AA008", plot_title = "Test station", step = 5,
    x0 = seq_len(n), x1 = rev(seq_len(n)),
    stat = rep(1, n), prob = rep(0.5, n), test_s = rep(0, n)
  )

  expect_type(prep, "list")
  expect_named(prep, c("StationID", "Station_lname", "variable", "bin_width",
                       "range1", "range2", "p_used", "fail", "bin_method",
                       "test_method", "series"))
  expect_named(prep$series, c("period", "period1", "period2", "stat", "prob",
                              "code"))
  expect_equal(nrow(prep$series), n)
  expect_false(prep$fail)
})

test_that("ch_polar_plot_prep output can be plotted", {
  n <- 73
  prep <- ch_polar_plot_prep(
    station = "05AA008", plot_title = "Test station", step = 5,
    x0 = seq_len(n), x1 = rev(seq_len(n)),
    stat = rep(1, n), prob = rep(0.01, n),
    test_s = rep(c(1, -1, 0), length.out = n)
  )

  expect_draws(ch_polar_plot(prep))
})

test_that("ch_polar_plot_prep reports mismatched input lengths", {
  n <- 10
  ok <- seq_len(n)

  expect_match(
    ch_polar_plot_prep("s", "t", 5, x0 = ok, x1 = seq_len(n + 1),
                       stat = ok, prob = ok, test_s = ok),
    "unequal length"
  )
  expect_match(
    ch_polar_plot_prep("s", "t", 5, x0 = ok, x1 = ok,
                       stat = seq_len(n + 1), prob = ok, test_s = ok),
    "x0 and stat unequal length"
  )
  expect_match(
    ch_polar_plot_prep("s", "t", 5, x0 = ok, x1 = ok,
                       stat = ok, prob = seq_len(n + 1), test_s = ok),
    "x0 and prob unequal length"
  )
  expect_match(
    ch_polar_plot_prep("s", "t", 5, x0 = ok, x1 = ok,
                       stat = ok, prob = ok, test_s = seq_len(n + 1)),
    "x0 and test_s unequal length"
  )
})


# --- ch_polar_plot_peaks ---------------------------------------------------

test_that("ch_polar_plot_peaks draws the empty base plot", {
  expect_draws(ch_polar_plot_peaks())
})

test_that("ch_polar_plot_peaks draws with regime shading", {
  expect_draws(ch_polar_plot_peaks(shading = TRUE))
})

test_that("ch_polar_plot_peaks plots peak days around the perimeter", {
  amax <- ch_sh_get_amax(test_flows())

  expect_draws(ch_polar_plot_peaks(days = amax$doy, title = "05AA008"))
})

test_that("ch_polar_plot_peaks plots a centroid", {
  amax <- ch_sh_get_amax(test_flows())
  amax <- amax[amax$days >= 365, ]
  m_r <- ch_circ_mean_reg(amax)

  expect_draws(
    ch_polar_plot_peaks(direction = m_r$mean, regularity = m_r$regularity,
                        title = "05AA008")
  )
})

test_that("ch_polar_plot_peaks plots peaks and centroid together", {
  amax <- ch_sh_get_amax(test_flows())
  amax <- amax[amax$days >= 365, ]
  m_r <- ch_circ_mean_reg(amax)

  expect_draws(
    ch_polar_plot_peaks(days = amax$doy, direction = m_r$mean,
                        regularity = m_r$regularity, title = "05AA008")
  )
})

test_that("ch_polar_plot_peaks accepts labels and a detail matrix", {
  amax <- ch_sh_get_amax(test_flows())
  amax <- amax[amax$days >= 365, ]
  m_r <- ch_circ_mean_reg(amax)

  expect_draws(
    ch_polar_plot_peaks(direction = m_r$mean, regularity = m_r$regularity,
                        labels = "centroid", label_pos = 3)
  )

  detail <- matrix(c("21", "black", "red", "1.2"), nrow = 1)
  expect_draws(
    ch_polar_plot_peaks(direction = m_r$mean, regularity = m_r$regularity,
                        in_detail = detail)
  )
})

test_that("ch_polar_plot_peaks restores graphics parameters", {
  # Regression test: a second on.exit() call without add = TRUE used to replace
  # the par-restoring one, so the function leaked its cex.lab and col.lab
  # settings into the caller's device.
  local_null_device()
  before <- par(c("cex.lab", "col.lab"))
  suppressMessages(ch_polar_plot_peaks())
  after <- par(c("cex.lab", "col.lab"))

  expect_equal(after, before)
})
