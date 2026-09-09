make_flows <- function(n_series = 2) {
  dd <- seq.Date(as.Date("2010-10-01"), as.Date("2013-09-30"), by = 1)
  set.seed(101)
  df <- data.frame(Date = dd)
  for (i in seq_len(n_series)) {
    df[[paste0("series", i)]] <- abs(rnorm(length(dd))) * i
  }
  df
}

make_precip <- function() {
  dd <- seq.Date(as.Date("2010-10-01"), as.Date("2013-09-30"), by = 1)
  set.seed(202)
  data.frame(Date = dd, precip = abs(rnorm(length(dd))) * 10)
}


test_that("ch_model_hydrograph returns TRUE for a basic plot", {
  out <- expect_draws(ch_model_hydrograph(flows = make_flows(),
                                          winter_shading = FALSE))
  expect_true(out)
})

test_that("ch_model_hydrograph draws with winter shading and labels", {
  out <- expect_draws(
    ch_model_hydrograph(flows = make_flows(), winter_shading = TRUE,
                        flow_labels = c("simulated", "observed"))
  )
  expect_true(out)
})

test_that("ch_model_hydrograph draws with precipitation on a second axis", {
  out <- expect_draws(
    ch_model_hydrograph(flows = make_flows(), precip = make_precip(),
                        range_mult_flow = 1.7, range_mult_precip = 2,
                        leg_box = TRUE)
  )
  expect_true(out)
})

test_that("ch_model_hydrograph honours a plotting period", {
  out <- expect_draws(
    ch_model_hydrograph(flows = make_flows(), prd = "2011-10-01/2012-09-30")
  )
  expect_true(out)
})

test_that("ch_model_hydrograph accepts custom labels and legend placement", {
  out <- expect_draws(
    ch_model_hydrograph(flows = make_flows(), ylabel = "Q",
                        precip_label = "P", leg_pos = "topright",
                        leg_box = FALSE, zero_axis = FALSE)
  )
  expect_true(out)
})

test_that("ch_model_hydrograph draws a single flow series", {
  out <- expect_draws(ch_model_hydrograph(flows = make_flows(1)))
  expect_true(out)
})

test_that("ch_model_hydrograph rejects a non-data-frame flows argument", {
  local_null_device()
  expect_error(ch_model_hydrograph(flows = 1:10), "must be a data frame")
})

test_that("ch_model_hydrograph rejects an empty flows data frame", {
  local_null_device()
  empty <- make_flows()[0, ]
  expect_error(ch_model_hydrograph(flows = empty), "zero rows")
})

test_that("ch_model_hydrograph requires a data column beside Date", {
  local_null_device()
  expect_error(
    ch_model_hydrograph(flows = data.frame(Date = Sys.Date())),
    "no data columns"
  )
})

test_that("ch_model_hydrograph requires Date to be the first column", {
  local_null_device()
  df <- make_flows()
  df <- df[, c("series1", "Date", "series2")]
  expect_error(ch_model_hydrograph(flows = df), "must be the first attribute")
})

test_that("ch_model_hydrograph rejects too many flow columns", {
  local_null_device()
  expect_error(ch_model_hydrograph(flows = make_flows(12)),
               "more than 11 data columns")
})

test_that("ch_model_hydrograph checks the number of flow labels", {
  local_null_device()
  expect_error(
    ch_model_hydrograph(flows = make_flows(2), flow_labels = "only one"),
    "same number of labels"
  )
})
