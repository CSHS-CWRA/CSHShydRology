context("Testing ch_regime_plot")

# Asserts only that the call runs to completion without throwing. Nothing here
# inspects what was drawn, so these pin the calling contract -- that arguments
# still bind as they did, that the guards fire -- and not the graphics.
#
# pdf(NULL) sends output to a null device, which also stops base plotting from
# leaving an Rplots.pdf behind in the working directory. force() is required:
# `expr` arrives as an unevaluated promise, so without it the plotting call
# would never run and the test would pass while testing nothing.
quiet_plot <- function(expr) {
  pdf(NULL)
  on.exit(dev.off())
  force(expr)
  invisible(TRUE)
}

test_that("the original data frame interface still works", {
  expect_true(quiet_plot(ch_regime_plot(CAN05AA008, colour = TRUE, wyear = 1)))
})

test_that("existing positional calls are unaffected by the added arguments", {
  # The date/flow/data/id arguments were appended after `quant`, so every
  # position of the original signature must still mean what it did.
  expect_true(quiet_plot(ch_regime_plot(CAN05AA008, 10)))
  expect_true(quiet_plot(ch_regime_plot(CAN05AA008, 10, FALSE)))
  expect_true(quiet_plot(ch_regime_plot(CAN05AA008, 1, TRUE, 200)))
  expect_true(quiet_plot(ch_regime_plot(CAN05AA008, 10, FALSE, 300, NULL)))
  expect_true(quiet_plot(
    ch_regime_plot(CAN05AA008, 1, TRUE, 1, NULL,
                   c(0.99, 0.9, 0.75, 0.5, 0.25, 0.1, 0.01))
  ))
})

test_that("date and flow can be given as columns of DF", {
  expect_true(quiet_plot(
    ch_regime_plot(CAN05AA008, date = Date, flow = Flow, id = ID)
  ))
})

test_that("date and flow can be given as vectors, with no station id", {
  expect_true(quiet_plot(
    ch_regime_plot(date = CAN05AA008$Date, flow = CAN05AA008$Flow)
  ))
})

test_that("plot() defaults can be overridden through ...", {
  expect_true(quiet_plot(
    ch_regime_plot(CAN05AA008, date = Date, flow = Flow, xlim = c(90, 220))
  ))
})

test_that("incomplete or absent input is reported clearly", {
  expect_error(ch_regime_plot(), "either")
  expect_error(ch_regime_plot(date = CAN05AA008$Date), "both")
  expect_error(ch_regime_plot(flow = CAN05AA008$Flow), "both")
})

test_that("a malformed data frame is reported clearly", {
  expect_error(ch_regime_plot(data.frame(Date = Sys.Date())), "`Date` and `Flow`")
  expect_error(ch_regime_plot(1:10), "must be a data frame")
})

test_that("more than one station id is rejected", {
  expect_error(
    ch_regime_plot(date = CAN05AA008$Date, flow = CAN05AA008$Flow,
                   id = c("05AA008", "01AD002")),
    "more than one station"
  )
})

test_that("DF supplies both the data and the named columns", {
  # the same plot, reached two ways
  expect_true(quiet_plot(ch_regime_plot(CAN05AA008)))
  expect_true(quiet_plot(ch_regime_plot(CAN05AA008, date = Date, flow = Flow)))
})
