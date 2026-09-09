test_that("ch_get_peaks returns the documented list", {
  flows <- test_flows()
  threshold <- 0.5 * max(flows$Flow)

  peaks <- suppressMessages(ch_get_peaks(flows, threshold))

  expect_type(peaks, "list")
  expect_named(peaks, c("POTevents", "ncases", "case"))
  expect_s3_class(peaks$POTevents, "data.frame")
  expect_type(peaks$case, "list")
})

test_that("ch_get_peaks POTevents has the documented five columns", {
  flows <- test_flows()
  peaks <- suppressMessages(ch_get_peaks(flows, 0.5 * max(flows$Flow)))
  events <- peaks$POTevents

  expect_named(events, c("st_date", "max_date", "max", "volume", "duration"))
  expect_s3_class(events$st_date, "Date")
  expect_s3_class(events$max_date, "Date")
  expect_type(events$max, "double")
  expect_gt(nrow(events), 0)
})

test_that("ch_get_peaks ncases matches the number of events", {
  flows <- test_flows()
  peaks <- suppressMessages(ch_get_peaks(flows, 0.5 * max(flows$Flow)))

  expect_equal(peaks$ncases, nrow(peaks$POTevents))
})

test_that("ch_get_peaks only reports events above the threshold", {
  flows <- test_flows()
  threshold <- 0.5 * max(flows$Flow)

  peaks <- suppressMessages(ch_get_peaks(flows, threshold))

  expect_true(all(peaks$POTevents$max > threshold))
})

test_that("ch_get_peaks event maxima and dates are consistent", {
  flows <- test_flows()
  events <- suppressMessages(ch_get_peaks(flows, 0.5 * max(flows$Flow)))$POTevents

  # The peak of each event must be on or after the event start
  expect_true(all(events$max_date >= events$st_date))
  # and the recorded maximum must be the flow observed on max_date
  for (i in seq_len(nrow(events))) {
    expect_equal(flows$Flow[flows$Date == events$max_date[i]], events$max[i])
  }
})

test_that("ch_get_peaks durations and volumes are positive", {
  flows <- test_flows()
  events <- suppressMessages(ch_get_peaks(flows, 0.5 * max(flows$Flow)))$POTevents

  expect_true(all(events$duration >= 1))
  expect_true(all(events$volume > 0))
})

test_that("ch_get_peaks finds more events as the threshold is lowered", {
  flows <- test_flows()

  high <- suppressMessages(ch_get_peaks(flows, 0.6 * max(flows$Flow)))
  low  <- suppressMessages(ch_get_peaks(flows, 0.2 * max(flows$Flow)))

  expect_gt(low$ncases, high$ncases)
})

test_that("ch_get_peaks returns nothing when the threshold is never exceeded", {
  flows <- test_flows()

  expect_message(
    result <- ch_get_peaks(flows, max(flows$Flow) * 2),
    "is greater than maximum observed flow"
  )
  expect_null(result)
})

test_that("ch_get_peaks tolerates missing flows", {
  flows <- test_flows()
  flows$Flow[c(50, 51, 1000)] <- NA

  peaks <- suppressMessages(ch_get_peaks(flows, 0.5 * max(flows$Flow, na.rm = TRUE)))

  expect_s3_class(peaks$POTevents, "data.frame")
  expect_gt(nrow(peaks$POTevents), 0)
})
