test_that("ch_read_ECDE_flows reads an ECDE file into the documented shape", {
  flows <- ch_read_ECDE_flows(fixture_path("ecde_flows_sample.csv"))

  expect_s3_class(flows, "data.frame")
  expect_named(flows, c("ID", "PARAM", "Date", "Flow", "SYM"))
  expect_s3_class(flows$Date, "Date")
  expect_type(flows$Flow, "double")
  expect_gt(nrow(flows), 0)
})

test_that("ch_read_ECDE_flows drops the three trailing disclaimer lines", {
  path <- fixture_path("ecde_flows_sample.csv")
  # one header line + n data rows + three disclaimer lines
  n_lines <- length(readLines(path))
  flows <- ch_read_ECDE_flows(path)

  expect_equal(nrow(flows), n_lines - 1 - 3)
  # The last retained row must be a real observation, not disclaimer text
  expect_false(is.na(flows$Date[nrow(flows)]))
  expect_false(is.na(flows$Flow[nrow(flows)]))
})

test_that("ch_read_ECDE_flows converts the yyyy/mm/dd date strings", {
  flows <- ch_read_ECDE_flows(fixture_path("ecde_flows_sample.csv"))

  expect_equal(min(flows$Date), as.Date("2000-01-01"))
  expect_equal(max(flows$Date), as.Date("2000-12-31"))
  expect_false(anyNA(flows$Date))
})

test_that("ch_read_ECDE_flows rejects a missing or unspecified file", {
  expect_error(ch_read_ECDE_flows(""), "not specified")
  expect_error(
    ch_read_ECDE_flows(file.path(tempdir(), "definitely-not-here.csv")),
    "not found"
  )
})
