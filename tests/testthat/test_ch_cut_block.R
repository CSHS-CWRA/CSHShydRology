test_that("ch_cut_block extracts the requested period", {
  flows <- test_flows()

  sub <- suppressMessages(ch_cut_block(flows, "1992/01/01", "1994/12/31"))

  expect_s3_class(sub, "data.frame")
  expect_named(sub, names(flows))
  expect_equal(min(sub$Date), as.Date("1992-01-01"))
  expect_equal(max(sub$Date), as.Date("1994-12-31"))
})

test_that("ch_cut_block accepts both slash and dash date formats", {
  flows <- test_flows()

  slash <- suppressMessages(ch_cut_block(flows, "1992/01/01", "1992/12/31"))
  dash  <- suppressMessages(ch_cut_block(flows, "1992-01-01", "1992-12-31"))

  expect_equal(slash, dash)
})

test_that("ch_cut_block reports how many records were selected", {
  flows <- test_flows()

  expect_message(
    ch_cut_block(flows, "1992/01/01", "1992/01/31"),
    "31 records were selected"
  )
})

test_that("ch_cut_block refuses an unrecognised date format", {
  flows <- test_flows()

  expect_message(
    result <- ch_cut_block(flows, "1992.01.01", "1992.12.31"),
    "incorrect date format"
  )
  expect_null(result)
})

test_that("ch_cut_block refuses a start date before the record begins", {
  flows <- test_flows()

  expect_message(
    result <- ch_cut_block(flows, "1950/01/01", "1995/12/31"),
    "before records are available"
  )
  expect_null(result)
})

test_that("ch_cut_block warns but still returns when the end date is late", {
  flows <- test_flows()

  expect_message(
    sub <- ch_cut_block(flows, "1998/01/01", "2050/12/31"),
    "after records are available"
  )
  expect_s3_class(sub, "data.frame")
  # Truncated to what is actually available
  expect_equal(max(sub$Date), max(flows$Date))
})
