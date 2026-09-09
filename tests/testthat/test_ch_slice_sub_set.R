# Tests for ch_slice and ch_sub_set_Years.

test_that("ch_slice returns a factor with one level per bin", {
  doy <- 1:365

  bins <- suppressMessages(ch_slice(doy, 5))

  expect_s3_class(bins, "factor")
  expect_length(bins, 365)
  # floor(366 / 5) = 73 bins
  expect_equal(nlevels(bins), 73)
  expect_equal(levels(bins), as.character(1:73))
})

test_that("ch_slice puts the first `step` days in bin 1", {
  bins <- suppressMessages(ch_slice(1:365, 30))

  expect_equal(as.character(bins[1:30]), rep("1", 30))
  expect_equal(as.character(bins[31:60]), rep("2", 30))
})

test_that("ch_slice never exceeds the bin count, even on day 366", {
  bins <- suppressMessages(ch_slice(1:366, 5))

  expect_false(anyNA(bins))
  expect_lte(max(as.numeric(as.character(bins))), 73)
})

test_that("ch_slice reports how partial bins were handled", {
  expect_message(ch_slice(1:365, 7), "Bins = 52")
})

test_that("ch_slice bin widths are equal apart from the last", {
  bins <- suppressMessages(ch_slice(1:365, 5))
  counts <- as.vector(table(bins))

  expect_true(all(counts[-length(counts)] == 5))
})


test_that("ch_sub_set_Years returns matching positions and labels", {
  years <- 1900:2045

  out <- ch_sub_set_Years(years, 20)

  expect_type(out, "list")
  expect_named(out, c("position", "label"))
  expect_equal(length(out$position), length(out$label))
  # Positions index into the input, and the labels are the values there
  expect_equal(years[out$position], out$label)
})

test_that("ch_sub_set_Years samples every nth element", {
  out <- ch_sub_set_Years(1:100, 10)

  expect_equal(out$position, seq(10, 100, by = 10))
  expect_equal(out$label, seq(10, 100, by = 10))
})

test_that("ch_sub_set_Years works on any simple vector", {
  out <- ch_sub_set_Years(LETTERS, 5)

  expect_equal(out$label, LETTERS[c(5, 10, 15, 20, 25)])
  expect_type(out$label, "character")
})
