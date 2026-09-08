test_that("ch_get_wscstation returns one row with the added long name", {
  info <- ch_get_wscstation("05BB001", metadata = HYDAT_list)

  expect_s3_class(info, "data.frame")
  expect_equal(nrow(info), 1)
  # A 21st column holding the assembled title is appended
  expect_equal(ncol(info), 21)
  expect_equal(names(info)[21], "Station_lname")
})

test_that("ch_get_wscstation builds the long name from the station fields", {
  info <- ch_get_wscstation("05BB001", metadata = HYDAT_list)

  expect_type(info$Station_lname, "character")
  expect_true(grepl("05BB001", info$Station_lname, fixed = TRUE))
  expect_true(grepl(info$StationName, info$Station_lname, fixed = TRUE))
  expect_true(grepl(info$Prov, info$Station_lname, fixed = TRUE))
})

test_that("ch_get_wscstation marks RHBN stations with an asterisk", {
  rhbn <- HYDAT_list[!is.na(HYDAT_list$RHBN) & HYDAT_list$RHBN == TRUE, ]
  skip_if(nrow(rhbn) == 0, "no RHBN stations in HYDAT_list")

  info <- ch_get_wscstation(rhbn$Station[1], metadata = HYDAT_list)
  expect_true(endsWith(info$Station_lname, "*"))
})

test_that("ch_get_wscstation does not mark non-RHBN stations", {
  other <- HYDAT_list[!is.na(HYDAT_list$RHBN) & HYDAT_list$RHBN == FALSE, ]
  skip_if(nrow(other) == 0, "no non-RHBN stations in HYDAT_list")

  info <- ch_get_wscstation(other$Station[1], metadata = HYDAT_list)
  expect_false(endsWith(info$Station_lname, "*"))
})

test_that("ch_get_wscstation defaults to the bundled HYDAT_list", {
  expect_equal(
    ch_get_wscstation("05BB001"),
    ch_get_wscstation("05BB001", metadata = HYDAT_list)
  )
})

test_that("ch_get_wscstation reports an unknown station", {
  expect_message(
    result <- ch_get_wscstation("99ZZ999", metadata = HYDAT_list),
    "not found"
  )
  # documented fallback: the station id is returned unchanged
  expect_equal(result, "99ZZ999")
})

test_that("ch_get_wscstation accepts a caller-supplied metadata frame", {
  custom <- HYDAT_list[HYDAT_list$Station == "05BB001", ]
  custom$StationName <- "A RENAMED RIVER"

  info <- ch_get_wscstation("05BB001", metadata = custom)

  expect_true(grepl("A RENAMED RIVER", info$Station_lname, fixed = TRUE))
})
