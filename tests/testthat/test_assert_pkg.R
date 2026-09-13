context("Testing assert_pkg")

test_that("assert_pkg errors when the package is not installed", {
  expect_error(
    CSHShydRology:::assert_pkg("definitely_not_a_package"),
    "definitely_not_a_package"
  )
})

test_that("assert_pkg error message tells the user how to install", {
  expect_error(
    CSHShydRology:::assert_pkg("definitely_not_a_package"),
    "install.packages",
    fixed = TRUE
  )
})

test_that("assert_pkg is silent when the package is installed", {
  expect_silent(CSHShydRology:::assert_pkg("stats"))
  expect_null(CSHShydRology:::assert_pkg("stats"))
})
