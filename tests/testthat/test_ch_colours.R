# Tests for the colour helpers: ch_col_gradient, ch_color_gradient,
# ch_col_transparent and ch_circular_colors.
#
# Note that ch_col_gradient and ch_color_gradient are near-identical: they
# differ only in their default palette (red-white-blue vs red-white-green).

is_hex_colour <- function(x) grepl("^#[0-9A-Fa-f]{6}$", x)
is_hex_colour_alpha <- function(x) grepl("^#[0-9A-Fa-f]{8}$", x)

test_that("ch_col_gradient returns one colour per input value", {
  x <- c(0, 1, 2, 3, 4, 5, 10)

  out <- ch_col_gradient(x)

  expect_type(out, "character")
  expect_length(out, length(x))
  expect_true(all(is_hex_colour(out)))
})

test_that("ch_col_gradient maps the extremes to the ends of the palette", {
  out <- ch_col_gradient(c(0, 5, 10), colors = c("red", "white", "blue"))

  expect_equal(tolower(out[1]), "#ff0000")
  expect_equal(tolower(out[3]), "#0000ff")
})

test_that("ch_col_gradient honours common limits", {
  # With climits set, the same value must get the same colour regardless of the
  # rest of the vector, which is the point of the argument.
  a <- ch_col_gradient(c(0, 5), climits = c(0, 10))
  b <- ch_col_gradient(c(0, 5, 10), climits = c(0, 10))

  expect_equal(a[1], b[1])
  expect_equal(a[2], b[2])
})

test_that("ch_col_gradient respects colsteps", {
  expect_length(ch_col_gradient(1:5, colsteps = 20), 5)
  # More steps means more distinct colours are available
  fine <- ch_col_gradient(seq(0, 1, length.out = 50), colsteps = 50)
  coarse <- ch_col_gradient(seq(0, 1, length.out = 50), colsteps = 3)
  expect_gt(length(unique(fine)), length(unique(coarse)))
})

test_that("ch_col_gradient rejects a NULL x", {
  expect_error(ch_col_gradient(NULL), "x is NULL")
})


test_that("ch_color_gradient returns one colour per input value", {
  x <- c(0, 1, 1, 3, 4, 5, 10)

  out <- ch_color_gradient(x)

  expect_type(out, "character")
  expect_length(out, length(x))
  expect_true(all(is_hex_colour(out)))
})

test_that("ch_color_gradient reproduces its documented example", {
  expect_equal(
    ch_color_gradient(c(0, 1, 1, 3, 4, 5, 10)),
    c("#8B0000", "#B50000", "#B50000", "#FF2B2B", "#FF9292", "#FFF9F9", "#006400")
  )
})

test_that("ch_color_gradient honours common limits and rejects NULL", {
  a <- ch_color_gradient(c(0, 5), climits = c(0, 10))
  b <- ch_color_gradient(c(0, 5, 10), climits = c(0, 10))
  expect_equal(a, b[1:2])

  expect_error(ch_color_gradient(NULL), "x is NULL")
})

test_that("ch_col_gradient and ch_color_gradient differ only in default palette", {
  x <- c(0, 2, 4, 6, 8, 10)
  shared <- c("darkred", "red", "white", "blue", "darkblue")

  expect_equal(ch_col_gradient(x, colors = shared),
               ch_color_gradient(x, colors = shared))
})


test_that("ch_col_transparent appends an alpha channel", {
  out <- ch_col_transparent("blue", 100)

  expect_type(out, "character")
  expect_length(out, 1)
  expect_true(is_hex_colour_alpha(out))
  # blue is 0000FF, and 100 decimal is 64 hex
  expect_equal(toupper(out), "#0000FF64")
})

test_that("ch_col_transparent maps the alpha endpoints", {
  expect_equal(toupper(ch_col_transparent("red", 0)), "#FF000000")
  expect_equal(toupper(ch_col_transparent("red", 255)), "#FF0000FF")
})

test_that("ch_col_transparent vectorises over colours and transparencies", {
  out <- ch_col_transparent(c("green", "red"), c(100, 200))

  expect_length(out, 2)
  expect_true(all(is_hex_colour_alpha(out)))
})

test_that("ch_col_transparent recycles a length-one argument", {
  many_trans <- ch_col_transparent("blue", c(50, 100, 150))
  expect_length(many_trans, 3)

  many_cols <- ch_col_transparent(c("blue", "red", "green"), 100)
  expect_length(many_cols, 3)
})

test_that("ch_col_transparent rejects mismatched lengths", {
  expect_error(
    ch_col_transparent(c("blue", "red"), c(1, 2, 3)),
    "Vector lengths not correct"
  )
})


test_that("ch_circular_colors returns n distinct colours", {
  out <- ch_circular_colors(n = 10)

  expect_type(out, "character")
  expect_length(out, 10)
  # The whole point of this version is that the first and last differ, unlike
  # the original in the circular package
  expect_false(out[1] == out[10])
  expect_equal(length(unique(out)), 10)
})

test_that("ch_circular_colors returns colours with an alpha channel", {
  out <- ch_circular_colors(n = 12)

  expect_true(all(is_hex_colour_alpha(out)))
  # default alpha is 1.0, i.e. fully opaque
  expect_true(all(toupper(substr(out, 8, 9)) == "FF"))
})

test_that("ch_circular_colors respects alpha", {
  out <- ch_circular_colors(n = 4, alpha = 0.5)

  expect_length(out, 4)
  expect_false(any(toupper(substr(out, 8, 9)) == "FF"))
})

test_that("ch_circular_colors works for a range of sizes", {
  for (n in c(1, 2, 3, 12, 24)) {
    expect_length(ch_circular_colors(n = n), n)
  }
})
