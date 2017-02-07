context("Weight matrix")

# Tests ==========

test_that("Circular matrix uses only TRUE and NA values", {
  w <- weight_matrix_circular(11)
  expect_equal(unique(c(w)), c(NA, TRUE))
})

test_that("Circular 'fade' matrix does not contain NA", {
  w <- weight_matrix_circular_fade(11, 2)
  expect_equal(w[is.na(w)], numeric(0))
})

