context("Getis-Ord stuff")

# Tests ==========

test_that("GetisOrd function used the standard version by default", {
  r <- raster(matrix(rnorm(400), 20))
  w <- weight_matrix_circular_fade(7, 2)
  st <- GetisOrdStandardStats(r)

  expect_equal( GetisOrd(r, w) , GetisOrd(r, w,st) )
})
