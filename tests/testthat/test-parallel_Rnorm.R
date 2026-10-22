library(testthat)
library(NormGen)

context("Testing parallel_Rnorm function")

test_that("parallel_Rnorm structure and output types are correct", {
  res <- parallel_Rnorm(1000, 0, 1, seed = 2026)
  expect_length(res, 1000)
  expect_type(res, "double")
})

test_that("Explicit seed parameter guarantees strict reproducibility", {
  # Strict bit-by-bit identity check for parallel generation execution
  sample1 <- parallel_Rnorm(5000, 0, 1, seed = 2026)
  sample2 <- parallel_Rnorm(5000, 0, 1, seed = 2026)
  expect_identical(sample1, sample2)
})

test_that("Parallel generation output matches target distribution properties", {
  # Validate that the parallel generator outputs comply with standard normal targets at a loose tolerance
  res_parallel <- parallel_Rnorm(10000, 0, 1, seed = 2026)

  expect_equal(mean(res_parallel), 0, tolerance = 5e-2)
  expect_equal(sd(res_parallel), 1, tolerance = 5e-2)
})

test_that("An invalid number of parameters for parallel version", {
  expect_error(parallel_Rnorm())
})
