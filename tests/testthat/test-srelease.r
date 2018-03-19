## tests for single release tag recapture models

# load packages for writing new tests
# library(tagr)
# library(testthat)

## test check_srelease_inputs
test_that("test srelease inputs", {
   expect_warning(check_srelease_inputs(-1, 10, 1), "releases, catch and recaptures must not be negative")
})


## test estimates of population size
test_that("test srelease N_hat", {
  expect_that(chapman_n(tags=100, catch=100, recaps=10)[["N_hat"]], equals(926.3636, tolerance=1e-4)) 
  expect_that(chapman_wt(tags=100, catch=100, recaps=10, mean_wt=5)[["N_hat"]], equals(959.0909, tolerance=1e-4))
  expect_that(petersen(tags=100, catch=100, recaps=10)[["N_hat"]], equals(1000, tolerance=1e-4)) 
  })

## test variances of population size
test_that("test srelease var_N", {
  expect_that(chapman_n(tags=100, catch=100, recaps=10)[["var_N"]], equals(56906.4, tolerance=1e-4)) 
  expect_that(chapman_wt(tags=100, catch=100, recaps=10, mean_wt=5)[["var_N"]], equals(32866.74, tolerance=1e-4))
  expect_that(petersen(tags=100, catch=100, recaps=10)[["var_N"]], equals(90000, tolerance=1e-4)) 
})
