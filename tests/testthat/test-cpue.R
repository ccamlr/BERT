## tests for cpue-by-seabed area biomass estimates

# load packages for writing new tests
# library(BERT)
# library(testthat)

test_that("test cpue_bio inputs", {
  expect_warning(check_cpue_bio(-1,0,0,0,0),"CPUE, planimetric area and reference biomass must not be zero or negative")
})

