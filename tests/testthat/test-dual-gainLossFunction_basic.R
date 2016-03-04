context("6. Testing DRP gainLossFunction")



test_that("results are identical", {

  expect_identical(-0.516158, round(dualGainLossFunction(1,c(5,15)),6))

})
