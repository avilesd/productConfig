context("6. Testing DRP gainLossFunction")



test_that("results are identical", {

  camera2_config <- read.csv("C:/Users/diego/Google Drive/KIT/Seminar_eC_HCI/R/camera2_config.csv")

  expect_identical(-0.516158, round(dualGainLossFunction(1,c(5,15)),6))
  # dualValueMatrix.oneAttr(myData, 9, attr=1, rounds="all",  dual.refps = c(1.5,-2.5))
  #aMatrix and dual.refps should be
  #attr1
  #0round   5.5
  #1round   5.5
  #2round   5.5
  #3round   5.5
  #$`9`
  #[1] 5 1

  #Test value matrix dM(myData, 11, attr=3, rounds="all", dual = c(1.5, 2.5))
  vm11_3 <- readRDS("C:/Users/diego/Google Drive/KIT/Seminar_eC_HCI/R/productConfig/data/vm11_3.rds")
  dm11_3 <- readRDS("C:/Users/diego/Google Drive/KIT/Seminar_eC_HCI/R/productConfig/data/dm11_3.rds")
  expectedVM <- dualValueMatrix.oneAttr(camera2_config, 11, attr=3, rounds="all", dual = c(1.5, 2.5))

  expect_identical(vm11_3, smallerThanZero(dm11_3[[1]], c(1.5,2.5), lambda=2.25, delta=0.8))
  expect_identical(vm11_3, expectedVM[[1]])

})
