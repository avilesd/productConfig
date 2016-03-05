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

  #gainLoss should - dualValueMatrix(myData, 10, attr=1:3, rounds="all",  dual.refps = c(1.5,2.5))
  #       attr1      attr2      attr3
  #0round 2.3845413 -2.9314944 -2.9314944
  #1round 0.9509677 -2.9314944 -2.9314944
  #2round 0.9509677  0.7409121 -2.9314944
  #3round 0.9509677 -0.8659935 -2.9314944
  #4round 0.9509677 -0.8659935 -0.8659935

  #normalized
  #$`10`
  #attr1      attr2      attr3
  #0round 1.0000000 -1.0000000 -1.0000000
  #1round 0.3988053 -1.0000000 -1.0000000
  #2round 0.3988053  0.2527421 -1.0000000
  #3round 0.3988053 -0.2954103 -1.0000000
  #4round 0.3988053 -0.2954103 -0.2954103

  #Test value matrix dM(myData, 11, attr=3, rounds="all", dual = c(1.5, 2.5))
  vm11_3 <- readRDS("C:/Users/diego/Google Drive/KIT/Seminar_eC_HCI/R/productConfig/data/vm11_3.rds")
  dm11_3 <- readRDS("C:/Users/diego/Google Drive/KIT/Seminar_eC_HCI/R/productConfig/data/dm11_3.rds")
  expectedVM <- dualValueMatrix.oneAttr(camera2_config, 11, attr=3, rounds="all", dual = c(1.5, 2.5))

  # No longer expecting identical, since this was made before normalizing
  #expect_identical(vm11_3, smallerThanZero(dm11_3[[1]], c(1.5,2.5), lambda=2.25, delta=0.8))
  #expect_identical(vm11_3, expectedVM[[1]])

  # Before normalizing dual : overallPV_interface(dual10.full, weight = c(0.25,.25,.25,.25))
  #$`10`
  #0round      1round      2round      3round      4round
  #-1.79124087 -2.32285703 -0.44540967 -1.13409607 -0.03942965

})
