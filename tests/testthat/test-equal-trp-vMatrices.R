context("5. Testing TRP results with presaved data")



test_that("results are identical", {
  # laptop #
  camera2_config <- read.csv("C:/Users/diego/Google Drive/KIT/Seminar_eC_HCI/R/camera2_config.csv")
  # CIP Pool #camera2_config <- read.csv("U:/Development/BA_files/camera2_config.csv")
  trp10.13 <- readRDS("C:/Users/diego/Google Drive/KIT/Seminar_eC_HCI/R/productConfig/data/trp10-13.rds")
  trp10.4 <- readRDS("C:/Users/diego/Google Drive/KIT/Seminar_eC_HCI/R/productConfig/data/trp10-4.rds")

  result13 <- trpValueMatrix(camera2_config, 10, attr=1:3, rounds="all")
  result4 <- trpValueMatrix(camera2_config, 10, attr=4, rounds="all", mr=0.40, sq=0.17, g=-0.10, cost_ids=4) #Input as with actual tri.refps

  expect_identical(trp10.13, result13)
  expect_identical(trp10.4, result4)

})
