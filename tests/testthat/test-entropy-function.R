context("0.2 Testing Entropy Function")



test_that("results are numerically equal", {

  camera2_config <- read.csv("C:/Users/diego/Google Drive/KIT/Seminar_eC_HCI/R/camera2_config.csv")

  entropy_matrix <- readRDS("C:/Users/diego/Google Drive/KIT/Seminar_eC_HCI/R/productConfig/data/entropy_matrix1.rds")
  testVector <- c(0.06410176, 0.20608266, 0.01347882, 0.08125553, 0.35867045, 0.27641078)
  expect_equal(entropy_matrix, testVector)


})
