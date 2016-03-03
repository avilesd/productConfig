context("1-Testing different forms of calc norm.matrices")



test_that("results are identical", {
  # laptop #
  camera2_config <- read.csv("C:/Users/diego/Google Drive/KIT/Seminar_eC_HCI/R/camera2_config.csv")
  # CIP Pool #  camera2_config <- read.csv("U:/Development/BA_files/camera2_config.csv")
  all.users <- getAllUserIds(camera2_config)
  norm.gainLoss <- norm.gainLoss(camera2_config, all.users, attr=c(1,3), rounds="all")
  norm.gainLoss.sep <- norm.gainLoss.sep(camera2_config, all.users, attr=c(1,3), rounds="all")

  expect_identical(norm.gainLoss, norm.gainLoss.sep)

})
