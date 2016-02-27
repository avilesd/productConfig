context("4.Testing legacy @attr_weight with new @getAttrWeights")



test_that("results are numerically the same", {
  #' ToDo make result from legacy functions into a list to make comparable or extract from list
  #' with for loop or easiest way.



  camera2_config <- read.csv("C:/Users/diego/Google Drive/KIT/Seminar_eC_HCI/R/camera2_config.csv")
  all.users <- getAllUserIds(camera2_config)
  randomRounds <- sample(c("first","last","all"), 1)
  amountUsers <- sample(1:length(all.users), 1)
  randomUsers <- sample(all.users, amountUsers)

  legacyPV <- unname(powerful_function(camera2_config, randomUsers, rounds=randomRounds, FUN=pvalue_matrix))
  renewedPV <- unname(pvMatrix(camera2_config, randomUsers, rounds=randomRounds))

  expect_identical(legacyPV, renewedPV)

})
