context("2.Testing overall_pv against vectorized overallPV")



test_that("results are identical, with static weight", {
  # laptop #
  camera2_config <- read.csv("C:/Users/diego/Google Drive/KIT/Seminar_eC_HCI/R/camera2_config.csv")
  # CIP Pool #camera2_config <- read.csv("U:/Development/BA_files/camera2_config.csv")
  all.users <- getAllUserIds(camera2_config)
  randomRounds <- sample(c("first","last","all"), 1)
  amountUsers <- sample(1:length(all.users), 1)
  randomUsers <- sample(all.users, amountUsers)

  overall_new <- unname(overallPV(camera2_config, randomUsers, rounds=randomRounds, weight = c(0.25,0.25,0.25,0.25)))
  overall_legacy <- unname(powerful_function(camera2_config, randomUsers, FUN=overall_pv, rounds=randomRounds ,weight=c(0.25,0.25,0.25,0.25)))

  #fulle.dual.refps <- c(1.50,  2.5 ,1.50,  2.5 ,1.50,  2.5 ,0.17, -0.1)
  #highestDRP <- getHighestRound(overallDRP(myData, 11, attr=1:4, rounds="all", cost_ids = 4, dual.refps=full.dual.refps))
  #highestPV <- getHighestRound(overallPV(myData, all.users, attr=1:4, rounds="all", cost_ids = 4, refps = c(1.5,1.5,1.5,0.17)))

  expect_identical(overall_new, overall_legacy)

  #compare(highestDRP, highestPV)

})
