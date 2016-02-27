context("4.Testing legacy @attr_weight with new @getAttrWeights with 'deprecated_FUN'")



test_that("results are numerically the same", {
  #' ToDo make result from legacy functions into a list to make comparable or extract from list
  #' with for loop or easiest way.

  # laptop # camera2_config <- read.csv("C:/Users/diego/Google Drive/KIT/Seminar_eC_HCI/R/camera2_config.csv")
  # CIP Pool #
  camera2_config <- read.csv("U:/Development/BA_files/camera2_config.csv")
  all.users <- getAllUserIds(camera2_config)
  all.attr <- get_attrs_ID(camera2_config)

  randomRounds <- sample(c("first","last","all"), 1)
  amountUsers <- sample(1:length(all.users), 1)
  randomUsers <- sample(all.users, amountUsers)

  amount.eq.WeightAndAttr <- sample(1:length(all.attr), 1)
  randomAttr <- sample(all.attr, amount.eq.WeightAndAttr) # sort? possible error?
  randomWeightVector <- runif(amount.eq.WeightAndAttr, 0.0 , 500.0)
  print(amount.eq.WeightAndAttr)

  legacy.oPV <- unname(powerful_function(camera2_config, randomUsers, rounds=randomRounds, FUN=overall_pv))
  renewed.oPV <- unname(overallPV(camera2_config, randomUsers, rounds=randomRounds))

  #' Test overall Prospect Value without any weight parameters
  expect_identical(legacy.oPV, renewed.oPV)

  # deprecated_FUN : Expect same results with random weight parameters, with length(weightVector) == length(attr)
  legacy.sameLength <- unname(powerful_function(camera2_config, userid = randomUsers, rounds=randomRounds, FUN=overall_pv, attr = randomAttr, weight = randomWeightVector))
  new.sameLength <- unname(overallPV(camera2_config, userid = randomUsers, attr = randomAttr, rounds=randomRounds,  weight = randomWeightVector))

  expect_identical(legacy.sameLength, new.sameLength)
  print(randomUsers)
  print(randomAttr)
  print(randomWeightVector)
  print(head(legacy.sameLength))
  print(head(new.sameLength))


  # deprecated_FUN: Expect some errors, first controlled then random
    # Controlled
  expect_error(powerful_function(camera2_config, 9:11, FUN = overall_pv, weight=1:3))
  expect_error(overallPV(camera2_config, 9:11, weight=1:3))
    # Random with different length
  #expect_error(overall_pv(camera2_config, userid = randomUsers, attr = randomAttr,  weight = randomWeightVector))
  #expect_error(overallPV(camera2_config, userid = randomUsers, attr = randomAttr,  weight = randomWeightVector))

})
