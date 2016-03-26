context("10.Equality for get_attr_weight and getAttrWeight(weightFUN='deprecated'")



test_that("results are equal, input and calculated weights", {
  # laptop #
  camera2_config <- read.csv("C:/Users/diego/Google Drive/KIT/Seminar_eC_HCI/R/camera2_config.csv")
  # CIP Pool #  camera2_config <- read.csv("U:/Development/BA_files/camera2_config.csv")
  all.users <- getAllUserIds(camera2_config)
  all.attr <- get_attrs_ID(camera2_config)

  amountUsers <- sample(1:length(all.users), 1)
  randomUsers <- sample(all.users, amountUsers)

  amount.eq.WeightAndAttr <- sample(1:length(all.attr), 1)
  randomAttr <- sample(all.attr, amount.eq.WeightAndAttr) # sort? possible error?
  random4 <- runif(4, 0.0 , 500.0)

  #' Test weight functions with input

})
