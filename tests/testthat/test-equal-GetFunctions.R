context("0.5.Testing GetFunctions for basic functionality")



test_that("results are identical", {
  # laptop #
  camera2_config <- read.csv("C:/Users/diego/Google Drive/KIT/Seminar_eC_HCI/R/camera2_config.csv")
  # CIP Pool #camera2_config <- read.csv("U:/Development/BA_files/camera2_config.csv")
  all.users <- get_all_userids(camera2_config)
  randomUser <- sample(all.users, 1)
  randomAttr <- sample(get_attrs_ID(camera2_config), 1)

  expect_identical(get_table_by_ID(camera2_config, randomUser), getTableById(camera2_config, randomUser)[[1]])
  expect_identical(get_rounds_by_ID(camera2_config, randomUser), getRoundsById(camera2_config, randomUser)[[1]])
  expect_identical(get_all_userids(camera2_config), getAllUserIds(camera2_config))
  expect_identical(unname(get_all_default_rps(camera2_config, randomUser)), unname(getDefaultRefps(camera2_config, randomUser)[[1]]))
  expect_identical(get_attr_values(camera2_config, randomAttr), getAttrValues(camera2_config, randomAttr)[[1]])
})
