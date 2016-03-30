

#Added new function
# DOCU: This nor any other vectorized function sort attributes!!!!!, that is the results are printed in the order they were given! Impacts
# tests, since they have to be sorted, such as before in powerful_function and other not vectorized fun.
overallPV <- function (dataset, userid = NULL, attr = NULL, rounds = NULL, refps = NULL, cost_ids = NULL,  weight = NULL, weightFUN = "differenceToIdeal",
                       alpha = 0.88, beta = 0.88, lambda = 2.25, gamma) {

  if(is.null(weight) & is.null(dataset)) {
    stop("Unable to get weights. You need to enter the weights or provide the dataset for us to calculate them.")
  }

  weight <- getAttrWeights(dataset, userid, weight,  attr, rounds, cost_ids, weightFUN, gamma)

  pvMatrices <- pvMatrix(dataset, userid, attr, rounds, refps, cost_ids, alpha, beta, lambda)

  tryCatchResult = tryCatch({
    overall_pv <- mapply(overall_pv_extend, pvMatrices, weight, SIMPLIFY = F) ##Perhaps mapply when data.frame, make weights as list?!

  }, warning = function(condition) {
    message("Probably amount of users differs from amount of weightVectors and they cannot be recycled.")
    message("Result most likely not accurrate, check your arguments.")
  }, error = function(condition) {
    stop("The input in the weight parameters can be flexible but check if you are entering the right amount of weightVectors, users and attributes")
  }, finally={
  })

  overall_pv
}

overallTRP <- function(dataset, userid = NULL, attr = NULL, rounds = NULL, refps = NULL, cost_ids = NULL,  weight = NULL, weightFUN = "differenceToIdeal",
                        tri.refps = NULL, beta_f = 5, beta_l = 1, beta_g = 1, beta_s = 3, gamma) {

  if(is.null(weight) & is.null(dataset)) {
    stop("Unable to get weights. You need to enter the weights or provide the dataset for us to calculate them.")
  }

  weight <- getAttrWeights(dataset, userid, weight,  attr, rounds, cost_ids, weightFUN, gamma)

  trpValueMatrices <- trp.valueMatrix(dataset, userid, attr, rounds, cost_ids, tri.refps, beta_f, beta_l, beta_g, beta_s)

  tryCatchResult = tryCatch({
    overall_pv <- mapply(overall_pv_extend, trpValueMatrices, weight, SIMPLIFY = F) ##Perhaps mapply when data.frame, make weights as list?!

  }, warning = function(condition) {
    message("Probably amount of users differs from amount of weightVectors and they cannot be recycled.")
    message("Result most likely not accurrate, check your arguments.")
  }, error = function(condition) {
    stop("The input in the weight parameters can be flexible but check if you are entering the right amount of weightVectors, users and attributes")
  }, finally={
  })

  overall_pv
}

overallDRP <- function(dataset, userid = NULL, attr = NULL, rounds = NULL, cost_ids = NULL,weight = NULL, weightFUN = "differenceToIdeal",
                        dual.refps = c(sq=NA, g=NA), lambda = 2.25, delta = 0.8, consumption_fun = NULL, gamma) {

  if(is.null(weight) & is.null(dataset)) {
    stop("Unable to get weights. You need to enter the weights or provide the dataset for us to calculate them.")
  }

  weight <- getAttrWeights(dataset, userid, weight,  attr, rounds, cost_ids, weightFUN, gamma)

  dualValueMatrices <- dual.valueMatrix(dataset, userid, attr, rounds, cost_ids, dual.refps,lambda, delta, consumption_fun)

  tryCatchResult = tryCatch({
    overall_pv <- mapply(overall_pv_extend, dualValueMatrices, weight, SIMPLIFY = F) ##Perhaps mapply when data.frame, make weights as list?!

  }, warning = function(condition) {
    message("Probably amount of users differs from amount of weightVectors and they cannot be recycled.")
    message("Result most likely not accurrate, check your arguments.")
  }, error = function(condition) {
    stop("The input in the weight parameters can be flexible but check if you are entering the right amount of weightVectors, users and attributes")
  }, finally={
  })

  overall_pv
}

#Still used in vectorialised
overall_pv_extend <- function(value_matrix, weight = NULL) {
  if (length(weight) != ncol(value_matrix)) {
    text <- paste0("weights: ", length(weight), " != ", ncol(value_matrix), " cols in valueMatrix.")
    stop("Amount of weights does not equal the amount of columns/attr: ", text, " Check your arguments")
  }
  else {
    result = tryCatch({
      result <- apply(value_matrix, 1, function(my_vector) { sum(my_vector*weight)})
    }, warning = function(condition) {
      message("Amount of weights does not equal the amount of columns/attr: ", text)
    }, error = function(condition) {
      message("An error is unlikely, possible wrong type input.")
    }, finally={
    })
  }

  result
}




