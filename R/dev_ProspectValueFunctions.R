
overall_pv <- function (dataset, userid = NULL, attr = NULL, rounds = NULL, refps = NULL, cost_ids = NULL,  weight = NULL,
                        alpha = 0.88, beta = 0.88, lambda = 2.25) {
  if(is.null(weight) & is.null(dataset)) {
    stop("Unable to get weights. You need to enter the weights or provide the dataset for us to calculate them. ")
  }
  if(is.null(weight) & !is.null(dataset)) {
    weight <-get_attr_weight(dataset, userid, weight,  attr, rounds, cost_ids)
  }

  v_matrix <- pvalue_matrix(dataset, userid, attr, rounds, refps, cost_ids, alpha, beta, lambda)

  overall_pv <- overall_pv_extend(v_matrix, weight)

  overall_pv

}

#Added new function
# DOCU: This nor any other vectorized function sort attributes!!!!!, that is the results are printed in the order they were given! Impacts
# tests, since they have to be sorted, such as before in powerful_function and other not vectorized fun.
overallPV <- function (dataset, userid = NULL, attr = NULL, rounds = NULL, refps = NULL, cost_ids = NULL,  weight = NULL, weightFUN = "deprecated_FUN",
                       alpha = 0.88, beta = 0.88, lambda = 2.25) {

  if(is.null(weight) & is.null(dataset)) {
    stop("Unable to get weights. You need to enter the weights or provide the dataset for us to calculate them.")
  }

  weight <- getAttrWeights(dataset, userid, weight,  attr, rounds, cost_ids, weightFUN)

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

###########

pvalue_matrix <- function(dataset, userid = NULL, attr = NULL, rounds = NULL, refps = NULL, cost_ids = NULL,
                          alpha = 0.88, beta = 0.88, lambda = 2.25) {
  ngain <- norm_g_l_matrices(dataset, userid, attr, rounds, refps, cost_ids)$ngain
  nloss <- norm_g_l_matrices(dataset, userid, attr, rounds, refps, cost_ids)$nloss

  v_matrix <- prospect_value_matrix_extend(ngain, nloss, alpha, beta, lambda)
  v_matrix

}

#########
# Do not delete this function in vectorialize, could still be useful
prospect_value_matrix_extend <- function(ngain = NULL, nloss = NULL, alpha = 0.88, beta = 0.88, lambda = 2.25)  {
  if((is.null(ngain) || is.null(nloss)) ) {
    stop("You need to provide both normalized gain and loss matrices. Helpful functions: norm_g_l_matrices, gain_matrix,
         loss_matrix, gain_loss_matrices")
  }
  if(!identical(dim(ngain), dim(nloss))) {
    stop("The input matrices do not have equal dimensions.")
  }
  else {
    value_matrix <- matrix(NA, nrow(ngain), ncol(ngain))

    for(n in 1:nrow(ngain)) {
      value_matrix[n, ] <- mapply(pvalue_fun, ngain[n, ], nloss[n, ], alpha, beta, lambda)
    }
  }
  value_matrix
}

#Added function pvMatrix_extend to input ngain and nloss was deprecated, but still accessible.
pvMatrix <- function(dataset, userid = NULL, attr = NULL, rounds = NULL, refps = NULL, cost_ids = NULL,
                     alpha = 0.88, beta = 0.88, lambda = 2.25) {
  normalizedgainLoss <- norm.gainLoss(dataset, userid, attr, rounds, refps, cost_ids, binded = F)
  pvMatrixList <- with(normalizedgainLoss, mapply(pvalue_fun, gain, loss, alpha, beta, lambda, SIMPLIFY = F))
  pvMatrixList
}


##Auxiliary function, not necessary to document right now.
pvalue_fun <- function(ngain_ij, nloss_ij, alpha = 0.88, beta = 0.88, lambda = 2.25) {
  result <- ((ngain_ij)^alpha) + (-lambda*((-nloss_ij)^beta))
  result

}

######

overall_pv_extend <- function(value_matrix, weight = NULL) {
  if (length(weight) != ncol(value_matrix)) {
    text <- paste0("weights: ", length(weight), " != ", ncol(value_matrix), " rows in valueMatrix.")
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




