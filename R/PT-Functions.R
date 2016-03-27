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

# Still used in vectorialised. Auxiliary function, not necessary to document right now.
pvalue_fun <- function(ngain_ij, nloss_ij, alpha = 0.88, beta = 0.88, lambda = 2.25) {
  result <- ((ngain_ij)^alpha) + (-lambda*((-nloss_ij)^beta))
  result

}

#' Gain matrix
#'
#' Creates the Gains matrix parting from a decision matrix and a vector
#' containing the reference points (aspiration-levels). We based our
#' calculations for gains and losses from a scientific paper, please see source
#' and references.
#'
#' @param data data.frame with the user generated data from a product
#'   configurator. Please see \code{decision_matrix} for specifications of the
#'   data.frame.
#'
#' @param userid an integer that gives the information of which user the matrix
#'   should be calculated.
#'
#' @param attr attributes IDs, vector of integer numbers corresponding to the
#'   attributes you desire to use; attr are assumed to be 1-indexed.
#'
#' @param rounds integer vector. Which steps of the configuration process should
#'   be shown? See Details.
#'
#' @param refps numeric vector. Reference Points: each point corresponds to one
#'   attribute, i.e. each attribute has only one aspiration level. Default
#'   setting assumes the aspiration levels as the default values of the initial
#'   product configuration for each user.
#'
#' @param cost_ids argument used to convert selected cost attributes into
#'   benefit attributes. Integer vector.
#'
#' @details General: The gain_matrix with ncol = number of attributes you
#' selected or all(default) and nrow= number of rounds you selected or the first
#' and last(default) for a selected user.
#'
#' \code{data} We assume the input data.frame has following columns usid = User
#' IDs, round = integers indicating which round the user is in (0-index works
#' best for 'round'), atid = integer column for referring the attribute ID (1
#' indexed), selected = numeric value of the attribute for a specific, given
#' round, selectable = amount of options the user can chose at a given round,
#' with the current configuration. This is a necessary parameter.
#'
#' \code{userid} is a necessary parameter, without it you'll get a warning.
#' Default is NULL.
#'
#' \code{attr} Default calculates with all attributes. Attributes are
#' automatically read from provided table, it is important you always provide
#' the complete dataset so that the package functions properly. Moreover the
#' attribute will be sorted in ascending order, i.e. if you input attr=
#' c(1,3,2), the decision matrix resulting will display the columns in order:
#' attr1 attr2 attr3.
#'
#' \code{rounds} Default calculates first round(initia product config) and last
#' round of the session. Default calculates with first and last attributes
#' (initial and final product configuration). To choose all give "all" as
#' argument for rounds, see example. "first" and "last" are also possible
#' argument values. You can give a vector of arbitrarily chosen rounds as well.
#'
#' \code{refps} If you only want to see the results for one attribute you may
#' enter only a couple of reference points but you have to tell the function
#' which attributes you want to use those referene points for. So the amount of
#' attr and of refps should be the same. Moreover the functions always orders de
#' attr, so be sure to input the reference point also in an ascending order
#' corresponding to their attributes. (refps will not be ordered)
#'
#' \code{cost_ids} Default assumes all your attributes are of benefit type, that
#' is a higher value in the attribute means the user is better of than with a
#' lower value. If one or more of the attributes in your data is of cost type,
#' e.g. price, so that lower is better then you should identify this attributes
#' as such, providing their id, they'll be converted to benefit type (higher
#' amount is better).
#'
#' This function is for one user only, for more or all users see
#' \code{\link{powerful_function}}
#'
#' @return a gain matrix for a specific user.
#' @examples
#' gain_matrix(pc_config_data, 11)
#' gain_matrix(my_data, userid = 11, attr = c(1,3,5))
#' gain_matrix(keyboard_data, 60, rounds = "all", refps = c(1,3,4,0), cost_ids = 4)
#' gain_matrix(data1, 2, rounds = "last", attr = 1)
#' @export

gainMatrix <- function(dataset, userid = NULL, attr = NULL, rounds = NULL, refps = NULL, cost_ids = NULL) {
  desList <- decisionMatrix(dataset, userid, attr, rounds, cost_ids)
  refPs <- referencePoints(dataset, userid, refps, attr, cost_ids)

  tMatrixList <- lapply(desList, t)

  result <- mapply(gainFunction, tMatrixList, refPs, SIMPLIFY = F)

  #desList <- lapply(result, dim, nrow = rounds, ncol= length(attr), byrow = T)
  res <- mapply(function(tempData7, tempData8) {dim(tempData7) <- c(ncol(tempData8), nrow(tempData8)); tempData7}, result, desList, SIMPLIFY = F)
  finalRes <- lapply(res, t)
  finalRes
}

#' Loss matrix
#'
#' Creates the Loss matrix parting from a decision matrix and a vector
#' containing the reference points (aspiration-levels). We based our
#' calculations for gains and losses from a scientific paper, please see source
#' and references.
#'
#' @param data data.frame with the user generated data from a product
#'   configurator. Please see \code{decision_matrix} for specifications of the
#'   data.frame.
#'
#' @param userid an integer that gives the information of which user the matrix
#'   should be calculated.
#'
#' @param attr attributes IDs, vector of integer numbers corresponding to the
#'   attributes you desire to use; attr are assumed to be 1-indexed.
#'
#' @param rounds integer vector. Which steps of the configuration process should
#'   be shown? See Details.
#'
#' @param refps numeric vector. Reference Points: each point corresponds to one
#'   attribute, i.e. each attribute has only one aspiration level. Default
#'   setting assumes the aspiration levels as the default values of the initial
#'   product configuration for each user.
#'
#' @param cost_ids argument used to convert selected cost attributes into
#'   benefit attributes. Integer vector.
#'
#' @details General: The loss_matrix with ncol = number of attributes you
#' selected or all(default) and nrow= number of rounds you selected or the first
#' and last(default) for a selected user.
#'
#' \code{data} We assume the input data.frame has following columns usid = User
#' IDs, round = integers indicating which round the user is in (0-index works
#' best for 'round'), atid = integer column for referring the attribute ID (1
#' indexed), selected = numeric value of the attribute for a specific, given
#' round, selectable = amount of options the user can chose at a given round,
#' with the current configuration. This is a necessary parameter.
#'
#' \code{userid} is a necessary parameter, without it you'll get a warning.
#' Default is NULL.
#'
#' \code{attr} Default calculates with all attributes. Attributes are
#' automatically read from provided table, it is important you always provide
#' the complete dataset so that the package functions properly. Moreover the
#' attribute will be sorted in ascending order, i.e. if you input attr=
#' c(1,3,2), the decision matrix resulting will display the columns in order:
#' attr1 attr2 attr3.
#'
#' \code{rounds} Default calculates first round(initia product config) and last
#' round of the session. Default calculates with first and last attributes
#' (initial and final product configuration). To choose all give "all" as
#' argument for rounds, see example. "first" and "last" are also possible
#' argument values. You can give a vector of arbitrarily chosen rounds as well.
#'
#' \code{refps} If you only want to see the results for one attribute you may
#' enter only a couple of reference points but you have to tell the function
#' which attributes you want to use those referene points for. So the amount of
#' attr and of refps should be the same. Moreover the functions always orders de
#' attr, so be sure to input the reference point also in an ascending order
#' corresponding to their attributes. (refps will not be ordered)
#'
#' \code{cost_ids} Default assumes all your attributes are of benefit type, that
#' is a higher value in the attribute means the user is better of than with a
#' lower value. If one or more of the attributes in your data is of cost type,
#' e.g. price, so that lower is better then you should identify this attributes
#' as such, providing their id, they'll be converted to benefit type (higher
#' amount is better).
#'
#' This function is for one user only, for more or all users see
#' \code{\link{powerful_function}}
#'
#' @return a loss matrix for a specific user.
#' @examples
#' loss_matrix(pc_config_data, 11)
#' loss_matrix(my_data, userid = 11, attr = c(1,3,5))
#' loss_matrix(keyboard_data, 60, rounds = "all", refps = c(1,3,4,0), cost_ids = 4)
#' loss_matrix(data1, 2, rounds = "last", attr = 1)
#' @export

lossMatrix <- function(dataset, userid = NULL, attr = NULL, rounds = NULL, refps = NULL, cost_ids = NULL) {
  desList <- decisionMatrix(dataset, userid, attr, rounds, cost_ids)
  refPs <- referencePoints(dataset, userid, refps, attr, cost_ids)

  tMatrixList <- lapply(desList, t)

  result <- mapply(lossFunction, tMatrixList, refPs, SIMPLIFY = F)

  #This should be a standalone function
  res <- mapply(function(tempData7, tempData8) {dim(tempData7) <- c(ncol(tempData8), nrow(tempData8)); tempData7}, result, desList, SIMPLIFY = F)
  finalRes <- lapply(res, t)
  finalRes
}

#' Merges gain and loss matrices
#'
#' Returns a list with two elements one is the $gain matrix and the second one
#' is the $loss matrix. The user can change the \code{result_type} to "cbind" or
#' "rbind", both cases resulting in a merged matrix, which is easier to work
#' with than a list. For a specificied \code{userid}.
#'
#' @param data data.frame with the user generated data from a product
#'   configurator. Please see \code{decision_matrix} for specifications of the
#'   data.frame.
#'
#' @param userid an integer that gives the information of which user the matrix
#'   should be calculated.
#'
#' @param attr attributes IDs, vector of integer numbers corresponding to the
#'   attributes you desire to use; attr are assumed to be 1-indexed.
#'
#' @param rounds integer vector. Which steps of the configuration process should
#'   be shown? See Details.
#'
#' @param refps numeric vector. Reference Points: each point corresponds to one
#'   attribute, i.e. each attribute has only one aspiration level. Default
#'   setting assumes the aspiration levels as the default values of the initial
#'   product configuration for each user.
#'
#' @param result_type allows to change the result type. Default returns a list
#'   with two elements. Other possibilites are "cbind" and "rbind" as character
#'   input which do exactly what their name suggests; return a column- or row-
#'   binded matrix.
#'
#' @param cost_ids argument used to convert selected cost attributes into
#'   benefit attributes. Integer vector.
#'
#' @details \code{data} We assume the input data.frame has following columns
#' usid = User IDs, round = integers indicating which round the user is in
#' (0-index works best for 'round'), atid = integer column for referring the
#' attribute ID (1 indexed), selected = numeric value of the attribute for a
#' specific, given round, selectable = amount of options the user can chose at a
#' given round, with the current configuration. This is a necessary parameter.
#'
#' \code{userid} is a necessary parameter, without it you'll get a warning.
#'
#' \code{attr} Default calculates with all attributes. Attributes are
#' automatically read from provided table, it is important you always provide
#' the complete dataset so that the package functions properly. Moreover the
#' attribute will be sorted in ascending order, i.e. if you input attr=
#' c(1,3,2), the decision matrix resulting will display the columns in order:
#' attr1 attr2 attr3.
#'
#' \code{rounds} Default calculates first round(initia product config) and last
#' round of the session. Default calculates with first and last attributes
#' (initial and final product configuration). To choose all give "all" as
#' argument for rounds, see example. "first" and "last" are also possible
#' argument values. You can give a vector of arbitrarily chosen rounds as well.
#'
#' \code{refps} If you only want to see the results for one attribute you may
#' enter only a couple of reference points but you have to tell the function
#' which attributes you want to use those referene points for. So the amount of
#' attr and of refps should be the same. Moreover the functions always orders de
#' attr, so be sure to input the reference point also in an ascending order
#' corresponding to their attributes. (refps will not be ordered)
#'
#' \code{result_type} Default assumes all your attributes are of benefit type,
#' that is a higher value in the attribute means the user is better of than with
#' a lower value. If one or more of the attributes in your data is of cost type,
#' e.g. price, so that lower is better then you should identify this attributes
#' as such, providing their id, they'll be converted to benefit type (higher
#' amount is better).
#'
#' \code{cost_ids} Default assumes all your attributes are of benefit type, that
#' is a higher value in the attribute means the user is better of than with a
#' lower value. If one or more of the attributes in your data is of cost type,
#' e.g. price, so that lower is better then you should identify this attributes
#' as such, providing their id, they'll be converted to benefit type (higher
#' amount is better).
#'
#' This function is for one user only, for more or all users see
#' \code{\link{powerful_function}}
#'
#' @return gain and loss matrix for a specific user.
#' @examples
#' gain_loss_matrices(pc_config_data, 11)
#' gain_loss_matrices(my_data, userid = 11, result_type = "cbind")
#' gain_loss_matrices(monitor_data, 50, rounds = "last", refps = c(0.1,0.3,0.4,0.5), cost_ids = 3)
#' gain_loss_matrices(data1, 40, attr = 1)
#' gain_loss_matrices(data, 3, result_type = "cbind", attr = c(1,2,3,4) )
#' @export

gainLoss <- function(dataset, userid = NULL, attr = NULL, rounds = NULL, refps = NULL, cost_ids = NULL, unlist = T) {
  gainList <- gainMatrix(dataset, userid, attr, rounds, refps, cost_ids) # here is probably the bottlenech
  lossList <- lossMatrix(dataset, userid, attr, rounds, refps, cost_ids) # and here

  desList <- decisionMatrix(dataset, userid, attr, rounds, cost_ids)
  refPs <- referencePoints(dataset, userid, refps, attr, cost_ids)

  tMatrixList <- lapply(desList, t)

  #gainList2 <- mapply(gainFunction, tMatrixList, refPs, SIMPLIFY = F)
  #lossList2 <- mapply(lossFunction, tMatrixList, refPs, SIMPLIFY = F)

  #if (unlist) {
    #gain.loss <- mapply(rbind, gainList, lossList, SIMPLIFY = F)
  #}
  #else{
  #gain.loss <- mapply(list, gain = gainList, loss = lossList, SIMPLIFY = F)
  #gain.loss <- mapply(rbind, gainList, lossList, SIMPLIFY = F)
  #}
  gain.loss <- mapply(rbind, gainList, lossList, SIMPLIFY = F)
  gain.loss
}

#' Normalizes gain and loss matrices
#'
#' Returns a list with two elements one is the normalized $ngain matrix and the
#' second one is a normalized $nloss matrix. There is an internal discussion
#' about whether a matrix with one row or round should be normalized, which is
#' not in the paper we based our calculations from. Up until this version, the
#' function normalizes every gain and loss matrices that it gets. For what this
#' means please see details.
#'
#' @param data data.frame with the user generated data from a product
#'   configurator. Please see \code{decision_matrix} for specifications of the
#'   data.frame.
#'
#' @param userid an integer that gives the information of which user the
#'   matrices should be calculated.
#'
#' @param attr attributes IDs, vector of integer numbers corresponding to the
#'   attributes you desire to use; attr are assumed to be 1-indexed.
#'
#' @param rounds integer vector. Which steps of the configuration process should
#'   be shown? See Details.
#'
#' @param refps numeric vector. Reference Points: each point corresponds to one
#'   attribute, i.e. each attribute has only one aspiration level. Default
#'   setting assumes the aspiration levels as the default values of the initial
#'   product configuration for each user.
#'
#' @param cost_ids argument used to convert selected cost attributes into
#'   benefit attributes. Integer vector.
#'
#' @details If you want to know more details about each parameter, look at
#' \code{gain_matrix loss_matrix decision_matrix}.
#'
#' The function normalizes both gain and loss matrices independently on the
#' amount of rows, for nrow > 1 this works as expected. The problem arises when
#' the matrices have only one row, i.e. one round. This results in normalized
#' matrices which can only contain 0 or 1 as a result, since a positive gain in
#' one specific attribute means a 0 in losses for the same attribute in the loss
#' matrix. Therefore if a gain is bigger than one, when normalizing it ends up
#' being 1 (gain) or -1 (loss) which loses information about the magnitude of
#' the gain and loss, respectively. Definitely a point to be discussed and
#' improved. Please refer to ...p2.
#'
#' This function is for one user only, for more or all users see
#' \code{\link{powerful_function}}
#'
#' @return  normalized gain and loss matrices for a specific user.
#' @examples
#' norm_g_l_matrices(pc_config_data, 11)
#' norm_g_l_matrices(my_data, userid = 11, result_type = "cbind")
#' norm_g_l_matrices(monitor_data, 50, rounds = "last", refps = c(0.1,0.3,0.4,0.5), cost_ids = 3)
#' norm_g_l_matrices(data1, 8, attr = 1)
#'
#' @export

## to be used
norm.gainLoss <- function(dataset, userid = NULL, attr = NULL, rounds = NULL, refps = NULL, cost_ids = NULL, binded = T) {
  desList <- decisionMatrix(dataset, userid, attr, rounds, cost_ids)
  refPs <- referencePoints(dataset, userid, refps, attr, cost_ids)

  tMatrixList <- lapply(desList, t)

  gainVector <- mapply(gainFunction, tMatrixList, refPs, SIMPLIFY = F)
  lossVector <- mapply(lossFunction, tMatrixList, refPs, SIMPLIFY = F)

  #Using matrix and lapply is harder, because different amount of rounds and therefore rows.
  gainList <- mapply(function(temp5, temp6) {dim(temp5) <- c(ncol(temp6), nrow(temp6)); temp5}, gainVector, desList, SIMPLIFY = F)
  lossList <- mapply(function(temp5, temp6) {dim(temp5) <- c(ncol(temp6), nrow(temp6)); temp5}, lossVector, desList, SIMPLIFY = F)

  gainList <- lapply(gainList, t) # correct form
  lossList <- lapply(lossList, t)

  bindedUnnorm <- mapply(rbind, gainList, lossList, SIMPLIFY = F)

  #Goal calculate hmax
  result4max <- lapply(bindedUnnorm, function(temp) apply(temp, 2, abs))
  hmaxVector <- lapply(result4max, function(temp1) apply(temp1, 2, max)) # returns a list with the hmax vector
  hmaxVector <- lapply(hmaxVector, function(temp2) replace(temp2, temp2==0.0, 1.00)) #remove 0 to avoid NA when dividing

  g.normMatrix <- mapply(function(aMatrix, aVector) aMatrix / aVector[col(aMatrix)], gainList, hmaxVector, SIMPLIFY = F)
  l.normMatrix <- mapply(function(aMatrix, aVector) aMatrix / aVector[col(aMatrix)], lossList, hmaxVector, SIMPLIFY = F)

  if (binded) {
    bothMatrix <-mapply(rbind, g.normMatrix, l.normMatrix, SIMPLIFY = F)
    bothMatrix
  }
  else {
    bothMatrix <- list(gain = g.normMatrix, loss = l.normMatrix) # use lapply to join gain-loss for each userid
  }
  bothMatrix
}

#' Calculates the gain for one attribute
#'
#' A simple function that given the value of an attribute (s_ij) and the
#' reference point of the same attribute, calculates the gain and returns it. It
#' is not built as a stand alone function, rather as an object to be used by
#' other major functions, such as \code{gain_matrix loss_matrix}.
#'
#' @param s_ij value of attribute j in round i
#'
#' @param e_j reference point off attribute j
#'
#' @details For understanding how this works, see the function itself or refer
#' to the paper. It handles only discrete numbers, so no interval numbers. Also
#' a point to improve further on.
#'
#' @return  gain, numeric value.
#' @examples
#' gain_fun_a(5, 1)  # returns: 4
#' gain_fun_a(2, 3)  # returns: 0
#'
#' @export

gainFunction <- function(v1, v2) {
  gainVector <- mapply(gain_fun_a, v1, v2)
  gainVector
}

#' Calculates the loss for one attribute
#'
#' A simple function that given the value of an attribute (s_ij) and the
#' reference point of the same attribute, calculates the loss and returns it. It
#' is not built as a stand alone function, rather as an object to be used by
#' other major functions, such as \code{gain_matrix loss_matrix}.
#'
#' @param s_ij value of attribute j in round i
#'
#' @param e_j reference point off attribute j
#'
#' @details For understanding how this works, see the function itself or refer
#' to the paper. It handles only discrete numbers, so no interval numbers. Also
#' a point to improve further on.
#'
#' @return  loss, numeric value.
#' @examples
#' loss_fun_a(5, 1)  # returns: 0
#' loss_fun_a(2, 3)  # returns: -1
#'
#' @export

lossFunction <- function(v1, v2) {
  lossVector <- mapply(loss_fun_a, v1, v2)
  lossVector
}
