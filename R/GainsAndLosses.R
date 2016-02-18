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

gain_matrix <- function(data, userid = NULL, attr = NULL, rounds = NULL, refps = NULL, cost_ids = NULL) {
  ## Calculate decision matrix based on inputs
  des_matrix <- decision_matrix(data, userid, attr, rounds, cost_ids)


  ## Get reference points(aspiration-levels) from input
  refps_vector <-ref_points(data, userid, refps, attr, cost_ids)

  ## Create empty result Gain matrix
  dim_matrix <- dim(des_matrix)
  gain_matrix <- matrix(2, dim_matrix[1],dim_matrix[2])

  ## TODO: 1. name the matrix
  m <- 1
  ## Fill the matrix
  for(n in 1:dim_matrix[1]) {
    gain_matrix[m, ] <- mapply(gain_fun_a, des_matrix[m, ], refps_vector)
    m <- m + 1
  }

  gain_matrix

}

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

gainMatrix2 <- function(dataset, userid = NULL, attr = NULL, rounds = NULL, refps = NULL, cost_ids = NULL) {
  desList <- decisionMatrix(dataset, userid, attr, rounds, cost_ids)
  refPs <- referencePoints(dataset, userid, refps, attr, cost_ids)

  tMatrixList <- lapply(desList, t)

  result <- mapply(gainFunction, tMatrixList, refPs, SIMPLIFY = F)

  allRounds <- getRoundsById(dataset, userid)
  if (is.null(attr)) attr <- get_attrs_ID(dataset)

  if(is.null(rounds)) {
    rounds <-  rep(2, times=length(userid))
  }
  else if (rounds == "all"){
    rounds <- sapply(allRounds, length)
  }
  else if (rounds == "last") {
    rounds <-  rep(1, times=length(userid))
  }
  else if (rounds == "first") {
    rounds <-  rep(1, times=length(userid))
  }
  for(i in 1:length(rounds)) {
    result[[i]] <- matrix(result[[i]],nrow = rounds[i], ncol = length(attr), byrow = T)
  }
  result
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

loss_matrix <- function(data, userid = NULL, attr = NULL, rounds = NULL, refps = NULL, cost_ids = NULL) {
  ## Calculate decision matrix based on inputs
  des_matrix <- decision_matrix(data, userid, attr, rounds, cost_ids)


  ## Get reference points(aspiration-levels) from input
  refps_vector <-ref_points(data, userid, refps, attr, cost_ids)

  ## Create empty result Gain matrix
  dim_matrix <- dim(des_matrix)
  loss_matrix <- matrix(2, dim_matrix[1],dim_matrix[2])

  ## TODO: 1. name the matrix
  m <- 1
  ## Fill the matrix
  for(n in 1:dim_matrix[1]) {
    loss_matrix[m, ] <- mapply(loss_fun_a, des_matrix[m, ], refps_vector)
    m <- m + 1
  }
  loss_matrix
}

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
gain_loss_matrices <- function(data, userid = NULL, attr = NULL, rounds = NULL, refps = NULL, result_type = NULL, cost_ids = NULL) {

  g_matrix <- gain_matrix(data, userid, attr, rounds, refps, cost_ids)
  l_matrix <- loss_matrix(data, userid, attr, rounds, refps, cost_ids)

  ##Depending on result_type return matrices accordingly
  if(is.null(result_type)){
    result <- list(gain = g_matrix, loss = l_matrix)
  }
  else if(result_type == "rbind") {
    result <- rbind(g_matrix, l_matrix)
  }
  else if(result_type == "cbind") {
    result <- cbind(g_matrix, l_matrix)
  }
  result

}

gainLoss <- function(dataset, userid = NULL, attr = NULL, rounds = NULL, refps = NULL, cost_ids = NULL) {
  gainList <- gainMatrix(dataset, userid, attr, rounds, refps, cost_ids)
  lossList <- lossMatrix(dataset, userid, attr, rounds, refps, cost_ids)

  #if (unlist) {
  #  gain.loss <- mapply(rbind, gainList, lossList, SIMPLIFY = F)
  #}
  #else{
  #  gain.loss <- mapply(list, gain = gainList, loss = lossList, SIMPLIFY = F)
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

norm_g_l_matrices <- function(data, userid = NULL, attr = NULL, rounds = NULL, refps = NULL, cost_ids = NULL) {
  g_l_matrix <- gain_loss_matrices(data, userid, attr, rounds, refps, result_type="rbind", cost_ids)

  n_gain <- gain_matrix(data, userid, attr, rounds, refps, cost_ids)
  n_loss <- loss_matrix(data, userid, attr, rounds, refps, cost_ids)

  hmax_vector <- numeric(0)
  number_col <-  dim(g_l_matrix)[2]

  for(n in 1:number_col) {
    hmax_vector <- c(hmax_vector, max(abs(g_l_matrix[,n])))
    if(hmax_vector[n] == 0) {

    }
    else {
      n_gain[,n] <- n_gain[,n]/hmax_vector[n]
      n_loss[,n] <- n_loss[,n]/hmax_vector[n]
    }
  }
  result <- list(ngain = n_gain, nloss = n_loss)
  result
}

norm.gainLoss <- function(dataset, userid = NULL, attr = NULL, rounds = NULL, refps = NULL, cost_ids = NULL) {
  desList <- decisionMatrix(dataset, userid, attr, rounds, cost_ids)
  refPs <- referencePoints(dataset, userid, refps, attr, cost_ids)

  tMatrixList <- lapply(desList, t)

  gainList <- mapply(gainFunction, tMatrixList, refPs, SIMPLIFY = F)
  lossList <- mapply(lossFunction, tMatrixList, refPs, SIMPLIFY = F)
  #Check if maybe faster with gainLoss unlist= T, think it through before executing

  #bindedGain <- mapply(function(tempData7, tempData8) {dim(tempData7) <- c(nrow(tempData8), ncol(tempData8)); tempData7}, gainList, desList, SIMPLIFY = F)
  #bindedLoss <- mapply(function(tempData7, tempData8) {dim(tempData7) <- c(nrow(tempData8), ncol(tempData8)); tempData7}, lossList, desList, SIMPLIFY = F)

  vectorBoth <- mapply(c, gainList, lossList, SIMPLIFY = F)
  matrixBoth <- mapply(function(tempData7, tempData8) {dim(tempData7) <- c(nrow(tempData8)*2, ncol(tempData8)); tempData7}, vectorBoth, desList, SIMPLIFY = F)
  #Goal calculate hmax
  #With info you have create a 'muster' template with correct number of attributes/columns! rows may vary

  result4max <- lapply(matrixBoth, function(temp) apply(temp, 2, abs))
  hmaxVector <- lapply(result4max, function(temp) apply(temp, 1, max)) # returns a list with the hmax vector
  hmaxVector <- lapply(hmaxVector, function(temp2) replace(temp2, temp2==0.0, 1.00)) #remove 0 to avoid NA when dividing

  g.normMatrix <- mapply("/", gainList, hmaxVector, SIMPLIFY = F)
  l.normMatrix <- mapply("/", lossList, hmaxVector, SIMPLIFY = F)

  # Idea t() in the end
  g.normMatrix <- lapply(g.normMatrix, t)
  l.normMatrix <- lapply(l.normMatrix, t)
  g.normMatrix
  l.normMatrix
  #bothMatrix <- lapply(gainLoss(dataset, userid, attr, rounds, refps, cost_ids, unlist=T),t)
  #bothMatrix <- mapply("%/%", bothMatrix, hmaxVector, SIMPLIFY = F)
  #bothMatrix <- lapply(bothMatrix, t)
  #bothMatrix

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

gain_fun_a <- function(s_ij, e_j) {
  if(s_ij >= e_j) {
    gain <- s_ij - e_j
  }
  else {
    gain <- 0
  }
  gain
}

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
loss_fun_a <- function(s_ij, e_j) {
  if(s_ij >= e_j) {
    loss <- 0
  }
  else {
    loss <- s_ij - e_j
  }
  loss
}

lossFunction <- function(v1, v2) {
  lossVector <- mapply(loss_fun_a, v1, v2)
  lossVector
}
