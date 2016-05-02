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

#' Calcultes the Value Matrix
#'
#' According to the parameters, it first calculates the normalized gain and loss
#' matrices. Using \code{\link{prospect_value_matrix_extend}} it calculates the
#' value matrix using the value function given by Tversky & Kahnemann(1992)[1].
#'
#' @inheritParams referencePoints
#' @inheritParams pvalue_fun
#'
#' @details \code{dataset} We assume the input data.frame has following columns
#'   usid = User IDs, round = integers indicating which round the user is in
#'   (0-index works best for 'round'), atid = integer column for referring the
#'   attribute ID (1 indexed), selected = numeric value of the attribute for a
#'   specific, given round, selectable = amount of options the user can chose at
#'   a given round, with the current configuration. This is a necessary
#'   parameter.
#'
#'   \code{userid} is a necessary parameter.
#'
#'   For more details on the other parameters, please refer to
#'   \code{\link{decisionMatrix}}.
#'
#' @return the value matrix
#'
#' @examples
#' pvMatrix(myData, 9:10, rounds="all")
#' pvMatrix(data_pc, 100, weight=c(0.1,0.4,0.3,0.2))
#' pvMatrix(full_data, userid = 25 ,alpha = 0.95, beta = 0.78)
#'
#' @export

pvMatrix <- function(dataset, userid = NULL, attr = NULL, rounds = NULL, refps = NULL, cost_ids = NULL,
                     alpha = 0.88, beta = 0.88, lambda = 2.25) {
  normalizedgainLoss <- norm.gainLoss(dataset, userid, attr, rounds, refps, cost_ids, binded = F)
  pvMatrixList <- with(normalizedgainLoss, mapply(pvalue_fun, gain, loss, alpha, beta, lambda, SIMPLIFY = F))
  pvMatrixList
}

#' Implements prospect theory's value function
#'
#' Given gains and losses relative to a reference points, we use prospect
#' theory's value function to calculate the value matrix[1]. We use default values for the
#' parameters of diminishing sensitity and loss aversion, but this can be
#' inputed differently. Calculates the value function for one value at a time. The function
#' is used to calculate the value matrix [2] in \code{\link{pvMatrix}}.
#'
#' @param ngain_ij gain value corresponding to a specific attribute (j) and round (i)
#' @param nloss_ij loss value corresponding to the same specific attribute and round as \code{ngain_ij}
#' @param alpha parameter for diminishing sensitivity in the gain domain. Default value = 0.88
#' @param beta parameter for diminishing sensitivity in the loss domain. Default value = 0.88
#' @param lambda parameter for loss aversion. Default value = 2.25
#'
#' @return the output of the value function [1]
#'
#' @references [1] Kahneman, D., & Tversky, A. (1979). Prospect theory: An
#'   analysis of decision under risk. Econometrica: Journal of the Econometric
#'   Society, 263-291.
#'
#'   [2] Fan, Z. P., Zhang, X., Chen, F. D., & Liu, Y. (2013). Multiple
#'   attribute decision making considering aspiration-levels: A method based on
#'   prospect theory. Computers & Industrial Engineering, 65(2), 341-350.
#'
#' @examples
#' pvalue_fun(3, 0)
#' pvalue_fun(0, -1.5, alpha =0.75, lambda=3)
#'
#' @export

pvalue_fun <- function(ngain_ij, nloss_ij, alpha = 0.88, beta = 0.88, lambda = 2.25) {
  result <- ((ngain_ij)^alpha) + (-lambda*((-nloss_ij)^beta))
  result

}

#'Gain matrix
#'
#'Creates the Gains matrix parting from a decision matrix and a vector
#'containing the reference points (typically the status-quo). A gain represents
#'a positive difference between a given value in the decision matrix and its
#'corresponding reference point. We based our calculations for gains and losses
#'from a scientific paper, please see source and references [1,2]. This
#'functions is intended to use only for single referene point theories, not for
#'multiple reference point approaches. For the latter, refer to
#'\code{\link{overallDRP}}, and \code{\link{overallTRP}}.
#'
#'@inheritParams referencePoints
#'
#'@details General: The returned gainMatrix has: ncol = number of attributes you
#'  selected or all(default) and nrow= number of rounds you selected or the
#'  first and last(default) for a selected user. Results are unnamed.
#'
#'  \code{dataset} We assume the input data.frame has following columns usid =
#'  User IDs, round = integers indicating which round the user is in (0-index
#'  works best for 'round'), atid = integer column for referring the attribute
#'  ID (1 indexed), selected = numeric value of the attribute for a specific,
#'  given round, selectable = amount of options the user can chose at a given
#'  round, with the current configuration. This is a necessary parameter.
#'
#'  \code{userid} is a necessary parameter, without it you'll get a warning.
#'  Default is NULL.
#'
#'  \code{attr} Default calculates with all attributes. Attributes are
#'  automatically read from provided table, it is important you always provide
#'  the complete dataset so that the package functions properly. Moreover the
#'  attributes will not be sorted. Output columns are returned in the ordered
#'  they were inputed.
#'
#'  \code{rounds} If you need to compute different rounds for each user you
#'  enter, this argument accepts a list of integer vectors indicating which
#'  rounds should be used for each user. The function does not read names, it
#'  works in the order the list was given.
#'
#'  \code{refps} If you only want to see the results for one attribute you may
#'  enter only a couple of reference points but you have to tell the function
#'  which attributes you want to use those referene points for. So the amount of
#'  attr and of refps should be the same. Moreover the functions always orders
#'  de attr, so be sure to input the reference point also in an ascending order
#'  corresponding to their attributes. (refps will not be ordered)
#'
#'  \code{cost_ids} If \code{attr} and \code{cost_ids} differ, the functions
#'  will first compute the entire decision matrix using the \code{cost_ids} and
#'  only in the end will it 'subset' the result to the desired \code{attr}.
#'
#'@return a list of gain matrices, one for each user.
#'
#'@references [1] Fan, Z. P., Zhang, X., Chen, F. D., & Liu, Y. (2013). Multiple
#'  attribute decision making considering aspiration-levels: A method based on
#'  prospect theory. Computers & Industrial Engineering, 65(2), 341-350.
#'
#'  [2]Kahneman, D., & Tversky, A. (1979). Prospect theory: An analysis of
#'  decision under risk. Econometrica: Journal of the Econometric Society,
#'  263-291.
#'
#' @examples #Not Runnable yet
#' gainMatrix(pc_config_data, 9:11)
#' gainMatrix(my_data, userid = 11, rounds="all")
#' gainMatrix(keyboard_data, 60, refps = c(1,3,4,0), cost_ids = 4)
#' gainMatrix(data1, 2, rounds = "last", attr = 1, cost_ids=1)
#'@export

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

#'Loss matrix
#'
#'Creates the Loss matrix parting from a decision matrix and a vector containing
#'the reference points (typically the status-quo). A loss represents a positive
#'difference between a given value in the decision matrix and its corresponding
#'reference point [1,2]. This functions is intended to use only for single
#'referene point theories, not for multiple reference point approaches. For the
#'latter, refer to \code{\link{overallDRP}}, and \code{\link{overallTRP}}.
#'
#'@inheritParams referencePoints
#'
#'@details The returned lossMatrix has: ncol = number of attributes you selected
#'  or all(default) and nrow= number of rounds you selected or the first and
#'  last(default) for a selected user. Results are unnamed.
#'
#'  \code{dataset} We assume the input data.frame has following columns usid =
#'  User IDs, round = integers indicating which round the user is in (0-index
#'  works best for 'round'), atid = integer column for referring the attribute
#'  ID (1 indexed), selected = numeric value of the attribute for a specific,
#'  given round, selectable = amount of options the user can chose at a given
#'  round, with the current configuration. This is a necessary parameter.
#'
#'  \code{userid} is a necessary parameter, without it you'll get a warning.
#'  Default is NULL.
#'
#'  \code{attr} Default calculates with all attributes. Attributes are
#'  automatically read from provided table, it is important you always provide
#'  the complete dataset so that the package functions properly. Moreover the
#'  attributes will not be sorted. Output columns are returned in the ordered
#'  they were inputed.
#'
#'  \code{rounds} If you need to compute different rounds for each user you
#'  enter, this argument accepts a list of integer vectors indicating which
#'  rounds should be used for each user. The function does not read names, it
#'  works in the order the list was given.
#'
#'  \code{refps} If you only want to see the results for one attribute you may
#'  enter only a couple of reference points but you have to tell the function
#'  which attributes you want to use those referene points for. So the amount of
#'  attr and of refps should be the same. Moreover the functions always orders
#'  de attr, so be sure to input the reference point also in an ascending order
#'  corresponding to their attributes. (refps will not be ordered)
#'
#'  \code{cost_ids} If \code{attr} and \code{cost_ids} differ, the functions
#'  will first compute the entire decision matrix using the \code{cost_ids} and
#'  only in the end will it 'subset' the result to the desired \code{attr}.
#'
#' @return a list of loss matrices, one for each user.
#'
#' @references [1] Fan, Z. P., Zhang, X., Chen, F. D., & Liu, Y. (2013). Multiple
#'  attribute decision making considering aspiration-levels: A method based on
#'  prospect theory. Computers & Industrial Engineering, 65(2), 341-350.
#'
#'  [2]Kahneman, D., & Tversky, A. (1979). Prospect theory: An analysis of
#'  decision under risk. Econometrica: Journal of the Econometric Society,
#'  263-291.
#'
#' @examples #Not Runnable yet
#' lossMatrix(pc_config_data, 9:11)
#' lossMatrix(my_data, userid = 11, rounds="all")
#' lossMatrix(keyboard_data, 60, refps = c(1,3,4,0), cost_ids = 4)
#' lossMatrix(data1, 2, rounds = "last", attr = 1, cost_ids=1)
#'@export

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
#' The matrices are composed by row-binded gain matrices (on top) and loss
#' matrices (below). This is more of an auxiliary functions, since it is easiert
#' to work with united matrices. Rows and columns are unnamed. The output of this
#' function is further used by \code{\link{norm.gainLoss}}.
#'
#' @inheritParams referencePoints
#'
#' @details \code{dataset} We assume the input data.frame has following columns
#'   usid = User IDs, round = integers indicating which round the user is in
#'   (0-index works best for 'round'), atid = integer column for referring the
#'   attribute ID (1 indexed), selected = numeric value of the attribute for a
#'   specific, given round, selectable = amount of options the user can chose at
#'   a given round, with the current configuration. This is a necessary
#'   parameter.
#'
#'   \code{dataset} We assume the input data.frame has following columns usid =
#'   User IDs, round = integers indicating which round the user is in (0-index
#'   works best for 'round'), atid = integer column for referring the attribute
#'   ID (1 indexed), selected = numeric value of the attribute for a specific,
#'   given round, selectable = amount of options the user can chose at a given
#'   round, with the current configuration. This is a necessary parameter.
#'
#'   \code{userid} is a necessary parameter, without it you'll get a warning.
#'   Default is NULL.
#'
#'   \code{attr} Default calculates with all attributes. Attributes are
#'   automatically read from provided table, it is important you always provide
#'   the complete dataset so that the package functions properly. Moreover the
#'   attributes will not be sorted. Output columns are returned in the ordered
#'   they were inputed.
#'
#'   \code{rounds} If you need to compute different rounds for each user you
#'   enter, this argument accepts a list of integer vectors indicating which
#'   rounds should be used for each user. The function does not read names, it
#'   works in the order the list was given.
#'
#'   \code{refps} If you only want to see the results for one attribute you may
#'   enter only a couple of reference points but you have to tell the function
#'   which attributes you want to use those referene points for. So the amount
#'   of attr and of refps should be the same. Moreover the functions always
#'   orders de attr, so be sure to input the reference point also in an
#'   ascending order corresponding to their attributes.
#'
#' @return a list of combined matrices.
#'
#' @examples #Not runnable yet
#' gainLoss(pc_config_data, 11:12)
#' gainLoss(my_data, userid = 10:100)
#' gainLoss(monitor_data, c(50,51), rounds = "last", refps = c(0.1,0.3,0.4,0.5), cost_ids = 3)
#' gainLoss(data1, 40, attr = 1)
#' gainLoss(data, 3, attr = c(1,2,3,4))
#' @export

gainLoss <- function(dataset, userid = NULL, attr = NULL, rounds = NULL, refps = NULL, cost_ids = NULL) {
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
#' Returns a list with two elements one is the normalized \code{$gain} matrix
#' and the second one is a normalized \code{$loss} matrix [1]. It calculates
#' both matrices separately, binds them together with \code{rbind} and
#' normalizes both according to the largest value in each column, including
#' values of both matrices. The output style can be changed throught the
#' \code{binded} argument. Rows and columns are named.
#'
#' @inheritParams referencePoints
#'
#' @param binded logical - Should the gain and loss matrices be outputed in a
#'   binded format or separately? Default is true, which returns a single matrix
#'   for each user.
#'
#' @details If you want to know more details about each parameter, look at
#'   \code{\link{gainMatrix}}, \code{\link{lossMatrix}} or
#'   \code{\link{decisionMatrix}}.
#'
#'   The function normalizes both gain and loss matrices independently on the
#'   amount of rows, for nrow > 1 this works as expected. The problem arises
#'   when the matrices have only one row, i.e. one round. This results in
#'   normalized matrices which can only contain 0 or 1 as a result, since a
#'   positive gain in one specific attribute means a 0 in losses for the same
#'   attribute in the loss matrix. Therefore if a gain is bigger than one, when
#'   normalizing it ends up being 1 (gain) or -1 (loss) which loses information
#'   about the magnitude of the gain and loss, respectively. Definitely a point
#'   to be discussed and improved. Please refer to ...p2.
#'
#'   This function is vectorialized in the \code{userid} argument.
#'
#' @return a list - of normalized gain and loss matrices for each user.
#'
#' @references [1] Fan, Z. P., Zhang, X., Chen, F. D., & Liu, Y. (2013).
#'   Multiple attribute decision making considering aspiration-levels: A method
#'   based on prospect theory. Computers & Industrial Engineering, 65(2),
#'   341-350.
#'
#' @examples
#' norm.gainLoss(pc_config_data, 11)
#' norm.gainLoss(pc_config_data, c(11,12,13,14,15,16,17,18))
#' norm.gainLoss(myData, 9, rounds=c(1,2,3))
#' norm.gainLoss(cam4, userid=20:30, refps=c(1.5,1.5,1.5,1.5), rounds="all", binded=F)
#' norm.gainLoss(data1, 8:16, attr = 1)
#'
#' @export

norm.gainLoss <- function(dataset, userid = NULL, attr = NULL, rounds = NULL, refps = NULL, cost_ids = NULL, binded = T) {
  desList <- decisionMatrix(dataset, userid, attr, rounds, cost_ids)
  refPs <- referencePoints(dataset, userid, refps, attr, cost_ids)

  #Save names
  namesOfCols <- colnames(desList[[1]])
  namesOfRows <- lapply(desList, function(temp) row.names(temp))

  tMatrixList <- lapply(desList, t)

  gainVector <- mapply(gainFunction, tMatrixList, refPs, SIMPLIFY = F)
  lossVector <- mapply(lossFunction, tMatrixList, refPs, SIMPLIFY = F)

  # Using matrix and lapply is harder, because different amount of rounds and therefore rows.
  gainList <- mapply(function(temp5, temp6) {dim(temp5) <- c(ncol(temp6), nrow(temp6)); temp5}, gainVector, desList, SIMPLIFY = F)
  lossList <- mapply(function(temp5, temp6) {dim(temp5) <- c(ncol(temp6), nrow(temp6)); temp5}, lossVector, desList, SIMPLIFY = F)

  gainList <- lapply(gainList, t) # correct form
  lossList <- lapply(lossList, t)


  gainList <- lapply(gainList, function(temp1, temp2) {colnames(temp1) <- temp2; temp1}, namesOfCols)
  gainList <- mapply(auxiliaryNameRows, gainList, namesOfRows, SIMPLIFY = F)
  lossList <- lapply(gainList, function(temp1, temp2) {colnames(temp1) <- temp2; temp1}, namesOfCols)
  lossList <- mapply(auxiliaryNameRows, gainList, namesOfRows, SIMPLIFY = F)

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

#' Calculates the gain for one single value
#'
#' Given the value of an attribute (v1) and the reference point of the same
#' attribute (v2), calculates the gain and returns it [1,2]. It is not built as
#' a stand alone function, rather as an object to be used by other major
#' functions, such as \code{gainMarix, lossMatrix}.
#'
#' @param v1 value of attribute j in round i
#'
#' @param v2 reference point of attribute j
#'
#' @details For understanding how this works, see the function itself or refer
#'   to the paper. It handles only discrete numbers, so no interval numbers.
#'   Also a point to improve further on.
#'
#' @return  gain, numeric value.
#'
#' @references [1] Fan, Z. P., Zhang, X., Chen, F. D., & Liu, Y. (2013).
#'   Multiple attribute decision making considering aspiration-levels: A method
#'   based on prospect theory. Computers & Industrial Engineering, 65(2),
#'   341-350.
#'
#'   [2]Kahneman, D., & Tversky, A. (1979). Prospect theory: An analysis of
#'   decision under risk. Econometrica: Journal of the Econometric Society,
#'   263-291.
#'
#' @examples
#' gainFunction(5, 1)  # returns: 4
#' gainFunction(2, 3)  # returns: 0
#'
#' @export

gainFunction <- function(v1, v2) {
  gainVector <- mapply(gain_fun_a, v1, v2)
  gainVector
}

#' Calculates the loss for one single value
#'
#' Given the value of an attribute (v1) and the
#' reference point of the same attribute (v2), calculates the loss and returns it [1,2]. It
#' is not built as a stand alone function, rather as an object to be used by
#' other major functions, such as \code{gainMarix, lossMatrix}.
#'
#' @param v1 value of attribute j in round i
#'
#' @param v2 reference point of attribute j
#'
#' @details For understanding how this works, see the function itself or refer
#' to the paper. It handles only discrete numbers, so no interval numbers. Also
#' a point to improve further on.
#'
#' @return loss, numeric value.
#'
#' @references [1] Fan, Z. P., Zhang, X., Chen, F. D., & Liu, Y. (2013). Multiple
#'  attribute decision making considering aspiration-levels: A method based on
#'  prospect theory. Computers & Industrial Engineering, 65(2), 341-350.
#'
#'  [2]Kahneman, D., & Tversky, A. (1979). Prospect theory: An analysis of
#'  decision under risk. Econometrica: Journal of the Econometric Society,
#'  263-291.
#'
#' @examples
#' lossFunction(5, 1)  # returns: 0
#' lossFunction(2, 3)  # returns: -1
#'
#' @export

lossFunction <- function(v1, v2) {
  lossVector <- mapply(loss_fun_a, v1, v2)
  lossVector
}

#Auxiliary not need to document
gain_fun_a <- function(s_ij, e_j) {
  if(s_ij >= e_j) {
    gain <- s_ij - e_j
  }
  else {
    gain <- 0
  }
  gain
}
#Auxiliary not need to document
loss_fun_a <- function(s_ij, e_j) {
  if(s_ij >= e_j) {
    loss <- 0
  }
  else {
    loss <- s_ij - e_j
  }
  loss
}

auxiliaryNameRows <- function(matrix1, rowNames) {
  if(!is.null(dim(matrix1))) {
    row.names(matrix1) <- rowNames
    }
  else {}
  as.matrix(matrix1)
}
