#' Alternative method for normalizing matrices
#'
#' This function is used as the first step in
#' \code{\link{weight.differenceToIdeal}} and \code{\link{weight.entropy}} for normalizing a matrix.
#'
#' @param aMatrix a numeric (decision) matrix - if matrix has only one row, then
#'   all values will be normalized to 1 even if the initial value is 0.
#'
#' @param attr attributes IDs, vector of integer numbers corresponding to the
#'   attributes you desire to use; attr are assumed to be 1-indexed. Even if not
#'   all attributes are given the function normalizes all columns, by default as
#'   benefit type unless shown otherwise with \code{cost_ids}.
#'
#' @param cost_ids argument used to convert selected cost attributes into
#'   benefit attributes. Integer vector.
#'
#' @details This functions uses an alternative approach to the one used in
#'   \code{\link{gainLoss}} in regard to \code{cost_ids}. Instead of multiplying all
#'   column values with (-1) so that lower values become the higher values, it
#'   normalizes relative to the lowest value, as given by [1] and [2]. Main
#'   difference in the result is that this normalizing function does not return
#'   negative values.
#'
#' @return a normalized matrix
#'
#' @references [1]Ma, J., Fan, Z. P., & Huang, L. H. (1999). A subjective and
#'   objective integrated approach to determine attribute weights. European
#'   journal of operational research, 112(2), 397-404.
#'
#'   [2] Fan, Z. P. (1996). Complicated multiple attribute decision making:
#'   theory and applications (Doctoral dissertation, Ph. D. Dissertation,
#'   North-eastern University, Shenyang, PRC).
#'
#' @examples # Runnable
#' normalize.altMethod(matrix(1:16, 4, 4))
#' normalize.altMethod(matrix(1:16, 4, 4), attr=1:4, cost_ids = c(2,4))
#'
#' @export

normalize.altMethod <- function(aMatrix, attr, cost_ids = NULL) {
  if (nrow(aMatrix) == 1) {
    aMatrix <- apply(aMatrix, 1:2, function(t) 1)
  }
  else{
    aMatrix[,cost_ids] <- apply(aMatrix[,cost_ids, drop = F], 2, function(t) { a_max <- max(t); a_min <- min(t);
    if (a_max == 0 & a_min == 0) {a_max <- 1; a_min <- 1}
    if (a_max == a_min) {res <- t/a_max}
    else {res <- (a_max-t)/(a_max-a_min)}; res})

    if(!is.null(cost_ids)) {
      benefitAttr <- attr[!attr %in% cost_ids]
    }
    else {
      benefitAttr <- 1:ncol(aMatrix)
    }
    aMatrix[,benefitAttr] <- apply(aMatrix[,benefitAttr, drop = F], 2, function(t) { a_max <- max(t); a_min <- min(t);
    if (a_max == 0 & a_min == 0) {a_max <- 1; a_min <- 1}
    if (a_max == a_min) {res <- t/a_max}
    else {res <- (t-a_min)/(a_max-a_min)}; res})
  }
  aMatrix
}

#' Title
#'
#' This function is indifferent as to where it is a cost_ids or not, since the
#' same formula works for both. It does have a limitation, i.e. if on a single
#' attribute, there are simultaneously negative and positive values, in details?
#'
#' @param aMatrix
#'
#' @return
#'
#' @details Note: this function has one important limitation, if within a same
#'   attribute there are negative and positive values, the function will likely
#'   produce a \code{NaN}.
#' @export
#'
#' @examples

#' Normalizes a function according to the entropy method
#'
#' This function helps realize the first step in \code{\link{weight.entropy}}
#' within the entropy method for calculating weights, as shown for example by
#' [1] and [2].
#'
#' @param aMatrix a numeric (decision) matrix - if matrix has only one row, then
#'   all values will be normalized to 1 even if the initial value is 0.
#'
#' @details This functions uses the same equation to normalize benefit and cost
#'   type attributes. It returns values between 0 and 1, with the only
#'   limitation that when an attribute has both negative and positive values, it
#'   may return a \code{NaN}.
#'
#' @return a normalized matrix
#'
#' @references [1]Lotfi, F. H., and Fallahnejad, R. (2010). Imprecise Shannons entropy and
#'   multi attribute decision making. Entropy, 12(1), 53-62.
#'
#'   [2] Fan, Z. P. (1996). Complicated multiple attribute decision making: theory
#'   and applications (Doctoral dissertation, Ph. D. Dissertation, North-eastern
#'   University, Shenyang, PRC).
#'
#'   [2] Hwang, C. L., & Yoon, K. (2012). Multiple attribute decision making:
#'   methods and applications a state-of-the-art survey (Vol. 186). Springer
#'   Science & Business Media.
#'
#' @examples # Runnable
#' normalize.sum(matrix(1:16, 4, 4))
#' normalize.sum(matrix(32:47, 4, 4))
#'
#' @export
normalize.sum <- function(aMatrix) {
  if (nrow(aMatrix) == 1) {
    aMatrix <- apply(aMatrix, 1:2, function(t) 1)
  }
  sumVector <- apply(aMatrix, 2, sum)
  sumVector <- replace(sumVector, sumVector==0.0, 1)
  aMatrix <- apply(aMatrix, 1, function(t) t/sumVector)

  t(aMatrix)
}


#' Auxiliary function for calculating weights
#'
#' Does not need full documentation, since not primarly intended for users, but
#' as an auxiliary function to \code{\link{weight.highestValue}}.
#'
#' @param aMatrix a matrix to be 'normalized'
#'
#' @param attr attributes IDs, vector of integer numbers corresponding to the
#'   attributes you desire to use; attr are assumed to be 1-indexed. Even if not
#'   all attributes are given the function normalizes all columns, by default as
#'   benefit type unless shown otherwise with \code{cost_ids}.
#'
#' @param cost_ids argument used to convert selected cost attributes into
#'   benefit attributes. Integer vector.
#'
#' @return a normalized matrix
#'
#' @export

normalize.highestValue <- function(aMatrix, attr, cost_ids = NULL) {
  dontCut <- F
  if (nrow(aMatrix) == 3) {
    aMatrix <- matrix(1, 1, ncol(aMatrix))
    dontCut <- T
  }
  else{
    aMatrix[,cost_ids] <- apply(aMatrix[,cost_ids, drop = F], 2, function(t) { a_max <- t[1]; a_min <- t[2];
    if (a_max == 0 & a_min == 0) {a_max <- 1; a_min <- 1}
    if (a_max == a_min) {res <- t/a_max}
    else {res <- (a_max-t)/(a_max-a_min)}; res})

    if(!is.null(cost_ids)) {
      benefitAttr <- attr[!attr %in% cost_ids]
    }
    else {
      benefitAttr <- 1:ncol(aMatrix)
    }
    aMatrix[,benefitAttr] <- apply(aMatrix[,benefitAttr, drop = F], 2, function(t) {a_max <- t[1]; a_min <- t[2];
    if (a_max == 0 & a_min == 0) {a_max <- 1; a_min <- 1}
    if (a_max == a_min) {res <- t/a_max}
    else {res <- (t-a_min)/(a_max-a_min)}; res})
  }
  if (!dontCut) aMatrix <- aMatrix[-c(1,2), ]
  aMatrix
}

getHighestRound <- function(aList) {
  numberOfMax <- sapply(aList, which.is.max)
  numberOfMax
}
