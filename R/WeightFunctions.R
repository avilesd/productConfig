## Not vectorized : deprecated 27.01.16

weight_higher_sum_value <- function(dataset, userid = NULL , rounds = NULL, cost_ids = NULL) {

  ##Calculate with always 4 attribute so that function works properly, take attr into account at result level.
  all_dec_matrices <- powerful_function(dataset, userid, FUN = decision_matrix, attr = NULL, rounds = "all", refps = NULL, cost_ids,
                                        weight = NULL, alpha = 0.88, beta = 0.88, lambda = 2.25, gainm = TRUE, result_type = NULL)

  if(length(userid) != 1) stop("Please enter only one userid, for more see powerful_function.")

  length_attr <- length(get_attrs_ID(dataset))

  sum_help <- rep.int(0, length_attr)

  for(i in all_dec_matrices) {

    help <- apply(i, 2, sum)
    sum_help <- sum_help + help
    sum_help <- abs(sum_help)
  }
  almost <- sum_help * c(1,1,1,10.5147)
  result <- almost/sum(almost)
  result

}

# Unncesessary arguments, but perhaps for other functions
get_attr_weight <- function(dataset = NULL, userid = NULL, weight = NULL,  attr = NULL, rounds = NULL, cost_ids = NULL) {

  if(is.null(attr)) attr <- get_attrs_ID(dataset) #Step 1 to handle attr

  if(is.null(weight) & is.null(userid)) {
    stop("You need to provide one user id OR enter your own weights")
  }
  if(is.null(weight)) {
    result <- weight_higher_sum_value(dataset, userid, rounds=rounds, cost_ids=cost_ids)
  }
  ## TODO proove if length(w) =length(attributes), proof if numeric and the sum of all = 1 or handle with result[attr]!!!!!!!!
  else{
    result <- weight
  }
  result <- result[attr] # Step 2 to handle attr
  result
}

## Docu: Vectorized interface function for real weight functions(not vectorized)
## Weights have to be input as list or single vector length(input) == length(allAttr)
## Weights will only accept all inputs in x, y equals length of userid.
## DOCU: Look at cases in notes, but three cases list1, list2+ or vector as input.
## If weights are given, it returns them as lists. Must return list.
## DOCU Important: Weight parameter allows for not 1 summing and negative inputs, up to
## user to check if make sense, result of negative weight is on negativity of some
## oPV not on its magnitude
## DOCU: From @overallPV, attr are not sorted, so attr and weight have to be given in same order.


#' Title
#'
#' @param dataset
#' @param userid
#' @param weight
#' @param attr
#' @param rounds
#' @param cost_ids
#' @param weightFUN
#'
#' @return
#' @export
#'
#' @examples
#' getAttrWeights(..., weightFUN = "differenceToIdeal")
#'
getAttrWeights <- function(dataset = NULL, userid = NULL, weight = NULL,  attr = NULL, rounds = NULL, cost_ids = NULL, weightFUN = "deprecated_FUN") {
  if((is.null(dataset) | is.null(userid)) & is.null(weight)) {
    stop("You need to provide the weights ('weight =') or userids + dataset for them to be calculated.")
  }
  if(is.vector(weight) & !is.list(weight)) {
    weight <- list("oneVector" = weight)
  }
  if(!is.vector(weight) & !is.list(weight) & !is.null(weight)) {
    stop("Input in weight parameter needs to be a list of numeric vectors or just one vector.")
  }

  if(is.null(weight)) {
    # ! ToDo check if functions handle correctly inputting userids missing. Old weight_sum_value calls other f(x) that do
    if (weightFUN == "deprecated_FUN") {
      result <- powerful_function(dataset, userid, FUN = get_attr_weight, weight, attr = attr, rounds = rounds, cost_ids=cost_ids)
    }
    ## DOCU: New functions must take into account attributes and calculate accordingly, perhaps it doesn't make sense with our data,
    ## but we have to give the choice
    if (weightFUN == "differenceToIdeal") {
      result <- weight.differenceToIdeal(dataset, userid, attr, rounds, cost_ids)
    }
    if (weightFUN == "Test2") {
      result <- weight_higher_sum_value(dataset, userid, attr, rounds, cost_ids)
    }
    if (weightFUN == "Test3") {
      result <- weight_higher_sum_value(dataset, userid, attr, rounds, cost_ids)
    }
  }
  else { result <- weight}
  result
}

## DOCU: New functions must take into account attributes and calculate accordingly, perhaps it doesn't make sense with our data,
## but we have to give the choice
#######
####### Idea for new function, normalize decision matrix, then
##' ndec11 <- norm.gainLoss(myData, 11, rounds="all")
#ndec11
#ndiffMatrix11 <- apply(ndec11[[1]], 2, diff)
#nabsdiffMatrix11 <- apply(ndiffMatrix11, 2, abs)
#nsumdiffMatrix11 <- apply(ndiffMatrix11, 2, sum)
#nsumdiffMatrix11 <- apply(nabsdiffMatrix11, 2, sum)

#' Calculates attribute weights using the 'objective approach'
#'
#' This function first normalizes a list of matrices and then calculates the
#' decision weight for each attribute, using the 'objective approach' as given
#' by [1] and [2]. The objective approach, in this case, uses only data gathered
#' from the decision matrix and it does not need a 'subjective' preference
#' matrix from the decision maker. The sum of a weight vector should always
#' equal 1.
#'
#' The result is a list of vectors with the same length as the number of columns
#' of the input matrices, i.e. each column gets a weight.
#'
#' @param dataset data.frame with the user generated data from a product
#'   configurator. See \code{decisionMatrix} for specifications of the dataset.
#' @param userid a vector of integers that gives the information of which users
#'   the matrix should be calculated. Vectorised.
#' @param attr attributes IDs, vector of integer numbers corresponding to the
#'   attributes you desire to use; attr are assumed to be 1-indexed. This
#'   function will calculate with all attributes and do the subsetting a
#'   posteriori.
#'
#'   If you want to get the weights for only two attributes you will have to
#'   first use \code{\link{decisionMatrix}} and then pass it on to
#'   \code{\link{normalize.altMethod}} and \code{\link{differenceToIdeal}}.
#' @param rounds integer vector or text option. Which steps of the configuration
#'   process should be taken into account? Defaults are "all" in order to have
#'   more data to calculate with. If \code{"first"} or \code{"last"} are entered
#'   there will be only one rounds to gather data from, consequently all
#'   attribtues will have the same weight.
#' @param cost_ids argument used to convert selected cost attributes into
#'   benefit attributes. Integer vector. This functions uses an alternative
#'   method \code{\link{normalize.altMethod}} that does not produce negative
#'   values.
#'
#' @details This function rewards attributes which values do not change much
#'   throughtout the decision matrix, even if the value is the lowest value for
#'   that attribute. For an opposite implicit effect see \code{\link{}}
#'   #####################################################reminder
#'
#'   \code{cost_ids} As in the other functions, if you enter a cost_ids that is
#'   not in your entered attributes, the functions will calculate the output
#'   with all attributes in your data, including the cost(s) attributes and only
#'   after the calculations does the function subset the result according to the
#'   \code{attr} input. When the attributes and cost_ids differ, the function
#'   allows the calculation but it will throw a warning.
#'
#' @return a list of weight vector(s)
#'
#' @references [1]Ma, J., Fan, Z. P., & Huang, L. H. (1999). A subjective and
#'   objective integrated approach to determine attribute weights. European
#'   journal of operational research, 112(2), 397-404. [2] Fan, Z. P. (1996).
#'   Complicated multiple attribute decision making: theory and applications
#'   (Doctoral dissertation, Ph. D. Dissertation, North-eastern University,
#'   Shenyang, PRC).
#'
#' @examples # Not runnable yet
#' weight.differenceToIdeal(myData, 15:22)
#' weight.differenceToIdeal(laptop_data, 40:45, attr= c(1, 3, 4), cost_ids = 4)
#' savedWeights <- weight.differenceToIdeal(myData, c(6, 15, 18, 20, 26), attr = 1:4, cost_ids = 4, rounds=1:8)
#' lapply(savedWeights, sum) # Should return 1 for any output of this function
#'
#' @export

weight.differenceToIdeal <- function(dataset, userid = NULL , attr = NULL, rounds = "all", cost_ids = NULL) {

  # Common Errors catched in dM (Tested: attr, rounds, all.users, cost_ids)
  if (is.null(attr)) attr <- get_attrs_ID(dataset)
  if (!all(cost_ids %in% attr)) warning("One of your cost_ids is not in your attributes. Is this still your intended result?")

  decisionList <- decisionMatrix(dataset, userid, attr = NULL, rounds)
  normList <- lapply(decisionList, normalize.altMethod, attr, cost_ids)
  weightList <- lapply(normList, differenceToIdeal, attr)
  weightList <- lapply(weightList, function(t) t[attr])
  weightList
}

#' Alternative method for normalizing matrices
#'
#' This function is used as the first step in
#' \code{\link{weight.differenceToIdeal}} for normalizing a matrix.
#'
#' @param aMatrix a numeric (decision) matrix - if matrix has only one row, then
#'   all values will be normalized to 1 even if the initial value is 0.
#'
#' @param attr attributes IDs, vector of integer numbers corresponding to the
#'   attributes you desire to use; attr are assumed to be 1-indexed. Even if not
#'   all attributes are given the function normalizes all columns, by default as
#'   benefit type unless shown otherwise with \code{cost_ids}.
#' @param cost_ids argument used to convert selected cost attributes into
#'   benefit attributes. Integer vector.
#'
#' @details This functions uses an alternative approach to the one used in
#'   \code{\link{gainLoss}} for \code{cost_ids}. Instead of multiplying all
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

#' Calculates decision weights using the 'objective approach'
#'
#' This function is used as the second step in
#' \code{\link{weight.differenceToIdeal}} for calculating a decision weight for
#' each \code{\link{attr}} in the decision matrix. The methodology of the
#' 'objective approach' for determining the weights is given by references [1]
#' and [2]. See Details.
#'
#' The sum of the output of this functions should always equal 1.
#'
#' @param normalizedMatrix a numeric matrix. If indeed normalized it should only
#'   contain values between \code{0} and \code{1}.
#' @param attr attributes IDs, vector of integer numbers corresponding to the
#'   attributes you desire to use; attr are assumed to be 1-indexed. Here it
#'   represents the number of columns of the input matrix.
#' @details It measures the distance of each column value against the best value
#'   for a given attribute. A smaller difference means that for that attribute a
#'   high value was \emph{consistently} taking in consideration, thus resulting
#'   in a higher weight. Two unintended consequences are: 1. matrices with one
#'   row will result in the same weight for all columns and 2. for an attribute
#'   where the value does not change at all (even if it's a low value) the
#'   function will reward it somewhat disproportionately.
#'
#' @return a decision weight (numeric vector with a sum of 1)
#'
#' @examples #Runnable
#' differenceToIdeal(matrix(c(1.0, 0.85, 0.42, 0, 0.5, 0, 1, 0.7), 4, 2), attr=1:4)
#' weights <- differenceToIdeal(matrix(c(1.0, 0.85, 0.42, 0, 0.5, 0, 1, 0.7), 2, 4), attr=1:4)
#' sum(weights) should return 1
#'
#'
#' @references [1]Ma, J., Fan, Z. P., & Huang, L. H. (1999). A subjective and
#'   objective integrated approach to determine attribute weights. European
#'   journal of operational research, 112(2), 397-404.
#'
#'   [2] Fan, Z. P. (1996). Complicated multiple attribute decision making:
#'   theory and applications (Doctoral dissertation, Ph. D. Dissertation,
#'   North-eastern University, Shenyang, PRC).
#' @export

differenceToIdeal <- function(normalizedMatrix, attr) {
  vector2_0 <- apply(normalizedMatrix, 2, function(t) { b_max <- max(t); sumOfDiff <- (sum((b_max - t)^2))  })
  vector2_0 <- replace(vector2_0, vector2_0==0.0, 1/(length(attr)/2)) # Avoid divide by zero, the 2 regulates the huge impact a none-changing value has on the weights
  vector2_1 <- 1/vector2_0
  vector3 <- sum(vector2_1)
  vector3 <- replace(vector3, vector3==0.0, 1)
  weightVector <- vector2_1/vector3
  weightVector
}

#' Calculates decision weights using the entropy method[1]
#'
#' @param dataset
#' @param userid
#' @param attr
#' @param rounds
#' @param cost_ids
#'
#' @return
#' @export
#'
#' @examples

weight.entropy <- function(dataset, userid = NULL , attr = NULL, rounds = "all", cost_ids = NULL) {

  # Common Errors catched in dM (Tested: attr, rounds, all.users, cost_ids)
  if (is.null(attr)) attr <- get_attrs_ID(dataset)
  if (!all(cost_ids %in% attr)) warning("One of your cost_ids is not in your attributes. Is this still your intended result?")

  decisionList <- decisionMatrix(dataset, userid, attr = NULL, rounds)
  normList <- lapply(decisionList, normalize.altMethod, attr, cost_ids)
  weightList <- lapply(normList, entropy)
  weightList <- lapply(weightList, function(t) t[attr])
  weightList
}
# Reference the book!
entropy <- function(normalizedMatrix) {
  #One column is unlikely since weight should be 1, one row is likely, catch row
  if(!is.matrix(normalizedMatrix)) stop("Input must be a matrix")
  if (nrow(normalizedMatrix)==1) {
    numberCol <- ncol(normalizedMatrix)
    weightVector <- rep(1, numberCol)
    weightVector <- weightVector/numberCol
  }
  else {
    normalizedMatrix <- replace(normalizedMatrix, normalizedMatrix==0.0, 1) #As in paper, log 0 should be equal 0 which is = log(1)
    kk <- 1/(log(nrow(normalizedMatrix)))

    e_j_secondTerm <- apply(normalizedMatrix, 2, function(x) sum(x*log(x)))
    e_j <- -kk*e_j_secondTerm
    d_j <- 1-e_j
    sumOfd_j <- sum(d_j)
    weightVector <- d_j/sumOfd_j
  }
  weightVector
}
