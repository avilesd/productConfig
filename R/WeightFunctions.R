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

#' Title
#'
#' @param dataset
#' @param userid
#' @param attr
#' @param rounds Default is rounds to have more data to calculate with. If you
#'   enter 'first' or' last' there is only one rounds to gather data from so
#'   consequently all attribtues will have the same weight.
#' @param cost_ids
#'
#' @details This function rewards attributes which values do not change much
#'   throughtout the decision matrix, even if the value is the lowest value.For
#'   an opposite implicit effect see \code{\link{}}
#'
#'   \code{cost_ids} As in the other functions, if you enter a cost_ids that is
#'   not in your entered attributes, the functions will calculate the output
#'   with all attributes in your data, including the cost(s) attributes and only
#'   after the calculations does the function subset the result according to the
#'   \code{attr} input. When the attributes and cost_ids differ, the function
#'   allows the calculation but it will throw a warning.
#'
#' @return
#'
#' @examples
#'
#' @references [1]Ma, J., Fan, Z. P., & Huang, L. H. (1999). A subjective and
#'   objective integrated approach to determine attribute weights. European
#'   journal of operational research, 112(2), 397-404.
#'
#'   [2] Fan, Z. P. (1996). Complicated multiple attribute decision making:
#'   theory and applications (Doctoral dissertation, Ph. D. Dissertation,
#'   North-eastern University, Shenyang, PRC).
#' @export

weight.differenceToIdeal <- function(dataset, userid = NULL , attr = NULL, rounds = "all", cost_ids = NULL) {

  ##Calculate with always 4 attribute so that function works properly, take attr into account at result level. Good idea, since we
  ## cut with [, x] in normalize.altMethod
  # Common Errors catched in dM (Tested: attr, rounds, all.users, cost_ids)
  if (!all(cost_ids %in% attr)) warning("One of your cost_ids is not in your attributes. Is this still your intended result?")
  if (is.null(attr)) attr <- get_attrs_ID(dataset)

  decisionList <- decisionMatrix(dataset, userid, attr = NULL, rounds)
  normList <- lapply(decisionList, normalize.altMethod, attr, cost_ids)
  weightList <- lapply(normList, differenceToIdeal, attr)
  weightList <- lapply(weightList, function(t) t[attr])
  weightList
}

#' Title
#'
#' @param aMatrix if matrix is only one row, then according to method in [1], no
#'   change to ideal, so all weights will be given the same weight, that is, 1/length(attr)
#'
#' @param attr
#' @param cost_ids
#'
#' @return
#' @export
#'
#' @examples
normalize.altMethod <- function(aMatrix, attr, cost_ids) {
  if (nrow(aMatrix) == 1) {
    aMatrix <- apply(aMatrix, 1:2, function(t) 1 )
  }
  else{
    aMatrix[,cost_ids] <- apply(aMatrix[,cost_ids, drop = F], 2, function(t) { a_max <- max(t); a_min <- min(t);
    if (a_max == 0 & a_min == 0) {a_max <- 1; a_min <- 1}
    if (a_max ==  a_min) {res <- t/a_max}
    else {res <- (a_max-t)/(a_max-a_min)}; res})

    benefitAttr <- attr[!attr %in% cost_ids]
    aMatrix[,benefitAttr] <- apply(aMatrix[,benefitAttr, drop = F], 2, function(t) { a_max <- max(t); a_min <- min(t);
    if (a_max == 0 & a_min == 0) {a_max <- 1; a_min <- 1}
    if (a_max ==  a_min) {res <- t/a_max}
    else {res <- (t-a_min)/(a_max-a_min)}; res})
  }
  aMatrix
}

differenceToIdeal <- function(normalizedMatrix, attr) {
  vector2_0 <- apply(normalizedMatrix, 2, function(t) { b_max <- max(t); sumOfDiff <- (sum((b_max - t)^2))  })
  vector2_0 <- replace(vector2_0, vector2_0==0.0, 1/(length(attr)/2)) # Avoid divide by zero, the 2 regulates the huge impact a none-changing value has on the weights
  vector2_1 <- 1/vector2_0
  vector3 <- sum(vector2_1)
  vector3 <- replace(vector3, vector3==0.0, 1)
  weightVector <- vector2_1/vector3
  weightVector
}
