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
    if (weightFUN == "Test1") {
      result <- weight_higher_sum_value(dataset, userid, rounds, cost_ids)
    }
    if (weightFUN == "Test2") {
      result <- weight_higher_sum_value(dataset, userid, rounds, cost_ids)
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
##'
