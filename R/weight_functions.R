## See if it does something useful or delete

weight_overall_freq <- function(dataset, userid = NULL , attr =NULL, rounds = NULL, refps =NULL, cost_ids =NULL) {
  all_dec_matrices <- powerful_function(dataset, userid, FUN=decision_matrix, attr, rounds, refps, cost_ids)

  if(is.null(attr)) {
    length_attr <- length(get_attrs_ID(dataset))
  }
  else {
    length_attr <- length(attr)
  }
  sum_help <- rep.int(0, length_attr)

  for(i in all_dec_matrices) {

    help <- apply(i, 2, sum)
    sum_help <- sum_help + help
  }
  almost <- sum_help * c(1,1,1,10.5147)
  result <- almost/sum(almost)
  result

}

#' To be use for weights.
#'
#' Description
#'
#' @param rel_frequeny logical. Relative frequency, if \code{rel_frequency = TRUE}, the function ignores the refps argument
#'        and calculates the aspiration level for each attribute as the relative frequency the user made for that attribute.
#'
#' ###to consider, defaults considers all your attributes in your table and calculates with relative frequency of the attributes
#' if no other weights are given. ##no userid because weights independent from user
#'
get_attr_weight <- function(dataset) {
  if(is.null(dataset) ) {
    refps <- get_all_default_rps(dataset, userid)
    ##TODO calculate relative frequency, see annotations notebook
  }
  ## TODO proove if length(w) =length(attributes), proof if numeric and the sum of all = 1!!!
  else{
    weight
  }
}
