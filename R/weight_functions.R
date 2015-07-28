## See if it does something useful or delete
## Add weight thing or in other function?!?!A priori thinking here..-
##Frequency based on the value level, the higher the historical value, the more important the attribute.
##Improvement since right now only for 4 attributes.
## ! No matter the order of the attribute, data will be always correspondent to its attribute.

weight_higher_avg_value <- function(dataset, userid = NULL , attr = NULL, rounds = NULL, cost_ids =NULL) {

  ##Calculate with always 4 attribute so that function works properly, take attr into account at result level.
  all_dec_matrices <- powerful_function(dataset, userid, FUN=decision_matrix, attr = NULL, rounds, cost_ids)

  if(length(userid) != 1) stop("Please enter only one userid, for more see powerful_function.")

  if(is.null(attr)) attr <- get_attrs_ID(dataset)

  length_attr <- length(get_attrs_ID(dataset))

  sum_help <- rep.int(0, length_attr)

  for(i in all_dec_matrices) {

    help <- apply(i, 2, sum)
    sum_help <- sum_help + help
  }

  almost <- sum_help * c(1,1,1,10.5147)
  result <- almost/sum(almost)
  result <- result[attr]
  result

}

##TODO calculate relative frequency, see annotations notebook: As separate function.

#' To be use for weights.
#'
#' Description
#'
#' @param rel_frequeny logical. Relative frequency, if \code{rel_frequency = TRUE}, the function ignores the refps argument
#'        and calculates the aspiration level for each attribute as the relative frequency the user made for that attribute.
#'
#' ###to consider, defaults considers all your attributes in your table and calculates with relative frequency of the attributes
#' if no other weights are given. ##no userid because weights independent from user
#' Probably change method parameter to weight.
#' Attr handling is in functions, not here!!!
#'
get_attr_weight <- function(dataset, userid = NULL, weight = NULL,  attr = NULL, rounds = NULL, cost_ids = NULL) {



  if(is.null(weight)) {
    result <- weight_higher_avg_value(dataset, userid, attr, rounds, cost_ids)
  }
  ## TODO proove if length(w) =length(attributes), proof if numeric and the sum of all = 1 or handle with result[attr]!!!!!!!!
  else{
    result <- weight
  }
  result
}
