## See if it does something useful or delete
## Add weight thing or in other function?!?!A priori thinking here..-
##Frequency based on the value level, the higher the historical value, the more important the attribute.
##Improvement since right now only for 4 attributes.
## ! No matter the order of the attribute, data will be always correspondent to its attribute.
## Idea get_attr_values and check lower and higher value, based on that calculate the special K factor.

#' Weights based on the sum of attribute values
#'
#' This weight function sums all attribute values for all available rounds. The higher the sum for one attribute, the better it will be weighted.
#' After getting the sum for each attribute, the results will be normalized resulting in all weights being smaller than 1 and the sum of all weights
#' equaling 1.
#'
#' @inheritParams powerful_function
#'
#' @details One problem of this function as it is (<= 0.2.0.9000) is that it works mostly assuming that the attrbute values
#' across all attributes have the same maximum and minimum value, so that a comparison of the sum of each attribute values makes sense.
#' In the data we worked building this package, there is already a problem with this since three attributes work with {0,1,2,3} values
#' and a forth works with values between [0,1]. For our package we found a solution, but it is necessary to implement a more general
#' solution.
#'
#' \code{cost_ids} It handles it well, by adding all together and treating it as cost and then just using abs() to get the positive value.
#'
#' If you want to know more about the other parameters, look at \code{gain_matrix loss_matrix decision_matrix}.
#'
#'
#' @return a
#'
#' @examples
#' norm_g_l_matrices(pc_config_data, 11)
#' norm_g_l_matrices(my_data, userid = 11, result_type = "cbind")
#' norm_g_l_matrices(monitor_data, 50, rounds = "last", refps = c(0.1,0.3,0.4,0.5), cost_ids = 3)
#' norm_g_l_matrices(data1, 8, attr = 1)
#'
#' @export

weight_higher_sum_value <- function(dataset, userid = NULL , attr = NULL, rounds = NULL, cost_ids = NULL) {

  ##Calculate with always 4 attribute so that function works properly, take attr into account at result level.
  all_dec_matrices <- powerful_function(dataset, userid, FUN = decision_matrix, attr = NULL, rounds = "all", refps = NULL, cost_ids,
                                        weight = NULL, alpha = 0.88, beta = 0.88, lambda = 2.25, gainm = TRUE, result_type = NULL)

  if(length(userid) != 1) stop("Please enter only one userid, for more see powerful_function.")

  if(is.null(attr)) attr <- get_attrs_ID(dataset)

  length_attr <- length(get_attrs_ID(dataset))

  sum_help <- rep.int(0, length_attr)

  for(i in all_dec_matrices) {

    help <- apply(i, 2, sum)
    sum_help <- sum_help + help
    sum_help <- abs(sum_help)
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
    result <- weight_higher_sum_value(dataset, userid, attr, rounds, cost_ids)
  }
  ## TODO proove if length(w) =length(attributes), proof if numeric and the sum of all = 1 or handle with result[attr]!!!!!!!!
  else{
    result <- weight
  }
  result
}
