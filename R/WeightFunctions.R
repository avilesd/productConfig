## Idea is that all the other functions for calculating weights are 'external' and they go through get_attr_weight
## Add weight thing or in other function?!?!A priori thinking here..-
## Frequency based on the value level, the higher the historical value, the more important the attribute.
## Improvement since right now only for 4 attributes.
## ! No matter the order of the attribute, data will be always correspondent to its attribute.
## Idea get_attr_values and check lower and higher value, based on that calculate the special K factor.
## Attr remove since it will be handled at the get_attr_weight LEVEL

#' Weights based on the sum of attribute values
#'
#' This weight function sums all attribute values for all available rounds. The
#' higher the sum for one attribute, the better it will be weighted. After
#' getting the sum for each attribute, the results will be normalized resulting
#' in all weights being smaller than 1 and the sum of all weights equaling 1.
#' Typically used within this package to calculate weights when the user has not
#' given any in \code{\link{get_attr_weight}}
#'
#' @inheritParams powerful_function
#'
#' @param rounds integer vector. Does not play a role at this moment (<=
#'   0.2.0.9000) in this function, but it does for future improvements.
#'
#' @details This function does not handle different amount of attributes, it
#'   always calculates weights for all attr. The filtering according to the
#'   user's input happens in \code{\link{get_attr_weight}}
#'
#'   One problem of this function as it is (<= 0.2.0.9000) is that it works
#'   mostly assuming that the attribute values across all attributes have the
#'   same maximum and minimum value, so that a comparison of the sum of each
#'   attribute values makes sense. In the data we worked building this package,
#'   there is already a problem with this since three attributes work with
#'   {0,1,2,3} values and a forth works with values between [0,1]. For our
#'   package we found a solution, but it is necessary to implement a more
#'   general solution.
#'
#'   \code{cost_ids} It handles it well, by adding all together and treating it
#'   as cost and then just using abs() to get the positive value. Default
#'   assumes all your attributes are of benefit type, that is a higher value in
#'   the attribute means the user is better of than with a lower value. If one
#'   or more of the attributes in your data is of cost type, e.g. price, so that
#'   lower is better then you should identify this attributes as such, providing
#'   their id, they'll be converted to benefit type (higher amount is better).
#'
#'   If you want to know more about the other parameters, e.g. \code{dataset},
#'   look at \code{powerful_function}.
#'
#' @return Relative weights for all attributes
#'
#' @examples
#' weight_higher_sum_value(pc_data, 11)
#' weight_higher_sum_value(my_data, userid = 1000, cost_ids = 4)
#' weight_higher_sum_value(monitor_data, 50)
#' weight_higher_sum_value(data1, 8, cost_ids = c(1,3))
#'
#' @family weight functions
#' @export

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

## TODO calculate relative frequency, see annotations notebook: As separate
## function. to consider, defaults considers all your attributes in your table
## and calculates with relative frequency of the attributes if no other weights
## are given. ##no userid because weights independent from user Probably change
## method parameter to weight. Attr handling here not in
## functions!!!!!!!!!!

#' Weights calculating function
#'
#' This function gives you an interface to all weight calculating functions in this package.
#'
#' @inheritParams powerful_function
#'
#' @param weight integer vector. Represents the importance or relevance that an attribute has and the weight it should have in the
#' calculation of the prospect value.
#'
#' @param rounds integer vector. Which steps of the configuration process should be taken into account?
#'
#' @details Default value of FUN uses \code{\link{weight_higher_sum_value}} to get the \code{weight} vector. Below you can
#' find alternative functions implemented until now to calculate weights.
#'
#' \code{attr} This function does handles different amount of attributes.
#'
#' \code{cost_ids} Are passed along to the function you chosee (See Also), not handled here.
#'
#' If you want to know more about the other parameters, look at \code{powerful_function}.
#'
#' @return Calculated weights according to the chosen weight_function
#'
#' @examples
#' get_attr_weight(pc_data, 11)
#' get_attr_weight(my_data, userid = 1000, cost_ids = 4)
#' get_attr_weight(monitor_data, 50)
#'
#' @family weight functions
#'
#' @export
#'

get_attr_weight <- function(dataset = NULL, userid = NULL, weight = NULL,  attr = NULL, rounds = NULL, cost_ids = NULL) {

  if(is.null(attr)) attr <- get_attrs_ID(dataset) #Step 1 to handle attr

  if(is.null(weight) & is.null(userid)) {
    stop("You need to provide one user id OR enter your own weights")
  }
  if(is.null(weight)) {
    result <- weight_higher_sum_value(dataset, userid, rounds, cost_ids)
  }
  ## TODO proove if length(w) =length(attributes), proof if numeric and the sum of all = 1 or handle with result[attr]!!!!!!!!
  else{
    result <- weight
  }
  result <- result[attr] # Step 2 to handle attr
  result
}
