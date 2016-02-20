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

#######

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
