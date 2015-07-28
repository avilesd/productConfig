#' Takes
#'
#' Pro
#'
#' @param data data.frame with the user generated data from a product configurator. See Details
#'  for the specifications of the data.frame.
#'
#' @param userid an integer that gives the information of which user you want the data from: User ID.
#'
#' @param attr attributes IDs, vector of integer numbers corresponding to the attributes you desire to use;1-indexed.
#'
#' @param rounds integer vector. Which steps of the configuration process should be shown? See Details.
#'
#' @param refps numeric vector. Reference Points: each point corresponds to one attribute, i.e. each attribute has only one
#' aspiration level.
#'
#' @param cost_ids argument used to convert selected cost attributes into benefit attributes. Integer vector.
#'
#' @param weight orders each attribute a weight <= 1 for the value functions to assign a value to each alternative or in the case of productConfig, to each round.
#'
#' @param alpha numeric between [0, 1]. Determines the concativity of the value function as given by Reference[1].
#'
#' @param beta numeric between [0, 1]. Determines the convexity of the value function as given by Reference[1].
#'
#' @param lambda lambda > 1. Parameter of loss aversion for the value function as given by Reference[1].
#'
#' @seealso \code{\link{powerful_function}}
#'
#' @details
#' If you need more detailed information about the parameters please see \code{\link{pvalue_matrix}}
#'
#' \code{data} We assume the input data.frame has following columns usid = User IDs, round = integers indicating which round the user is in
#' (0-index works best for 'round'), atid = integer column for referring the attribute ID (1 indexed), selected = numeric value of the attribute for a specific, given round,
#' selectable = amount of options the user can chose at a given round, with the current configuration. This is a necessary parameter.
#'
#' \code{userid} is a necessary parameter, without it you'll get a warning. Default is NULL.
#'
#' \code{weight}
#'
#' \code{alpha} Default value as given by Reference [1] is 0.88
#'
#' \code{beta} Default value as given by Reference [1] is 0.88
#'
#' \code{lambda} Default value as given by Reference [1] is 2.25
#'
#' @return The same value that returns the function you entered in \code{FUN} for the users you entered in the \code{userid} parameter.
#' @examples
#' powerful_function(data_camera, 2)  # FUN=decision_matrix
#' powerful_function(data_pc, userid = c(1,2,4,8,9,100))
#' powerful_function(my_table, get_all_userids(my_table))
#'
#' powerful_function(data_pc, userid = c(1,2,3), FUN = gain_matrix)
#' powerful_function(data_pc, userid = c(1,2,3), FUN = overall_pv, alpha = 0.9, weight=c(0.1,0.2,0.5,0.2))
#' powerful_function(datadata, c(6,9), FUN = ref_points) # Returns the default values for both users (user6, user9) assumed as reference points.
#'
#' @export
overall_pv <- function (dataset, userid = NULL, attr = NULL, rounds = NULL, refps = NULL, cost_ids = NULL,  weight = NULL,
                        alpha = 0.88, beta = 0.88, lambda = 2.25) {

  v_matrix <- pvalue_matrix(dataset, userid, attr, rounds, refps, cost_ids, alpha, beta, lambda)

  overall_pv <- overall_pv_extend(v_matrix, weight, dataset)

  overall_pv

}
##ready
pvalue_matrix <- function(dataset, userid = NULL, attr = NULL, rounds = NULL, refps = NULL, cost_ids = NULL,
                          alpha = 0.88, beta = 0.88, lambda = 2.25) {
  ngain <- norm_g_l_matrices(dataset, userid, attr, rounds, refps, cost_ids)$ngain
  nloss <- norm_g_l_matrices(dataset, userid, attr, rounds, refps, cost_ids)$nloss

  v_matrix <- prospect_value_matrix_extend(ngain, nloss, alpha, beta, lambda)
  v_matrix

}
##TODO y pv_matrix by original data and then usable on powerful_function
prospect_value_matrix_extend <- function(ngain = NULL, nloss = NULL,alpha = 0.88, beta = 0.88, lambda = 2.25)  {
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
##explain alpha, beta, lambda and digits = 2
pvalue_fun <- function(ngain_ij, nloss_ij, alpha = 0.88, beta = 0.88, lambda = 2.25) {
  result <- ((ngain_ij)^alpha) + (-lambda*((-nloss_ij)^beta))
  result

}



##TO THINK  value_matrix and/or dataset? If you want to weight with rel_frequency you need to provide the dataset
overall_pv_extend <- function(value_matrix, weight = NULL, wfrom_dataset) {
  if(is.null(weight)) {
    result <-get_attr_weight(wfrom_dataset)
  }
  else {
    if(length(weight) != ncol(value_matrix)) {
      warning("Length of weight does not equal amount of attributes, some recycling may have been done here.")
    }
    result <- apply(value_matrix, 1, function(my_vector) { sum(my_vector*weight)})
  }
  result
}




