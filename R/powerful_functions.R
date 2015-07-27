#' Takes one function and runs it for more than one user
#'
#' Provides a way to run a function \code{FUN} for more users at once.Almost all other functions in this package are built so that you get the data you need for
#' one specific user only at a time. The powerful_functionallows you to run almost all other functions in this package with the same parameters you are used to
#' use, but for more users at once. The difference being that for \code{userid} you can now enter just one ID or a vector of IDs contained in your data.
#'
#' Important is that you understand, the parameters of the function are passed on to other functions of the package, so the following description of
#' parameters applies to the functions you are going to input in the \code{FUN} parameter, rather than to the powerful_function itself. You can imagine
#' this function as being a loop-function that takes any other function and runs it for more \code{userid}s than just one.
#'
#' @param data data.frame with the user generated data from a product configurator. See Details
#'  for the specifications of the data.frame.
#'
#' @param userid an integer that gives the information of which user you want the data from: User ID.
#'
#' @param FUN character. Name of the function within the package you want to use.
#'
#' @param attr attributes IDs, vector of integer numbers corresponding to the attributes you desire to use; attr are assumed to be 1-indexed.
#'
#' @param rounds integer vector. Which steps of the configuration process should be shown? See Details.
#'
#' @param refps numeric vector. Reference Points: each point corresponds to one attribute, i.e. each attribute has only one
#' aspiration level. Default setting assumes the aspiration levels as the default values of the initial product configuration
#' for each user.
#'
#' @param cost_ids argument used to convert selected cost attributes into benefit attributes. Integer vector.
#'
#' @param weight orders each attribute a weight <= 1 for the value functions to assign a value to each alternative or in the case of productConfig, to each round.
#'
#' @param alpha numeric between [0, 1]. Determines the concativity of the value function as given by Reference [1].
#'
#' @param beta numeric between [0, 1]. Determines the convexity of the value function as given by Reference [1].
#'
#' @param lambda lambda > 1. Parameter of loss aversion for the value function as given by Reference [1].
#'
#' @param gainm loading...
#'
#' @param result_type allows to change the result type. Default returns a list with two elements. Other possibilites are "cbind" and "rbind" as character input which
#' do exactly what their name suggests; return a column- or row- binded matrix. Only for \code{gain_loss_matrices} function.
#'
#' @details
#' \code{data} We assume the input data.frame has following columns usid = User IDs, round = integers indicating which round the user is in
#' (0-index works best for 'round'), atid = integer column for referring the attribute ID (1 indexed), selected = numeric value of the attribute for a specific, given round,
#' selectable = amount of options the user can chose at a given round, with the current configuration. This is a necessary parameter.
#'
#' \code{userid}
#'
#' \code{FUN} Default value is "decision_matrix".
#'
#' \code{rounds} Default calculates first round(initia product config) and last round of the session.
#' Default calculates with first and last attributes (initial and final product configuration). To choose all give "all" as argument
#' for rounds, see example. "first" and "last" are also possible argument values. You can give a vector of arbitrarily chosen rounds as well.
#'
#' \code{weight} dfedf
#'
#' \code{alpha} Default value as given by Reference [1] is 0.88
#'
#' \code{beta} Default value as given by Reference [1] is 0.88
#'
#' \code{lambda} Default value as given by Reference [1] is 2.25
#'
#' @return The same value that returns the function you entered in \code{FUN} for the users you entered in the \code{userid} parameter.
#' @examples
#' powerful_function(data_camera, 2)
#'
#' @export

powerful_function <- function(dataset, userid = NULL, FUN = decision_matrix, attr = NULL, rounds = NULL, refps = NULL, cost_ids = NULL,
                              weight = NULL, alpha = 0.88, beta = 0.88, lambda = 2.25, gainm = TRUE, result_type = NULL) {
  if(!is.null(userid)) {
    result_list <- list()

    for(i in userid) {

      usid <- paste("usid", i, sep = "")

      ##Create conditions
      con1 <- (identical(FUN, get_attrs_ID) | identical(FUN, get_all_userids))
      con2 <- (identical(FUN, get_table_by_ID) | identical(FUN, get_rounds_by_ID) | identical(FUN, get_all_default_rps)
               | identical(FUN, read_attr_levels))
      con3 <- identical(FUN, decision_matrix)
      con4 <- identical(FUN, ref_points)
      con5 <- (identical(FUN, gain_matrix) | identical(FUN, loss_matrix) | identical(FUN, norm_g_l_matrices))
      con6 <- identical(FUN, gain_loss_matrices)
      con7 <- identical(FUN, pvalue_matrix)
      con8 <- identical(FUN, overall_pv)
      con9 <- identical(FUN, diego_pv)

      if(con1) {
        result_list[[usid]] <- FUN(dataset)
      }
      else if(con2) {
        result_list[[usid]] <- FUN(dataset, userid = i)
      }
      else if(con3) {
        result_list[[usid]] <- FUN(dataset, userid = i, attr, rounds, cost_ids)
      }
      else if(con4) {
        result_list[[usid]] <- FUN(dataset, userid = i, attr, refps, cost_ids)
      }
      else if(con5) {
        result_list[[usid]] <- FUN(dataset, userid = i, attr, rounds, refps, cost_ids)
      }
      else if(con6) {
        result_list[[usid]] <- FUN(dataset, userid = i, attr, rounds, refps, cost_ids, result_type)
      }
      else if(con7) {
        result_list[[usid]] <- FUN(dataset, userid = i, attr, rounds, refps, cost_ids, alpha, beta, lambda)
      }
      else if(con8) {
        result_list[[usid]] <- FUN(dataset, userid = i, attr, rounds, refps, cost_ids, weight, alpha, beta, lambda)
      }
      else if(con9) {
        result_list[[usid]] <- FUN(dataset, userid = i, attr, rounds, refps, cost_ids, weight, gainm)
      }
      else {
        print("It appears the function (FUN) you gave is not to be use here, or not implemented yet.")
      }
    }
    result_list
  }
  else {
    stop("You need to provide at least 1 userid.")
  }

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

overall_pv <- function (dataset, userid = NULL, attr = NULL, rounds = NULL, refps = NULL, cost_ids = NULL,  weight = NULL,
                        alpha = 0.88, beta = 0.88, lambda = 2.25) {

  v_matrix <- pvalue_matrix(dataset, userid, attr, rounds, refps, cost_ids, alpha, beta, lambda)

  overall_pv <- overall_pv_extend(v_matrix, weight, dataset)

  overall_pv

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


