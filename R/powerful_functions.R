#' Calculate the Gain matrix
#'
#' Creates the Gains matrix parting from a decision matrix and a vector containing the reference points (aspiration-levels).
#'
#' (NOT CHANGED)Creates a decision matrix, which is necessary to create the 'Gains' and 'Losses' matrices after. If no attribute vector is given,
#' (NOT CHANGED)containing the attributes IDs and if no alternatives are passed on as rounds, to be considered in the decision matrix, the function
#' (NOT CHANGED)will go with the defaults and extract them from the dataset. Will be used only for one user (one userid).
#'
#' @param dataset
#'
#' @param userid
#'
#' @param attr
#'
#' @param rounds
#'
#' @return (NOT CHANGED)A decision matrix with j amount of columns and i amount of rows. Colnames = attrIDs and rownames = chosen rounds.\code{user_id}.
#' @examples
#' decision_matrix(camera2_config_data, 11)
#' decision_matrix(my_data, userid = 11, attr = c(1,3,5))
#' decision_matrix(another_data, userid = 80, rounds = c(1,2,3,7,8,9))
#' decision_matrix(data2, 2, rounds = "all")
#' @export

powerful_function <- function(dataset, userid = NULL, FUN = decision_matrix, attr = NULL, rounds = NULL, refps = NULL, cost_ids = NULL,
                              weight = NULL, alpha = 0.88, beta = 0.88, lambda = 2.25, gainm = TRUE) {
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


