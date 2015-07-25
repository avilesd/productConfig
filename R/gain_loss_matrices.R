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

gain_matrix <- function(data, userid = NULL, attr = NULL, rounds = NULL, refps = NULL, cost_ids = NULL) {
  ## Calculate decision matrix based on inputs
  des_matrix <- decision_matrix(data, userid, attr, rounds, cost_ids)


  ## Get reference points(aspiration-levels) from input
  refps_vector <-ref_points(data, userid, refps, attr, cost_ids)

  ## Create empty result Gain matrix
  dim_matrix <- dim(des_matrix)
  gain_matrix <- matrix(2, dim_matrix[1],dim_matrix[2])

  ## TODO: 1. name the matrix
  m <- 1
  ## Fill the matrix
  for(n in 1:dim_matrix[1]) {
    gain_matrix[m, ] <- mapply(gain_fun_a, des_matrix[m, ], refps_vector)
    m <- m + 1
  }
  gain_matrix


}

#' Calculate the Loss matrix
#'
#' Creates the Loss matrix parting from a decision matrix and a vector containing the reference points (aspiration-levels).
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

loss_matrix <- function(data, userid = NULL, attr = NULL, rounds = NULL, refps = NULL, cost_ids = NULL) {
  ## Calculate decision matrix based on inputs
  des_matrix <- decision_matrix(data, userid, attr, rounds, cost_ids)


  ## Get reference points(aspiration-levels) from input
  refps_vector <-ref_points(data, userid, refps, attr, cost_ids)

  ## Create empty result Gain matrix
  dim_matrix <- dim(des_matrix)
  loss_matrix <- matrix(2, dim_matrix[1],dim_matrix[2])

  ## TODO: 1. name the matrix
  m <- 1
  ## Fill the matrix
  for(n in 1:dim_matrix[1]) {
    loss_matrix[m, ] <- mapply(loss_fun_a, des_matrix[m, ], refps_vector)
    m <- m + 1
  }
  loss_matrix


}

## Get both gain and loss matrix together
gain_loss_matrices <- function(data, userid = NULL, attr = NULL, rounds = NULL, refps = NULL, result_type = NULL, cost_ids = NULL) {

  g_matrix <- gain_matrix(data, userid, attr, rounds, refps, cost_ids)
  l_matrix <- loss_matrix(data, userid, attr, rounds, refps, cost_ids)

  ##Depending on result_type return matrices accordingly
  if(is.null(result_type)){
    result <- list(gain = g_matrix, loss = l_matrix)
  }
  else if(result_type == "rbind") {
    result <- rbind(g_matrix, l_matrix)
  }
  else if(result_type == "cbind") {
    result <- cbind(g_matrix, l_matrix)
  }
  result

}

## Normalized matrices
norm_g_l_matrices <- function(data, userid = NULL, attr = NULL, rounds = NULL, refps = NULL, cost_ids = NULL) {
  g_l_matrix <- gain_loss_matrices(data, userid, attr, rounds, refps, result_type="rbind", cost_ids)

  n_gain <- gain_matrix(data, userid, attr, rounds, refps, cost_ids)
  n_loss <- loss_matrix(data, userid, attr, rounds, refps, cost_ids)

  hmax_vector <- numeric(0)
  number_col <-  dim(g_l_matrix)[2]

  for(n in 1:number_col) {
    hmax_vector <- c(hmax_vector, max(abs(g_l_matrix[,n])))
    if(hmax_vector[n] == 0) {

    }
    else {
      n_gain[,n] <- n_gain[,n]/hmax_vector[n]
      n_loss[,n] <- n_loss[,n]/hmax_vector[n]
    }
  }
  result <- list(ngain = n_gain, nloss = n_loss)
  result
}


## Gain function
gain_fun_a <- function(s_ij, e_j) {
  if(s_ij >= e_j) {
    gain <- s_ij - e_j
  }
  else {
    gain <- 0
  }
  gain
}
## Loss function
loss_fun_a <- function(s_ij, e_j) {
  if(s_ij >= e_j) {
    loss <- 0
  }
  else {
    loss <- s_ij - e_j
  }
  loss
}
