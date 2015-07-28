#' Calcultes the overall prospect values
#'
#' For the given number of attributes \code{attr} (number of columns) and the given number of \code{rounds} (number of rows).The process works
#' in 3 main steps. (1) It calculates the normalized gain and loss matrices (2) with both matrices, the value matrix is the calculated and finally
#' (3) the prospect value for each alternative/round/row.
#'
#' @param dataset data.frame with the user generated data from a product configurator. See Details
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
#' @param weight numeric, represents the importance or relative relevance of each attribute.
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
#' This function is for one user or one userid for \strong{more users} and for more detailed \strong{parameter information} please
#' see \code{\link{powerful_function}}
#'
#' The 3 step calculation of the prospect values comes from one specific paper \italics{Reference[1]}. (1) For the noramlized gain and loss matrices
#' we use the function \code{\link{norm_g_l_matrices}} from this package. (2) The value matrix is calculated with a series of auxiliary
#' functions. (3) The prospect value works with a simple additive weighting method from \code{overall_pv_extend}.
#'
#' If you only have the normalized gain and loss matrices you can use first \code{\link{prospect_value_matrix_extend}} with parameters (norm_gain, norm_loss)
#' and that function returns a value matrix which you then can give to \code\link{overall_pv_extend} together with your desired weights to
#' get the prospect values for each alternative.
#'
#' \code{dataset} We assume the input data.frame has following columns usid = User IDs, round = integers indicating which round the user is in
#' (0-index works best for 'round'), atid = integer column for referring the attribute ID (1 indexed), selected = numeric value of the attribute for a specific, given round,
#' selectable = amount of options the user can chose at a given round, with the current configuration. This is a necessary parameter.
#'
#' \code{userid} is a necessary parameter.
#'
#' \code{weight} default orders each attribute a weight <= 1 according to the relative frequency with which the user interacted with that specific attribute. Ideally
#' the sum of all weights equals 1. ##Ignore-Bug: What happens if you give three attributes but enter 4 or more weights or vice versa?
#'
#' \code{alpha} Default value as given by Reference [1] is 0.88
#'
#' \code{beta} Default value as given by Reference [1] is 0.88
#'
#' \code{lambda} Default value as given by Reference [1] is 2.25
#'
#' @return overall prospect values for each attribute
#' @examples
#' overall_pv(data_camera, 2)
#' overall_pv(data_pc, userid = 1, weight=c(0.1,0.4,0.3,0.2))
#' overall_pv(full_data, 6 ,attr = c(1,2,3), rounds="all", alpha = 0.95, beta = 0.78)
#'
#' @family prospect value functions
#'
#' @export
#'
overall_pv <- function (dataset, userid = NULL, attr = NULL, rounds = NULL, refps = NULL, cost_ids = NULL,  weight = NULL,
                        alpha = 0.88, beta = 0.88, lambda = 2.25) {

  v_matrix <- pvalue_matrix(dataset, userid, attr, rounds, refps, cost_ids, alpha, beta, lambda)

  overall_pv <- overall_pv_extend(v_matrix, weight, dataset)

  overall_pv

}

#' Calcultes the Value Matrix
#'
#' According to the parameters, it first calculates the normalized gain and loss matrices. Using \code{\link{prospect_value_matrix_extend}} it
#' calculates the value matrix using the value function given by Tversky & Kahnemann(1992)[1].
#'
#' @inheritParams overall_pv
#'
#' @details
#' This function is for one user or one userid for \strong{more users} and for more detailed \strong{parameter information} please
#' see \code{\link{powerful_function}}
#'
#' \code{dataset} We assume the input data.frame has following columns usid = User IDs, round = integers indicating which round the user is in
#' (0-index works best for 'round'), atid = integer column for referring the attribute ID (1 indexed), selected = numeric value of the attribute for a specific, given round,
#' selectable = amount of options the user can chose at a given round, with the current configuration. This is a necessary parameter.
#'
#' \code{userid} is a necessary parameter.
#'
#' @return the value matrix
#'
#' @seealso \code{\link{powerful_function}}
#'
#' @examples
#' pvalue_matrix(data_camera, 2,)
#' pvalue_matrix(data_pc, 100, weight=c(0.1,0.4,0.3,0.2))
#' pvalue_matrix(full_data, userid = 25 ,alpha = 0.95, beta = 0.78)
#'
#' @family prospect value functions
#'
#' @export
#'
pvalue_matrix <- function(dataset, userid = NULL, attr = NULL, rounds = NULL, refps = NULL, cost_ids = NULL,
                          alpha = 0.88, beta = 0.88, lambda = 2.25) {
  ngain <- norm_g_l_matrices(dataset, userid, attr, rounds, refps, cost_ids)$ngain
  nloss <- norm_g_l_matrices(dataset, userid, attr, rounds, refps, cost_ids)$nloss

  v_matrix <- prospect_value_matrix_extend(ngain, nloss, alpha, beta, lambda)
  v_matrix

}

##TODO y pv_matrix by original data and then usable on powerful_function

#' Calculate Value Matrix
#'
#' Parting from entered normalized gain and loss matrices, this function calculates the value matrix with the value function from Prospect Theory
#' (Reference[1]). It differs from other functions in this package in that it does not take all parameters into account. Yo need to
#' pre-calculate the matrices. See Details.
#'
#' @param ngain normalized gain matrix
#'
#' @param nloss normalized loss matrix
#'
#' @param alpha numeric between [0, 1]. Determines the concativity of the value function as given by the value function[1].
#'
#' @param beta numeric between [0, 1]. Determines the convexity of the value function as given by the value function[1]
#'
#' @param lambda lambda > 1. Parameter of loss aversion for the value function as given by the value function[1].
#'
#' @details
#' You need to pre-calculate the normalized gain and loss matrices, for example with \code{\link{norm_g_l_matrices}} and give them as a parameter.
#' This is one of the few functions of this package that do not allow you to give the raw data from your product Configurator, but rather calculate
#' a previous result.
#'
#' In normal cases we recommend the use of \code{\link{pvalue_matrix}} and \code{\link{powerful_function}}for more users at once.
#' This is mainly an auxiliary function.
#'
#' @return the value matrix
#'
#' @seealso \code{\link{pvalue_matrix}}
#'
#' @examples
#' prospect_value_matrix_extend(my_ngain, my_nloss)
#' prospect_value_matrix_extend(ngain = matrix1, nloss = matrix2, alpha = 0.95)
#'
#' \dontrun{prospect_value_matrix_extend(my_full_data)}
#'
#' @family prospect value functions
#'
#' @export
prospect_value_matrix_extend <- function(ngain = NULL, nloss = NULL, alpha = 0.88, beta = 0.88, lambda = 2.25)  {
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

##Auxiliary function, not necessary to document right now.
pvalue_fun <- function(ngain_ij, nloss_ij, alpha = 0.88, beta = 0.88, lambda = 2.25) {
  result <- ((ngain_ij)^alpha) + (-lambda*((-nloss_ij)^beta))
  result

}


#' Calculate Prospect Values
#'
#' This function works with the simple additive weighting method. It takes a value matrix and the weight of each attribute
#' and calculates the prospect value for each attribute (column-wise).
#'
#' @param value_matrix numeric matrix, results from using the value function on previously calculated normalized gain and loss matrices.
#'
#' @param weight numeric, represents the importance or relative relevance of each attribute.
#'
#' @param dataset data.frame containing your raw data from the product configurator.
#'
#' @details
#' You need to pre-calculate the value matrix, for example with \code{\link{pvalue_matrix}} and give it as a parameter. This is one of the few functions
#' of this package that do not allow you to give the raw data from your product Configurator, but rather calculate a previous result(value matrix) to
#' input here.
#'
#' In normal cases we recommend the use of \code{\link{overall_pv}} and \code{\link{powerful_function}}for more users at once.
#' This is mainly an auxiliary function.
#'
#' \code{value_matrix} ncol = number of attributes, nrow = number of rounds.
#'
#' \code{weight} default orders each attribute a weight <= 1 according to the relative frequency with which the user interacted with that specific attribute. Ideally
#' the sum of all weights equals 1. ##Ignore-Bug: What happens if you give three attributes but enter 4 or more weights or vice versa?
#'
#' \code{dataset} For the ideal form of this data.frame please refer to \code{\link{powerful_function}} 's Details.
#'
#' @return prospect values for each attribute
#'
#' @seealso \code{\link{overall_pv}}
#'
#' @examples
#' overall_pv_extend(my_value_matrix, weight = c(0.1, 0.2, 0.4, 0.3))
#' overall_pv_extend(my_matrix, weight=c(0.8,0.05,0.05,0.1))
#' overall_pv_extend(vmatrix, dataset =  full_data_example)
#'
#' \dontrun{overall_pv_extend(value_mx)}  # Always provide weights or dataset.
#'
#' @family prospect value functions
#'
#' @export
overall_pv_extend <- function(value_matrix, weight = NULL, dataset = NULL) {
  if(is.null(weight) & is.null(dataset)) {
    stop("Unable to get weights. You need to enter the weights or provide the dataset for us to calculate them. ")
  }
  if(is.null(weight & !is.null(dataset))) {
    result <-get_attr_weight(dataset)
  }
  else {
    if(length(weight) != ncol(value_matrix)) {
      warning("Length of weight does not equal amount of attributes, some recycling may have been done here.")
    }
    result <- apply(value_matrix, 1, function(my_vector) { sum(my_vector*weight)})
  }
  result
}




