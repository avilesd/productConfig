#' Takes one function and runs it for more than one user
#'
#' Provides a way to run a function \code{FUN} for more users at once.Almost all other functions in this package are built so that you get the data you need for
#' one specific user only at a time. The powerful_functionallows you to run almost all other functions in this package with the same parameters you are used to
#' use, but for more users at once. The difference being that for \code{userid} you can now enter just one ID or a vector of IDs contained in your data.
#'
#' If you enter more parameters that a given function needs, the function will just take the parameters it needs and ignore the extra ones.
#' Important is that you understand, the parameters of the function are passed on to other functions of the package, so the following description of
#' parameters applies to the functions you are going to input in the \code{FUN} parameter, rather than to the powerful_function itself. You can imagine
#' this function as being a loop-function that takes any other function and runs it for more \code{userid}s than just one.
#'
#' @param dataset data.frame with the user generated data from a product configurator. See Details
#'  for the specifications of the data.frame.
#'
#' @param userid an integer that gives the information of which user you want the data from: User ID.
#'
#' @param FUN Name of the function within the package you want to use. Input without "".
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
#' \code{dataset} We assume the input data.frame has following columns usid = User IDs, round = integers indicating which round the user is in
#' (0-index works best for 'round'), atid = integer column for referring the attribute ID (1 indexed), selected = numeric value of the attribute for a specific, given round,
#' selectable = amount of options the user can chose at a given round, with the current configuration. This is a necessary parameter.
#'
#' \code{userid} is a necessary parameter, without it you'll get a warning. Default is NULL.
#'
#' \code{FUN} Default value is "decision_matrix". Some functions you can try \code{overall_pv gain_loss_matrices gain_matrix loss_matrix norm_g_l_matrices
#' decision_matrix get_attrs_ID get_table_by_ID get_rounds_by_ID ref_points}
#'
#' \code{rounds} Default calculates first round(initia product config) and last round of the session.
#' Default calculates with first and last attributes (initial and final product configuration). To choose all give "all" as argument
#' for rounds, see example. "first" and "last" are also possible argument values. You can give a vector of arbitrarily chosen rounds as well.
#'
#' \code{refps} If you only want to see the results for one attribute you may enter only a couple of reference points
#' but you have to tell the function which attributes you want to use those referene points for. So the amount of attr and of refps should be the same.
#' Moreover the functions always orders de attr, so be sure to input the reference point also in an ascending order corresponding to their attributes. (refps
#' will not be ordered)
#'
#' \code{cost_ids} Default assumes all your attributes are of benefit type, that is a higher value in the attribute means the user
#' is better of than with a lower value. If one or more of the attributes in your data is of cost type, e.g. price, so that lower is better then you should identify
#' this attributes as such, providing their id, they'll be converted to benefit type (higher amount is better).
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

powerful_function <- function(dataset, userid = NULL, FUN = decision_matrix, attr = NULL, rounds = NULL, refps = NULL, cost_ids = NULL,
                              weight = NULL, alpha = 0.88, beta = 0.88, lambda = 2.25, gainm = TRUE, result_type = NULL) {
  if(!is.null(userid)) {
    result_list <- list()

    for(i in userid) {

      usid <- paste("usid", i, sep = "")

      ##Create conditions
      con1 <- (identical(FUN, get_attrs_ID) | identical(FUN, get_all_userids))
      con2 <- (identical(FUN, get_table_by_ID) | identical(FUN, get_rounds_by_ID) | identical(FUN, get_all_default_rps)
               | identical(FUN, get_attr_values))
      con3 <- identical(FUN, decision_matrix)
      con4 <- identical(FUN, ref_points)
      con5 <- (identical(FUN, gain_matrix) | identical(FUN, loss_matrix) | identical(FUN, norm_g_l_matrices))
      con6 <- identical(FUN, gain_loss_matrices)
      con7 <- identical(FUN, pvalue_matrix)
      con8 <- identical(FUN, overall_pv)
      con9 <- identical(FUN, diego_pv)
      con10 <- identical(FUN, weight_higher_sum_value)
      con11 <- identical(FUN, get_attr_weight)

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
      else if(con10) {
        result_list[[usid]] <- FUN(dataset, userid = i, rounds, cost_ids)
      }
      else if(con11) {
        result_list[[usid]] <- FUN(dataset, userid = i, weight, attr, rounds, cost_ids)
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


