#' Calcultes the overall prospect values using PT (prospect theory)
#'
#' (1) It calculates the normalized gain and loss matrices, (2) with both
#' matrices the value matrix is then calculated and finally (3) the prospect
#' value for each alternative/round/row. From step (2) to (3) this function uses
#' prospect theory's (PT) value function[1,2] and a method proposed by [3]. Its
#' equivalent function for multi-reference point approaches are
#' \code{\link{overallDRP}} and \code{\link{overallTRP}}.
#'
#' In the context of data stemming from product configurators, the highest
#' overall value returned  by this function, means that specific product
#' configuration represented the highest value for the user. This depends mainly
#' on three important factors: (1) What theoretical framework you choose to use
#' (PT, DRP, TRP), (2) which decision weights you assign to each attribute, and
#' (3) the reference point(s) you input.
#'
#' @inheritParams gainMatrix
#' @inheritParams getAttrWeights
#'
#' @param alpha numeric between [0, 1]. Determines the concativity of the value
#'   function and has a default value of 0.88 as given by Reference[1].
#'
#' @param beta numeric between [0, 1]. Determines the convexity of the value
#'   function and has a default value of 0.88  as given by Reference[1].
#'
#' @param lambda lambda > 1. Parameter of loss aversion for the value function
#'   as and has a default value of 2.25 given by Reference[1].
#'
#' @seealso \code{\link{decisionMatrix, overallTRP, overallDRP,
#'   weight.differenceToIdeal, weight.entropy, weight.highAndStandard}}
#'
#' @details This function is vectorized in the \code{userid} parameter. The
#'   function does not sort attributes or user IDs. Order of the output is
#'   generated as given through the arguments.
#'
#'   The 3 step calculation of the prospect values comes from one specific paper
#'   \emph{Reference[1]}. (1) For the noramlized gain and loss matrices we use
#'   the function \code{\link{norm.gainLoss}} from this package. (2) The value
#'   matrix is calculated with a series of auxiliary functions. (3) The prospect
#'   value works with a simple additive weighting method from
#'   \code{overall_pv_extend}.
#'
#'   \code{dataset} We assume the input data.frame has following columns usid =
#'   User IDs, round = integers indicating which round the user is in (0-index
#'   works best for 'round'), atid = integer column for referring the attribute
#'   ID (1 indexed), selected = numeric value of the attribute for a specific,
#'   given round, selectable = amount of options the user can chose at a given
#'   round, with the current configuration. This is a necessary parameter.
#'
#'   \code{userid} is a necessary parameter.
#'
#'   \code{weight} default orders each attribute a weight <= 1 according to the
#'   the weight function \code{\link{differenceToIdeal}}. Ideally the sum of all
#'   weights equals 1.
#'
#'   \code{alpha} Default value as given by Reference [1] is 0.88
#'
#'   \code{beta} Default value as given by Reference [1] is 0.88
#'
#'   \code{lambda} Default value as given by Reference [1] is 2.25
#'
#' @return overall prospect values for each attribute
#' @examples #Not runnable yet
#' overallPV(data_camera, 2)
#' overallPV(data_pc, userid = 1, weight=c(0.1,0.4,0.3,0.2))
#' overallPV(full_data, 6 ,attr = c(1,2,3), rounds="all", alpha = 0.95, beta = 0.78)
#'
#' @references [1]Kahneman, D., & Tversky, A. (1979). Prospect theory: An
#'   analysis of decision under risk. Econometrica: Journal of the Econometric
#'   Society, 263-291.
#'
#'   [2] Tversky, A., & Kahneman, D. (1992). Advances in prospect theory:
#'   Cumulative representation of uncertainty. Journal of Risk and uncertainty,
#'   5(4), 297-323.
#'
#'   [3] Fan, Z. P., Zhang, X., Chen, F. D., & Liu, Y. (2013). Multiple
#'   attribute decision making considering aspiration-levels: A method based on
#'   prospect theory. Computers & Industrial Engineering, 65(2), 341-350.
#'
#' @export

overallPV <- function (dataset, userid = NULL, attr = NULL, rounds = NULL, refps = NULL, cost_ids = NULL,  weight = NULL, weightFUN = "differenceToIdeal",
                       alpha = 0.88, beta = 0.88, lambda = 2.25, gamma) {

  if(is.null(weight) & is.null(dataset)) {
    stop("Unable to get weights. You need to enter the weights or provide the dataset for us to calculate them.")
  }

  weight <- getAttrWeights(dataset, userid, weight,  attr, rounds, cost_ids, weightFUN, gamma)

  pvMatrices <- pvMatrix(dataset, userid, attr, rounds, refps, cost_ids, alpha, beta, lambda)

  tryCatchResult = tryCatch({
    overall_pv <- mapply(overall_pv_extend, pvMatrices, weight, SIMPLIFY = F) ##Perhaps mapply when data.frame, make weights as list?!

  }, warning = function(condition) {
    message("Probably amount of users differs from amount of weightVectors and they cannot be recycled.")
    message("Result most likely not accurrate, check your arguments.")
  }, error = function(condition) {
    stop("The input in the weight parameters can be flexible but check if you are entering the right amount of weightVectors, users and attributes")
  }, finally={
  })
  overall_pv
}

#' Calcultes the overall prospect values using the TRP approach (tri reference
#' point theory)
#'
#' The overall prospect values for each alternative (round) are calculated using
#' two components in a method described in [2]: (1) the value matrix, which is
#' calculated using \code{\link{trp.valueMatrix}} with the trp-value function as
#' given by [1]. And the (2) decision weights, which can be calculated in this
#' package using three functions. See Details of weight attribute. Its
#' equivalent function for prospect theory and the dual reference point approach
#' are \code{\link{overallPV}} and \code{\link{overallDRP}}.
#'
#' In the context of data stemming from product configurators, the highest
#' overall value returned  by this function, means that specific product
#' configuration represented the highest value for the user. This depends mainly
#' on three important factors: (1) What theoretical framework you choose to use
#' (PT, DRP, TRP), (2) which decision weights you assign to each attribute, and
#' (3) the reference point(s) you input.
#'
#' @inheritParams trpValueMatrix
#' @inheritParams getAttrWeights
#'
#' @seealso \code{\link{decisionMatrix, overallTRP, overallDRP,
#'   weight.differenceToIdeal, weight.entropy, weight.highAndStandard}}
#'
#' @details This function is vectorized in the \code{userid} parameter.
#'
#'   \code{dataset} We assume the input data.frame has following columns usid =
#'   User IDs, round = integers indicating which round the user is in (0-index
#'   works best for 'round'), atid = integer column for referring the attribute
#'   ID (1 indexed), selected = numeric value of the attribute for a specific,
#'   given round, selectable = amount of options the user can chose at a given
#'   round, with the current configuration. This is a necessary parameter.
#'
#'   \code{userid} is a necessary parameter.
#'
#'   \code{rounds} Default calculates with first and last rounds (initial and
#'   final product configuration). You can give a vector of arbitrarily chosen
#'   rounds as well.
#'
#'   \code{cost_ids} Default assumes all your attributes are of benefit type,
#'   that is a higher value in the attribute means the user is better off than
#'   with a lower value. If one or more of the attributes in your data is of
#'   cost type, e.g. price, so that lower is better then you should identify
#'   this attributes as such, providing their id, they'll be converted to
#'   benefit type (higher amount is better).
#'
#'   \code{weight} default orders each attribute a weight <= 1 according to the
#'   the weight function \code{\link{differenceToIdeal}}. Ideally the sum of all
#'   weights equals 1. The three weighting functions are:
#'   \code{\link{weight.differenceToIdeal, weight.entropy,
#'   weight.highAndStandard}}
#'
#'   \code{delta} [1] Initially called alpha, we chose delta to avoid confusion
#'   with prospect theory's parameter for concavity, such as seen in
#'   \code{\link{overallPV}}
#'
#'   Note: When converting a cost attribute to a benefit attribute its three
#'   reference points change as well, enter the unconverted tri.refps, the
#'   function transforms them automatically when it detects a \code{cost_ids !=
#'   NULL}. Also, since for cost attributes, lower is better, unconverted they
#'   should follow (G < SQ < MR).
#'
#' @return overall prospect values for each alternative/row
#'
#' @examples #Not runnable yet
#' overallTRP(pc_config_data, 9, attr=3,  tri.refps = c(1, 3.5))
#' overallTRP(pc_config_data, 9, attr=1 tri.refps = c(1, 3.5), lambda=2, delta=0.5)
#' overallTRP(myData, userid = 58, rounds = "all", attr=3:4, tri.refps=matrix(1:6, 2, 3, byrow=T))
#' overallTRP(myData, userid = 11, attr =  1, cost_ids = 1, tri.refps = c(8, 2))
#' overallTRP(full_data, 30:35 ,attr = c(1,2,3), rounds="all", tri.refps=matrix(1:9, 3, 3, byrow=T))
#'
#' @references [1] [1]Wang, X. T.; Johnson, Joseph G. (2012) \emph{A tri-reference
#'   point theory of decision making under risk. }Journal of Experimental
#'   Psychology
#'
#'   [2] Fan, Z. P., Zhang, X., Chen, F. D., & Liu, Y. (2013). Multiple
#'   attribute decision making considering aspiration-levels: A method based on
#'   prospect theory. Computers & Industrial Engineering, 65(2), 341-350.
#'
#' @export

overallTRP <- function(dataset, userid = NULL, attr = NULL, rounds = NULL, refps = NULL, cost_ids = NULL,  weight = NULL, weightFUN = "differenceToIdeal",
                        tri.refps = NULL, beta_f = 5, beta_l = 1, beta_g = 1, beta_s = 3, gamma) {

  if(is.null(weight) & is.null(dataset)) {
    stop("Unable to get weights. You need to enter the weights or provide the dataset for us to calculate them.")
  }

  weight <- getAttrWeights(dataset, userid, weight,  attr, rounds, cost_ids, weightFUN, gamma)

  trpValueMatrices <- trp.valueMatrix(dataset, userid, attr, rounds, cost_ids, tri.refps, beta_f, beta_l, beta_g, beta_s)

  tryCatchResult = tryCatch({
    overall_pv <- mapply(overall_pv_extend, trpValueMatrices, weight, SIMPLIFY = F) ##Perhaps mapply when data.frame, make weights as list?!

  }, warning = function(condition) {
    message("Probably amount of users differs from amount of weightVectors and they cannot be recycled.")
    message("Result most likely not accurrate, check your arguments.")
  }, error = function(condition) {
    stop("The input in the weight parameters can be flexible but check if you are entering the right amount of weightVectors, users and attributes")
  }, finally={
  })

  overall_pv
}

#' Calcultes the overall prospect values using the DRP approach (dual reference
#' point)
#'
#' The overall prospect values for each alternative (round) are calculated using
#' two components in a method described in [3]: (1) the value matrix, which is
#' calculated using \code{\link{dual.valueMatrix}} as given by [1] and (2) the
#' decision weights, which can be calculated in this package using three
#' functions. See Details of weight attribute. Its equivalent function for
#' prospect theory and the tri-reference point theory are
#' \code{\link{overallPV}} and \code{\link{overallTRP}}.
#'
#' In the context of data stemming from product configurators, the highest
#' overall value returned  by this function, means that specific product
#' configuration represented the highest value for the user. This depends mainly
#' on three important factors: (1) What theoretical framework you choose to use
#' (PT, DRP, TRP), (2) which decision weights you assign to each attribute, and
#' (3) the reference point(s) you input.
#'
#' @inheritParams dualValueMatrix
#' @inheritParams getAttrWeights
#'
#' @seealso \code{\link{decisionMatrix, overallTRP, overallDRP,
#'   weight.differenceToIdeal, weight.entropy, weight.highAndStandard}}
#'
#' @details This function is vectorized in the \code{userid} parameter.
#'
#'   \code{dataset} We assume the input data.frame has following columns usid =
#'   User IDs, round = integers indicating which round the user is in (0-index
#'   works best for 'round'), atid = integer column for referring the attribute
#'   ID (1 indexed), selected = numeric value of the attribute for a specific,
#'   given round, selectable = amount of options the user can chose at a given
#'   round, with the current configuration. This is a necessary parameter.
#'
#'   \code{userid} is a necessary parameter.
#'
#'   \code{rounds} Default calculates with first and last rounds (initial and
#'   final product configuration). You can give a vector of arbitrarily chosen
#'   rounds as well.
#'
#'   \code{cost_ids} Default assumes all your attributes are of benefit type,
#'   that is a higher value in the attribute means the user is better off than
#'   with a lower value. If one or more of the attributes in your data is of
#'   cost type, e.g. price, so that lower is better then you should identify
#'   this attributes as such, providing their id, they'll be converted to
#'   benefit type (higher amount is better).
#'
#'   \code{weight} default orders each attribute a weight <= 1 according to the
#'   the weight function \code{\link{differenceToIdeal}}. Ideally the sum of all
#'   weights equals 1. The three weighting functions are:
#'   \code{\link{weight.differenceToIdeal, weight.entropy,
#'   weight.highAndStandard}}
#'
#'   \code{delta} [1] Initially called alpha, we chose delta to avoid confusion
#'   with prospect theory's parameter for concavity, such as seen in
#'   \code{\link{overallPV}}
#'
#'   Note: When converting a cost attribute to a benefit attribute its two
#'   reference points change as well, enter the unconverted dual.refps, the
#'   function transforms them automatically when it detects a \code{cost_ids !=
#'   NULL}. Also, since for cost attributes, lower is better, unconverted they
#'   should follow (G < SQ).
#'
#' @return overall prospect values for each alternative/row
#'
#' @examples #Not runnable yet
#' overallDRP(pc_config_data, 9, dual.refps = c(1, 3.5))
#' overallDRP(pc_config_data, 9, dual.refps = c(1, 3.5), lambda=2, delta=0.5)
#' overallDRP(aDataFrame, userid = 100, rounds = "all", dual.refps = c(1, 2))
#' overallDRP(myData, userid = 11, attr =  1, cost_ids = 1, dual.refps = c(8, 2))
#' overallDRP(full_data, 15:16 ,attr = c(1,2,3), rounds="all", dual.refps=matrix(1:6, 3, 2, byrow=T))
#'
#' @references [1] Golman, R., & Loewenstein, G. (2011). Explaining Nonconvex
#'   Preferences with Aspirational and Status Quo Reference Dependence. Mimeo,
#'   Carnegie Mellon University.
#'
#'   [2] Tversky, A., & Kahneman, D. (1992). Advances in prospect theory:
#'   Cumulative representation of uncertainty. Journal of Risk and uncertainty,
#'   5(4), 297-323.
#'
#'   [3] Fan, Z. P., Zhang, X., Chen, F. D., & Liu, Y. (2013). Multiple
#'   attribute decision making considering aspiration-levels: A method based on
#'   prospect theory. Computers & Industrial Engineering, 65(2), 341-350.
#'
#' @export

overallDRP <- function(dataset, userid = NULL, attr = NULL, rounds = NULL, cost_ids = NULL, weight = NULL, weightFUN = "differenceToIdeal",
                        dual.refps = c(sq=NA, g=NA), lambda = 2.25, delta = 0.8, consumption_fun = NULL, gamma) {

  if(is.null(weight) & is.null(dataset)) {
    stop("Unable to get weights. You need to enter the weights or provide the dataset for us to calculate them.")
  }

  weight <- getAttrWeights(dataset, userid, weight,  attr, rounds, cost_ids, weightFUN, gamma)

  dualValueMatrices <- dual.valueMatrix(dataset, userid, attr, rounds, cost_ids, dual.refps,lambda, delta, consumption_fun)

  tryCatchResult = tryCatch({
    overall_pv <- mapply(overall_pv_extend, dualValueMatrices, weight, SIMPLIFY = F) ##Perhaps mapply when data.frame, make weights as list?!

  }, warning = function(condition) {
    message("Probably amount of users differs from amount of weightVectors and they cannot be recycled.")
    message("Result most likely not accurrate, check your arguments.")
  }, error = function(condition) {
    stop("The input in the weight parameters can be flexible but check if you are entering the right amount of weightVectors, users and attributes")
  }, finally={
  })

  overall_pv
}

#' Calculate Prospect Values
#'
#' This function works with the simple additive weighting method. It takes a
#' value matrix and the weight of each attribute and calculates the prospect
#' value for each alternative (round) [1,2].
#'
#' @param value_matrix a List of numeric matrices, results from using the value function on
#'   previously calculated normalized gain and loss matrices.
#'
#' @inheritParams getAttrWeights
#'
#' @details You need to pre-calculate the value matrix, for example with
#'   \code{\link{pvalue_matrix}} and give it as a parameter. This is one of the
#'   few functions of this package that do not allow you to give the raw data
#'   from your product Configurator, but rather calculate a previous
#'   result(value matrix) to input here.
#'
#'   \code{value_matrix} ncol = number of attributes, nrow = number of rounds.
#'
#' @return a list of overall prospect values for each round (each product
#'   alternative). Each element of a list is the result for one user.
#'
#' @references [1] Fan, Z. P., Zhang, X., Chen, F. D., & Liu, Y. (2013).
#'   Multiple attribute decision making considering aspiration-levels: A method
#'   based on prospect theory. Computers & Industrial Engineering, 65(2),
#'   341-350.
#'
#'   [2] Kahneman, D., & Tversky, A. (1979). Prospect theory: An analysis of
#'   decision under risk. Econometrica: Journal of the Econometric Society,
#'   263-291.
#'
#' @seealso \code{\link{overallPV}}
#'
#' @examples
#' overall_pv_extend(my_value_matrix, weight = c(0.1, 0.2, 0.4, 0.3))
#' overall_pv_extend(pvMatrix(someData, someUsers), weight = getAttrWeights(someData, someUsers))
#' overall_pv_extend(my_matrix, weight=c(0.8,0.05,0.05,0.1))
#' overall_pv_extend(vMatrix, weight =  aWeightVector)
#'
#' \dontrun{overall_pv_extend(value_mx)}  # Always provide weights or dataset.
#'
#' @export

overall_pv_extend <- function(value_matrix, weight = NULL) {
  if (length(weight) != ncol(value_matrix)) {
    text <- paste0("weights: ", length(weight), " != ", ncol(value_matrix), " cols in valueMatrix.")
    stop("Amount of weights does not equal the amount of columns/attr: ", text, " Check your arguments")
  }
  else {
    result = tryCatch({
      result <- apply(value_matrix, 1, function(my_vector) { sum(my_vector*weight)})
    }, warning = function(condition) {
      message("Amount of weights does not equal the amount of columns/attr: ", text)
    }, error = function(condition) {
      message("An error is unlikely, possible wrong type input.")
    }, finally={
    })
  }

  result
}




