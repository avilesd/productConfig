# Dual reference point:
# New set of functions for implementing the Dual Reference Model (SQ, Aspirations) and the overall prospect values
# x + ln(x) + ((10/(1+(1/e^(0.5*(x+ln(x)-5+ln(5))))))-(10/(1+(1/e^(0.5*(3+ln(3)-5+ln(5)))))))
# ((10/(1+(1/e^(0.5*(4+ln(4)-15+ln(15))))))-(10/(1+(1/e^(0.5*(3+ln(3)-15+ln(15))))))) x = 4, sq= 3 result = 0.143286
# ((10/(1+(1/e^(0.5*(2+ln(2)-15+ln(15))))))-(10/(1+(1/e^(0.5*(3+ln(3)-15+ln(15))))))) x = 2, sq= 3 result = -0.08188

#' Returns a Value Matrix using two reference points
#'
#' This function is based on the value function of the dual reference point
#' (dual-rp) models, as seen in reference 1 (Golman, Loewenstein). It first
#' builds a desicion matrix for each user and then applys the 'utility function'
#' over each value using two given reference points (SQ, G). It does so by
#' calling the function \code{\link{smallerThanZero}}. The dual-rp utility
#' function works in two steps, much like prospect theory's value function
#' \code{\link{overallPV},\link{pvMatrix}} See details and references.
#'
#' @param dataset data.frame with the user generated data from a product
#'   configurator. See \code{decisionMatrix} for specifications of the dataset.
#' @param userid a vector of integers that gives the information of which users
#'   the matrix should be calculated. Vectorised.
#' @param attr attributes IDs, vector of integer numbers corresponding to the
#'   attributes you desire to use; \code{attr} are assumed to be 1-indexed.
#' @param rounds integer vector or text option. Which steps of the configuration
#'   process should be shown? Defaults are first and last step. Text options are
#'   \code{all, first, last}.
#' @param cost_ids argument used to convert selected cost attributes into
#'   benefit attributes. Integer vector.
#' @param dual.refps numeric - two numbers indicating the status-quo and the
#'   aspiration level(goal) for the given attributes. Status-quo should always
#'   be the first input.
#' @param lambda numeric - parameter of loss aversion for the value function as
#'   given by reference[1]. Default value is 2.25 as in [2] and should be
#'   \code{lambda > 1}.
#' @param delta numeric - expresses the relative importance of the aspiration
#'   level to other factors. Default is 0.8 and it should satisfy \code{0 <
#'   delta <1}.
#' @param consumption_fun non-working parameter for future developments. Leave
#'   at NULL.
#'
#' @details Note that since the dual-rp value function uses a logarithmic
#'   function, attribute values and reference points should be larger than zero.
#'   Nontheless, if this function detects a zero or negative value it will
#'   monotonically scale your data and your reference points so that for all
#'   applies \code{x > 0}. The transformation does not have an impact on the
#'   absolute differences between attribute values and reference points.
#'
#'   This function only makes sense to use with multiple attributes if those
#'   attributes have exactly the same two reference points (sq, g). Therefore
#'   you will have to manually calculate all the value matrices for the
#'   different attributes (with different values) and cbind them together using
#'   mapply. The full matrix can then be given as an input to the
#'   \code{\link{overallPV_interface}} fucntion to calculate the overall
#'   prospect values for each round.
#'
#'   \code{data} We assume the input data.frame has following columns usid =
#'   User IDs, round = integers indicating which round the user is in (0-index
#'   works best for 'round'), atid = integer column for referring the attribute
#'   ID (1 indexed), selected = numeric value of the attribute for a specific,
#'   given round, selectable = amount of options the user can chose at a given
#'   round, with the current configuration.
#'
#'   \code{userid} is a necessary parameter, without it you'll get a warning.
#'   Default is NULL.
#'
#'   \code{attr} Default calculates with all attributes. Attributes are
#'   automatically read from provided dataset, it is important you always
#'   provide the complete data so that the package functions properly. Moreover,
#'   \code{userid} and \code{attr} will not be sorted and will appear in the
#'   order you input them.
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
#'   \code{delta} [1] Initially called alpha, we chose delta to avoid confusion
#'   with prospect theory's parameter for concavity, such as seen in
#'   \code{\link{overallPV}}
#'
#'   Note: When converting a cost attribute to a benefit attribute its two
#'   reference points change as well, enter the unconverted dual.refps, the
#'   function transforms them automatically when it detects a \code{cost_ids !=
#'   NULL}.
#'
#' @return a list of value matrices for each user.
#'
#' @references [1] Golman, R., & Loewenstein, G. (2011). Explaining Nonconvex
#'   Preferences with Aspirational and Status Quo Reference Dependence. Mimeo,
#'   Carnegie Mellon University.
#'
#'   [2] Tversky, A., & Kahneman, D. (1992). Advances in prospect theory:
#'   Cumulative representation of uncertainty. Journal of Risk and uncertainty,
#'   5(4), 297-323.
#'
#' @examples
#' dualValueMatrix(pc_config_data, 9:10, dual.refps = c(1, 3.5))
#' dualValueMatrix(aDataFrame, userid = 100, rounds = "all", dual.refps = c(1, 2))
#' dualValueMatrix(myData, userid = 11, attr = c(1,3,5), cost_ids = 2) #Input accepted but cost_ids = 2 will be ignored
#' dualValueMatrix(myData, userid = 11, attr =  1, cost_ids = 1, dual.refps = c(8, 2)) # Note that for cost attributes:  SQ > G
#' dualValueMatrix(keyboard_data, 60, rounds = "first", attr = 1, dual.refps = c(1, 2), lambda = 5, delta = 0.5)
#' dualValueMatrix(data1, 2) # Returns an error since no reference points given
#'
#' @export

dualValueMatrix <- function(dataset, userid = NULL, attr = NULL, rounds = NULL, cost_ids = NULL,
                            dual.refps = c(sq=NA, g=NA), lambda = 2.25, delta = 0.8, consumption_fun = NULL) {
  counter <- 0
  if (length(attr) == 1) {
    dual.list <- dualValueMatrix.oneAttr(dataset, userid, attr, rounds, cost_ids,
                                       dual.refps, lambda, delta, consumption_fun)
  }
  else {
    if (is.null(attr)) attr <- get_attrs_ID(dataset)
    for(i in attr) {
      if (counter == 0) {
        if(i %in% cost_ids) cost_ids_help <- i else cost_ids_help <- NULL
        dual.list <- dualValueMatrix.oneAttr(dataset, userid, attr=i, rounds, cost_ids_help,
                                             dual.refps, lambda, delta, consumption_fun)
        counter <- 1
      }
      else {
        if(i %in% cost_ids) cost_ids_help <- i else cost_ids_help <- NULL
        tempVariable <- dualValueMatrix.oneAttr(dataset, userid, attr=i, rounds, cost_ids_help,
                                                 dual.refps, lambda, delta, consumption_fun)
        dual.list <- mapply(cbind, dual.list, tempVariable, SIMPLIFY = F)
      }
    }
  }
  dual.list
}

#'Returns a Value Matrix using two reference points (one attribute only)
#'
#'This function is a more basic function than \code{\link{trpValueMatrix}}. This
#'function is based on the value function of the dual-reference point model
#'(dual-rp) [1]. It first builds a desicion matrix for each user and then applys
#'the drp-utility function over each value using \code{\link{smallerThanZero}}.
#'The dual-rp value function first creates a gain-loss matrix based on the two
#'reference points. It then outputs the value of each gain/loss based on the
#'loss aversion (lambda) and the relative importance of the goal (delta).
#'
#'@param dataset data.frame with the user generated data from a product
#'  configurator. See \code{decisionMatrix} for specifications of the dataset.
#'@param userid a vector of integers that gives the information of which users
#'  the matrix should be calculated. Vectorised.
#'@param attr attributes ID, \emph{one integer} corresponding to the attribute
#'  you desire to use; \code{attr} are assumed to be 1-indexed.
#'@param rounds integer vector or text option. Which steps of the configuration
#'  process should be shown? Defaults are first and last step. Text options are
#'  \code{all, first, last}.
#'@param cost_ids argument used to convert selected cost attributes into benefit
#'  attributes. Cost attribute means that weith a lower value, the user is
#'  better off than with a higher value (e.g. price). Default assumes all
#'  attributes are of benefit type (higher amount is better).
#'@param dual.refps numeric - two numbers indicating the status-quo and the
#'  aspiration level(goal) for the given attributes. Status-quo should always be
#'  the first input. Contrary to \code{\link{trpValueMatrix}}, this function
#'  also allows for aspiration levels to be smaller than the status-quo (g < sq)
#'  [1].
#'@param lambda numeric - parameter of loss aversion for the value function as
#'  given by [1]. Default value is 2.25 as in [2] and should be \code{lambda >
#'  1}.
#'@param delta numeric - expresses the relative importance of the aspiration
#'  level to other factors. Default is 0.8 and it should satisfy \code{0 < delta
#'  <1}.
#'
#'@details This function does the same as \code{\link{dualValueMatrix}} but only
#'  for one attribute, for more details please see the mentioned function.
#'
#'  Note: When converting a cost attribute to a benefit attribute its two
#'  reference points change as well, enter the unconverted refps, the function
#'  transforms them automatically when it detects a \code{cost_ids  != NULL}
#'
#'@return a list of value matrices with one attribute for each user.
#'
#'@references [1] Golman, R., & Loewenstein, G. (2011). Explaining Nonconvex
#'   Preferences with Aspirational and Status Quo Reference Dependence. Mimeo,
#'   Carnegie Mellon University.
#'
#'   [2] Tversky, A., & Kahneman, D. (1992). Advances in prospect theory:
#'   Cumulative representation of uncertainty. Journal of Risk and uncertainty,
#'   5(4), 297-323.
#'
#' @examples
#' dualValueMatrix.oneAttr(myData, 10, attr = 3, dual.refps = c(1, 3.5))
#' dualValueMatrix.oneAttr(myData, userid= 60, rounds= "all", attr = 1, dual.refps = c(1.5, 2.5)
#' dualValueMatrix.oneAttr(myData, 10, attr=4, dual.refps = c(0.17, -0.10), cost_ids = 4) # Note for cost_ids SQ > G
#'
#' # Return an error, 1.Too many attributes or 2. none entered
#' dualValueMatrix.oneAttr(keyboard_data, 8:9 , attr = c(10,12,14,16), dual.refps = c(100, 150))
#' dualValueMatrix.oneAttr(data1, 2) # 2. No attribute entered
#'
#'@export

dualValueMatrix.oneAttr <- function(dataset, userid = NULL, attr = NULL, rounds = NULL, cost_ids = NULL,
                                    dual.refps = c(sq=NA, g=NA), lambda = 2.25, delta = 0.8, consumption_fun = NULL) {
  sq <- dual.refps[1]
  g <- dual.refps[2]

  if(length(attr)!= 1) stop("Please insert (only) one attribute ID.")
  if(is.na(sq) | is.na(g)) stop("Please provide both reference points (sq, g)")

  if(!is.null(cost_ids)) {
    # This function does not checks if sq < g, since not required in the model
    g <- (-1)*g
    sq <- (-1)*sq
    dual.refps <- c(sq, g)
  }

  list.decMatrices <- decisionMatrix(dataset, userid, attr, rounds, cost_ids)
  if (is.null(consumption_fun)) {
    # Check if negative, through peeking data and cost_ids (already applied in dM), cost_ids not need explicit.
    # When all positive procede with f1 and then f2
    valueMatrices <- lapply(list.decMatrices, smallerThanZero, dual.refps, lambda, delta)
  }
  else { # Write in comments, no transformation will be carried on, negative values accepted
         # user will have to write its own function as an argument (function(t) ...)
    #gainLossMatrices <- lapply(list.decMatrices, gainLoss.consumption, dual.refps, consumption_fun)
    #valueMatrices <- lapply(gainLossMatrices, lossAversion.consumption, list.decMatrices, dual.refps, lambda, delta, consumption_fun)
    stop("consumption_fun should be NULL.")
  }
  valueMatrices
}

#@smallerThanZero, we decided to normalize just after calculating gains and
#losses, it is not 100% reliable, but much better than normalizing before (raw
#data) or after(value matrix) because you loose the information about 1.
#distance to the sq and 2. loss aversion effect, respectively. (In BA Writing Todo - Sprint 3)

#' Outputs a value matrix from a decision matrix
#'
#' This function is called within \code{\link{trpValueMatrix}}, but it is the
#' function that actually does the work of producing the value matrix.It works
#' in three steps. The first step checks if the matrix or the reference points
#' have a value smaller or equal to 0. If this is the case it monotonically
#' scales all values so that they are all larger than zero*. The second step
#' calculates a gain-loss matrix using both reference points and a function
#' given by [1]. The third step normalizes the gain-loss matrix to make all
#' attributes comparable.
#'
#' @param aMatrix the decision matrix, with \code{attr} as columns and
#'   \code{rounds} as rows.
#' @param dual.refps two numeric reference points (status-quo, goal).
#' @param lambda numeric - parameter of loss aversion for the value function as
#'   given by reference[1]. Default value is 2.25 as given by [2].
#' @param delta numeric - expresses the relative importance of the aspiration
#'   level to other factors. Default is 0.8 and it should satisfy \code{0 <
#'   delta <1}.
#'
#' @details *The transformation changes all values, but keeps the differences
#'   between them as they were.
#'
#' @return a value matrix with equal dimensions as the input \code{aMatrix}
#'
#' @references [1] Golman, R., & Loewenstein, G. (2011). Explaining Nonconvex
#'   Preferences with Aspirational and Status Quo Reference Dependence. Mimeo,
#'   Carnegie Mellon University.
#'
#'   [2] Tversky, A., & Kahneman, D. (1992). Advances in prospect theory:
#'   Cumulative representation of uncertainty. Journal of Risk and uncertainty,
#'   5(4), 297-323.
#'
#' @examples #Runnable
#' smallerThanZero(matrix(16:31, 4, 4), dual.refps= c(22, 28))
#' smallerThanZero(matrix(16:31, 4, 4), dual.refps= c(22, 28), delta= 0.6)
#' smallerThanZero(matrix(1:100, 5, 20, byrow= T), dual.refps= c(sq=45, g=88))
#'
#' #Not runnable yet
#' smallerThanZero <- function(decisionMatrix(myData, 9, rounds="all"), c(1.5, 2.5), lambda = 5, delta = 0.8)
#' @export

smallerThanZero <- function(aMatrix, dual.refps, lambda = 2.25, delta = 0.8) {
  if(all(aMatrix > 0) & all(dual.refps > 0)) {}
    # aMatrix and dual.refps stay the same
  else {
    #Important, should we change g and sq with (-1), yes see notes, here or outside? add cost_ids??
    #Yes sq and g, should already go (-1) here, think about best way smallerThanZero(dm910_4cost[[1]], c(-0.17,0.10))
     dmSmallerZero <- aMatrix[aMatrix <= 0]
     rpSmallerZero <- dual.refps[dual.refps <= 0]
     smallerZero <- c(dmSmallerZero, rpSmallerZero)
     smallest <- min(smallerZero)
     absOfSmallestPlus1 <- abs(smallest) + 1

     #Transform dm and dual.refps if
     aMatrix <- aMatrix + absOfSmallestPlus1
     dual.refps <- dual.refps + absOfSmallestPlus1
  }
  gainLossMatrix <- dualGainLossFunction(aMatrix, dual.refps)
  hmaxVector <-  max(abs(gainLossMatrix)) #since no 0 possible, replacing 0 with 1 is not necessary
  hmaxVector <- replace(hmaxVector, hmaxVector==0.0, 1.00) # Avoid divide by zero
  norm.gainLossMatrix <- gainLossMatrix/hmaxVector

  dual.vMatrix <- dualLossAversionFun(norm.gainLossMatrix, aMatrix, lambda, delta)
  dual.vMatrix

  #Call from here and return valueMatrix, give argument to return gain? or another equal function?
}

#' Title
#'
#' @param aMatrix
#' @param dual.refps
#'
#' @details *The transformation changes all values, but keeps the differences
#'   between them as they were.
#'
#' @return a value matrix with equal dimensions as the input \code{aMatrix}
#'
#' @references [1] Golman, R., & Loewenstein, G. (2011). Explaining Nonconvex
#'   Preferences with Aspirational and Status Quo Reference Dependence. Mimeo,
#'   Carnegie Mellon University.
#'
#'   [2] Tversky, A., & Kahneman, D. (1992). Advances in prospect theory:
#'   Cumulative representation of uncertainty. Journal of Risk and uncertainty,
#'   5(4), 297-323.
#'
#' @examples #Runnable
#' smallerThanZero(matrix(16:31, 4, 4), dual.refps= c(22, 28))
#' smallerThanZero(matrix(16:31, 4, 4), dual.refps= c(22, 28), delta= 0.6)
#' smallerThanZero(matrix(1:100, 5, 20, byrow= T), dual.refps= c(sq=45, g=88))
#'
#' #Not runnable yet
#' smallerThanZero <- function(decisionMatrix(myData, 9, rounds="all"), c(1.5, 2.5), lambda = 5, delta = 0.8)
#' @export

dualGainLossFunction <- function(aMatrix, dual.refps) {
  x <- aMatrix
  if(any(x < 0)) warning("x should not be smaller than 0.")
  sq <- dual.refps[1]
  g <- dual.refps[2]
  result <- (10/(1+(1/exp(0.5*(x+log(x)-g+log(g))))))-(10/(1+(1/exp(0.5*(sq+log(sq)-g+log(g))))))
  result
}

#' Title
#'
#' @param gainLossMatrix
#' @param aMatrix
#' @param lambda
#' @param delta
#'
#' @details *The transformation changes all values, but keeps the differences
#'   between them as they were.
#'
#' @return a value matrix with equal dimensions as the input \code{aMatrix}
#'
#' @references [1] Golman, R., & Loewenstein, G. (2011). Explaining Nonconvex
#'   Preferences with Aspirational and Status Quo Reference Dependence. Mimeo,
#'   Carnegie Mellon University.
#'
#'   [2] Tversky, A., & Kahneman, D. (1992). Advances in prospect theory:
#'   Cumulative representation of uncertainty. Journal of Risk and uncertainty,
#'   5(4), 297-323.
#'
#' @examples #Runnable
#' smallerThanZero(matrix(16:31, 4, 4), dual.refps= c(22, 28))
#' smallerThanZero(matrix(16:31, 4, 4), dual.refps= c(22, 28), delta= 0.6)
#' smallerThanZero(matrix(1:100, 5, 20, byrow= T), dual.refps= c(sq=45, g=88))
#'
#' #Not runnable yet
#' smallerThanZero <- function(decisionMatrix(myData, 9, rounds="all"), c(1.5, 2.5), lambda = 5, delta = 0.8)
#' @export

dualLossAversionFun <- function(gainLossMatrix, aMatrix, lambda = 2.25, delta = 0.8) {
  yMatrix <- apply(gainLossMatrix, 1:2, function(y) if(y < 0) {delta*lambda*y} else {delta*y})
  xMatrix <- apply(aMatrix, 1:2, function(x) (1-delta)*(x + log(x)))
  valueM <- yMatrix + xMatrix
  valueM
}
####################
####################
#gainLoss.consumption <- function(aMatrix, dual.refps, cons_fun) {
#  x <- aMatrix
#  sq <- dual.refps[1]
#  g <- dual.refps[2]
#  result <- (10/(1+(1/exp(0.5*(cons_fun(x)-cons_fun(g))))))-(10/(1+(1/exp(0.5*(cons_fun(sq)-cons_fun(g))))))
#  result
#}
#lossAversion.consumption <- function(gainLossMatrix, aMatrix, lambda = 2.25, delta = 0.8, cons_fun) {
#  yMatrix <- apply(gainLossMatrix, 1:2, function(y) if(y < 0) {delta*lambda*y} else {delta*y})
#  xMatrix <- apply(aMatrix, 1:2, function(x) (1-delta)*(cons_fun(x)))
#  valueM <- yMatrix + xMatrix
#  valueM
#}

#Vectorialised if empty, returns NA
#greaterThanZero <- function(x) {
#  result <- x[x <= 0]
#  if (length(result) == 0) result <- NA
#  result
#}
