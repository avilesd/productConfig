#New set of functions for calculating the value matrix of the Tri Reference Point theory and the overall prospect values

## DOCU: Mention that it is a practical problem that trp doesn't use normalized values for its value function, so
## you may have different (mr, sq, g) for each attribute, so you have to run the above function with the attribute
## that have the same values and then manually bind them.

## DOCU cost_ids still works pretty well, but the tri.refps also have to be
#changed, explain it on BA # with a nice diagram, mr and g exchange values and
#change from positive/negative sign. cost_ids, enter normal reference points, we
#will convert them. cost_ids has to equal the attribute you are inputting

## DOCU: you can enter cost_ids normally, program will recognize for which attr it should use the cost_ids

#Main Interface function as in P.10 from Notes

#' Returns a Value Matrix using three reference points
#'
#' This function is based on the value function of the tri-reference point (trp)
#' theory. It first builds a desicion matrix for each user and then applys the
#' trp-value function over each value using the three given reference points
#' (MR, SQ, G) and other four free parameters from the value function. See
#' references.
#'
#' @param dataset data.frame with the user generated data from a product
#'   configurator. See \code{decisionMatrix} for specifications of the dataset.
#' @param userid a vector of integers that gives the information of which users
#'   the matrix should be calculated. Vectorised.
#' @param attr attributes IDs, vector of integer numbers corresponding to the
#'   attributes you desire to use; attr are assumed to be 1-indexed.
#' @param rounds integer vector or text option. Which steps of the configuration
#'   process should be shown? Defaults are first and last step. Text options are
#'   \code{all, first, last}.
#' @param cost_ids argument used to convert selected cost attributes into
#'   benefit attributes. Integer vector.
#' @param mr numeric - Minimum Requirements is the lowest reference point
#' @param sq numeric - Status Quo reference point
#' @param g numeric - Goal reference point
#' @param beta(s) numeric arguments representing the psychological impact of an
#'   outcome equaling failer (_f), loss (_l), gain (_g) or success (_s). Default
#'   values are taken from our reference paper \code{(5,1,1,3)}.
#'
#'
#' @details This function only makes sense to use with multiple attributes if those
#'   attributes have exactly the same three reference points (mr, sq, g).
#'   Therefore you will have to manually calculate all the value matrices for
#'   the different attributes (with different values) and cbind them together
#'   using mapply. The full matrix can then be given as an input to the
#'   \code{\link{overallPV_interface}} fucntion to calculate the overall
#'   prospect values for each round.
#'
#'   General: The value matrix has ncol = number of attributes you selected or
#'   all(default) and nrow = number of rounds you selected or the first and
#'   last(default) for all selected users.
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
#'   About reference points with cost_ids: For a cost attribute it should be
#'   true, that a lower value is better for the user, this should also hold for
#'   the three reference points. So contrary to normal/benefit attributes \code{
#'   for cost attributes} reference points should follow that: \code{mr > sq >
#'   g}.
#'
#'   Note: When converting a cost attribute to a benefit attribute its three
#'   reference points change as well, enter the unconverted refps, the function
#'   transforms them automatically when it detects a \code{cost_ids != NULL}
#'
#' @return a list of value matrices for each user.
#'
#' @references [1]Wang, X. T.; Johnson, Joseph G. (2012) \emph{A tri-reference
#'   point theory of decision making under risk. }Journal of Experimental
#'   Psychology
#'
#' @examples
#' trpValueMatrix(pc_config_data, 9:11, mr = 0.5, sq = 2, g = 4.7)
#' trpValueMatrix(aDataFrame, userid = 100, rounds = "all", mr = 0.5, sq = 1.8, g = 2.5)
#' trpValueMatrix(my_data, userid = 11, attr = c(1,3,5), cost_ids = 2) #Input accepted but cost_ids = 2 will be ignored
#' trpValueMatrix(my_data, userid = 11, attr =  1, cost_ids = 1, mr = 10, sq = 5, g =3) # Note that for cost attributes: MR > SQ > G
#' trpValueMatrix(keyboard_data, 60, rounds = "first", attr=1, mr = 0.5, sq = 1.8, g = 2.5, beta_f = 6)
#' trpValueMatrix(data1, 2) # Returns an error since no reference points entered (mr, sq, g)
#'
#' @export

trpValueMatrix <- function(dataset, userid = NULL, attr = NULL, rounds = NULL, cost_ids = NULL,
                            mr = 0.5, sq = 1.5, g = 2.5, beta_f = 5, beta_l = 1, beta_g = 1, beta_s = 3) {
  counter <- 0
  if (length(attr) == 1) {
    trp.list <- trpValueMatrix.oneAttr(dataset, userid, attr, rounds, cost_ids,
                           mr, sq, g, beta_f, beta_l, beta_g, beta_s)
  }
  else {
    if (is.null(attr)) attr <- get_attrs_ID(dataset)
    for(i in attr) {
      if (counter == 0) {
        if(i %in% cost_ids) cost_ids_help <- i else cost_ids_help <- NULL
        trp.list <- trpValueMatrix.oneAttr(dataset, userid, attr=i, rounds, cost_ids_help,
                                           mr, sq, g, beta_f, beta_l, beta_g, beta_s)
        counter <- 1
      }
      else {
        if(i %in% cost_ids) cost_ids_help <- i else cost_ids_help <- NULL
        tempVariable <- trpValueMatrix.oneAttr(dataset, userid, attr=i, rounds, cost_ids_help,
                                               mr, sq, g, beta_f, beta_l, beta_g, beta_s)
        trp.list <- mapply(cbind, trp.list, tempVariable, SIMPLIFY = F)
      }
    }
  }
  trp.list
}

#' Returns a Value Matrix using three reference points (one attribute only)
#'
#' This function is a more basic function than \code{trpValueMatrix}, for a
#' detailed descrpition, go to \code{\link{trpValueMatrix}}. This function is
#' based on the value function of the tri-reference point (trp) theory. It first
#' builds a desicion matrix for each user and then applys the trp-value function
#' over each value using the three given reference points (MR, SQ, G) and other
#' four free parameters from the value function. See references.
#'
#' @param dataset data.frame with the user generated data from a product
#'   configurator. See \code{decisionMatrix} for specifications of the dataset.
#' @param userid a vector of integers that gives the information of which users
#'   the matrix should be calculated. Vectorised.
#' @param attr attributes ID, \emph{one integer} corresponding to the attribute
#'   you desire to use; attr are assumed to be 1-indexed.
#' @param rounds integer vector or text option. Which steps of the configuration
#'   process should be shown? Defaults are first and last step. Text options are
#'   \code{all, first, last}.
#' @param cost_ids argument used to convert selected cost attributes into
#'   benefit attributes. In this function, should be the same as \code{attr}.
#'   For a cost attribute it should be true, that a lower value is better for
#'   the user, this should also hold for the three reference points. So contrary
#'   to normal/benefit attributes \code{ for cost attributes} reference points
#'   should follow that: \code{mr > sq > g}.
#' @param mr numeric - Minimum Requirements is the lowest reference point
#' @param sq numeric - Status Quo reference point
#' @param g numeric - Goal reference point
#' @param beta(s) numeric arguments representing the psychological impact of an
#'   outcome equaling failer (_f), loss (_l), gain (_g) or success (_s). Default
#'   values are taken from our reference paper \code{(5,1,1,3)}.
#'
#' @details This function does the same as \code{\link{trpValueMatrix}} but only
#'   for one attribute, for more details please see the mentioned function.
#'
#'   Note: When converting a cost attribute to a benefit attribute its three
#'   reference points change as well, enter the unconverted refps, the function
#'   transforms them automatically when it detects a \code{cost_ids  != NULL}
#'
#' @return a list of value matrices with one attribute for each user.
#'
#' @references Wang, X. T.; Johnson, Joseph G. (2012) \emph{A tri-reference
#'   point theory of decision making under risk. }Journal of Experimental
#'   Psychology
#'
#' @examples
#' trpValueMatrix.oneAttr(pc_config_data, 9:15, attr = 15, mr = -1, sq = 0, g = 2.5)
#' trpValueMatrix.oneAttr(aDataFrame, userid = 100, rounds = "all",  attr = 1, mr = 0.5, sq = 1.8, g = 2.5)
#' trpValueMatrix.oneAttr(myData, 10, attr = 3, cost_ids = 3, mr=4, sq=2, g=0.5) # Note for cost_ids mr > sq > g
#'
#' # Return an error, 1.Too many attributes or 2. none entered
#' trpValueMatrix.oneAttr(keyboard_data, 8:9 , attr = c(10,12,14,16), mr = 0.5, sq = 1.8, g = 2.5)
#' trpValueMatrix.oneAttr(data1, 2) # 2. No attribute entered
#'
#' @export

trpValueMatrix.oneAttr <- function(dataset, userid = NULL, attr = NULL, rounds = NULL, cost_ids = NULL,
                                   mr = 0.5, sq = 1.5, g = 2.5, beta_f = 5, beta_l = 1, beta_g = 1, beta_s = 3) {

  if(length(attr)!= 1) stop("Please insert (only) one attribute ID.")

  if(!is.null(cost_ids)) {
    if(mr <= sq | mr <= g) stop("For cost attributes, since lower is better: Initial MR should be greater or equal to SQ or G")
    if(sq <= g) stop("For cost attributes, since lower is better: SQ cannot be smaller or equal to G")

    mr <- (-1)*mr
    g <- (-1)*g
    sq <- (-1)*sq
  }

  # First Transformation, monotonic transformation such that SQ = 0, this is an imoprtant step
  # see appendix, because it acts as the Gain and Loss matrix. Write it on the Documentation
  # Transform decision Matrix
  list.decMatrices <- decisionMatrix(dataset, userid, attr, rounds, cost_ids)
  list.decMatrices <- lapply(list.decMatrices, function(t) apply(t, 1:2, substract_sq, sq))

  #Transform reference points (substract SQ)
  mr <- substract_sq(mr, sq)
  g <- substract_sq(g, sq)
  sq <- substract_sq(sq, sq)

  if(sq != 0) stop("After first transform, sq != 0, sq = ", sq)

  tri.refps <- c(mr,sq,g)

  # Second Transformation, normalize, first normalize matrices (Normalize a.matrices and b.Refps)
  hmaxVector <- lapply(list.decMatrices, function(temp) apply(temp, 2, abs))
  hmaxVector <- lapply(hmaxVector, function(temp1) if(is.null(ncol(temp1))) {temp1} else {apply(temp1, 2, max)})
  hmaxVector <- lapply(hmaxVector, function(temp2) replace(temp2, temp2==0.0, 1.00)) #remove 0 to avoid NA when dividing
  list.decMatrices <- mapply(function(aMatrix, aVector) aMatrix / aVector[col(aMatrix)], list.decMatrices, hmaxVector, SIMPLIFY = F)

  # Second-Transformartion of reference points, DOCU?? Doesn't affect the user, just how we calculate it.
  tri.refps <- lapply(hmaxVector, function(temp, temp2) temp2/temp, tri.refps)

  valueMatrix <- mapply(trpValueFunction, list.decMatrices, tri.refps, SIMPLIFY = F)
  valueMatrix

}

#' Transform a decision Matrix into a trp value matrix
#'
#' This function is based on the value function of the tri-reference point (trp)
#' theory. It is an auxiliary function, which intends to facilitate the work and
#' readability of \code{\link{trpValueMatrix.oneAttr, trpValueMatrix}}. It takes
#' a matrix and the three given reference points (MR, SQ, G) as a vector
#' \code{tri.refps} and applys the trp value function \code{trpValueFunction} to
#' each element of the matrix. Also takes into account the for free \code{beta}
#' parameters of the function.
#'
#' @param aMatrix a non-empty matrix, tipically with one column since this
#'   function is called one attribute at a time by
#'   \code{trpValueMatrix.oneAttr}.
#' @param mr numeric - Minimum Requirements is the lowest reference point
#' @param sq numeric - Status Quo reference point
#' @param g numeric - Goal reference point
#' @param beta(s) numeric arguments representing the psychological impact of an
#'   outcome equaling failer (_f), loss (_l), gain (_g) or success (_s). Default
#'   values are taken from our reference paper \code{(5,1,1,3)}. See details.
#'
#' @details The functions test for MR < SQ < G
#'
#' The beta arguments are important arguments that give form to the value function proposed in [1].
#' A higher number represents a higher relative psychological impact to the decision maker. Since in [1] it is assumed that the
#' reference point 'Minimum Requierment' has a greater impact when is not reached (failure aversion), it should have a higher beta, so in general
#' \code{beta_f > beta_l > beta_g > beta_s}. See our reference paper for a detailed theoretical background.

#' @return returns a matrix with the outputs of the trp value function for each of its elements
#'
#' @references [1] Wang, X. T.; Johnson, Joseph G. (2012) \emph{A tri-reference
#'   point theory of decision making under risk. }Journal of Experimental
#'   Psychology
#'
#'   [2]Wang, X. T.; Johnson, Joseph G. (2012) \emph{Supplemental Material for: A tri-reference
#'   point theory of decision making under risk. }Journal of Experimental
#'   Psychology
#'
#' @examples # Runnable
#' trpValueFunction(aMatrix = matrix(1:6, 2, 3), triRefps = c(2,3,4.5))
#' trpValueFunction(matrix(1:16, 16, 1), triRefps = c(4, 8.9, 12.5), beta_f = 7)
#'
#' @export

trpValueFunction <- function(aMatrix, triRefps, beta_f = 5, beta_l = 1.5, beta_g = 1, beta_s = 3) {
  mr <- triRefps[1]
  sq <- triRefps[2]
  g <- triRefps[3]

  result <- apply(aMatrix, 1:2, trpValueFunction_extend, mr, sq, g)
  result
}

#' TTri Reference Point Value Function for one element
#'
#' Auxiliary function: it is based on the value function of the tri-reference
#' point (trp) theory. It's called by \code{\link{trpValueFunction}}, it takes
#' one element and puts it through the trp value function as seen in reference
#' [1]. Not vectorised.
#'
#' @param x one numeric value
#' @param mr numeric - Minimum Requirements is the lowest reference point
#' @param sq numeric - Status Quo reference point
#' @param g numeric - Goal reference point
#' @param beta(s) numeric arguments representing the psychological impact of an
#'   outcome equaling failer (_f), loss (_l), gain (_g) or success (_s). Default
#'   values are taken from our reference paper \code{(5,1,1,3)}. See references.
#'
#' @details The functions test for MR < SQ < G.
#'
#'   The beta arguments are important arguments that give form to the value
#'   function proposed in [1]. A higher number represents a higher relative
#'   psychological impact to the decision maker. Since in [1] it is assumed that
#'   the reference point 'Minimum Requierment' has a greater impact when is not
#'   reached (failure aversion), it should have a higher beta, so in general
#'   \code{beta_f > beta_l > beta_g > beta_s}. See our reference paper for a
#'   detailed theoretical background.
#'
#'   On reference points by cost type \code{attr}: For a cost attribute it should be
#'   true, that a lower value is better for the user, this should also hold for
#'   the three reference points. So contrary to normal/benefit attributes \code{
#'   for cost attributes} reference points should follow that: \code{mr > sq >
#'   g}.

#' @return the output of v(x) with v: trp value function([1]).
#'
#' @references [1] Wang, X. T.; Johnson, Joseph G. (2012) \emph{A tri-reference
#'   point theory of decision making under risk. }Journal of Experimental
#'   Psychology
#'
#'   [2]Wang, X. T.; Johnson, Joseph G. (2012) \emph{Supplemental Material for: A tri-reference
#'   point theory of decision making under risk. }Journal of Experimental
#'   Psychology
#'
#' @examples # Runnable
#' trpValueFunction_extend(0.18, mr = 0.15, sq = 0.55, g = 1.10)
#' trpValueFunction_extend(4, mr = 1, sq = 3, g = 8, beta_f = 7, beta_s = 4)
#'
#' @export

trpValueFunction_extend <- function(x, mr = 0.5, sq = 1.5, g = 2.5 , beta_f = 5, beta_l = 1, beta_g = 1, beta_s = 3) {
  if(mr >= sq | mr >= g) stop("MR cannot be greater or equal to SQ or G")
  if(sq >= g) stop("SQ cannot be greater or equal to G")

  if (x < mr) result <- mr*beta_f
  if (x >= mr & x < sq) result <- x*beta_l
  if (x >= sq & x < g) result <- x*beta_g
  if (x >= g) result <- g*beta_s

  result
}

# AUxiliaary function only.
#' cost_ids, enter normal reference points, we will convert them. cost_ids has to equal the attribute you are inputting
#' DOCU: Converting tri.refps, no need to convert if attribute is of type cost.

#' New function as interface with weights and your trp.valueMatrix
#' Docu: For the way the trp function works it is a little more complicated than for overallPV for the pt
#' here we have to manually calculate the AttributeWeights whit your desired function, e.g ww <- getAttrWeights(...)
#' and the trp.ValueMatrix separately as well, trp.VM <- mapply() OR trpValueMatrix(...)
#' then giving to this function as input and getting the desired result
#'
#' DOCU: Explain what _extends is in pC, singalizes major functions that do not take the normal inputs but user
#' other functions' results to work.

#' Runs a simple additive weighting function over matrices
#'
#' Auxiliary function: Takes a matrix and a numeric vector and returns the
#' overall weighted values for each row of the matrix by means of a simple
#' additiv weighting function.
#'
#' @param trp.ValueMatrix generally a \emph{list} of matrices from different
#'   users, such as the output of \code{\link{trpValueMatrix}}. One matrix is
#'   accepted as input but it will be coerced to a list.
#' @param weight generally a \emph{list} of weights from different users, such
#'   as the output of \code{\link{getAttrWeights}}. One vector is also accepted,
#'   if there are more than one matrices, the function will try to recycle the
#'   weight vector.
#'
#'
#' @details The columns of the matrix should be different attributes of a
#'   product or setup and the weight vector should contain a numeric value for
#'   each attribute, so that \code{ncol(trp.ValueMatrix)=length(weight)}. Both
#'   parameters are vectorised so you can enter a list of matrices in
#'   \code{trp.ValueMatrix} and a list of vector in \code{weight}. A matrix in
#'   the first argument or a vector in the second will be coerced into a list.
#'
#'   If some elements of the output list are called \code{$<NA>}, then try to
#'   avoid recycling by checking your \code{weight} input.
#'
#' @return a (list of) vector(s) of overall prospect values
#'
#' @examples #Runnable
#' overallPV_interface(trp.ValueMatrix = matrix(1:8, 2, 4), weight = c(0.25, 0.3, 0.15, 0.3))
#' overallPV_interface(matrix(1:32, 16, 2), c(0.72, 0.25))
#' overallPV_interface(list(m1 = matrix(1:32, 16, 2), m2 = matrix(1:14, 7, 2)),
#'                          weight = c(100, 200)) # weight will be recycled: used on both matrices
#' overallPV_interface(list(m1 = matrix(1:32, 16, 2), m2 = matrix(1:14, 7, 2)),
#'                          list(weight1 = c(100, 200), weight2 = c(20, 50)))
#'
#' #Not Runnable
#' overallPV_interface(aLargeListOfMatrices, weight = c(0.1, 0.2, 0.62, 0.05, 0.03))
#' overallPV_interface(aLargeListOfMatrices, aLargeListOfVectors) #both arguments should have equal length
#' @export

overallPV_interface <- function (trp.ValueMatrix, weight = NULL) {

  if(is.null(weight) | is.null(trp.ValueMatrix)) {
    stop("You need to provide both arguments: trp.ValueMatrix and their weights")
  }
  if(is.vector(weight) & !is.list(weight)) {
    weight <- list("oneVector" = weight)
  }
  if(is.matrix(trp.ValueMatrix) & !is.list(trp.ValueMatrix)) {
    trp.ValueMatrix <- list(oneMatrix = trp.ValueMatrix)
  }

  tryCatchResult = tryCatch({
    trp.overallPV <- mapply(overall_pv_extend, trp.ValueMatrix, weight, SIMPLIFY = F) ##Perhaps mapply when data.frame, make weights as list?!

  }, warning = function(condition) {
    message("Probably amount of users differs from amount of weightVectors and they cannot be recycled.")
  }, error = function(condition) {
    errorText <- paste0("Number of columns on your matrices:", ncol(trp.ValueMatrix[[1]])," differs from the length of at least one weight vector")
    message("Also possible: amount of matrices (users) differs from amount of weightVectors and the latter could not be recycled.")
    stop(errorText)
  }, finally={
  })

  trp.overallPV
}

# Doesn't recquire documentation, only an auxiliary function to transform tri.refps monotonically so that sq = 0
substract_sq <- function(x, status_quo) {
  res <- (-status_quo + x)
  res
}
