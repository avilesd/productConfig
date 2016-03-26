#'Weights calculating function
#'
#'This function gives you an interface to all weight calculating functions in
#'this package. The different methods and functions are:
#'\code{\link{weight.differenceToIdeal}}, \code{\link{weight.entropy}},
#'\code{\link{weight.differenceToIdeal}}, \code{\link{weight.highAndStandard}},
#'\code{\link{weight.standard}}. You also have the ability to enter your own
#'weights for each \code{userid}. This can be done through a list with the
#'exacht same length as the number of users you input. If you want to use the
#'same weight vector for all users you can enter it as a numeric vector, ideally
#'with \code{length(weight) == length(attr)}.
#'
#'The function is vectorised on \code{userid}. If you decide to enter your own
#'weights, and not calculating them, note that the function accepts a weight
#'vector with negative values and not adding up to 1. It is up to the user to
#'check if that makes sense.
#'
#'Lastly, this function is called from within \code{\link{overallPV}} and as in
#'that function, the \code{attr} parameter does not sort its input, so check
#'that if and when inputting your weights they correspond to the order you
#'entered the attributes.
#'
#'@inheritParams weight.differenceToIdeal
#'
#'@param cost_ids argument used to convert selected cost attributes into benefit
#'  attributes. Integer vector.
#'
#'@param weight numeric vector. Represents the importance or relevance that an
#'  attribute has and the weight it should have in the calculation of the
#'  prospect value.
#'
#'@param weightFUN indicated which weight function should be used to calculate
#'  the weight vector, the options are \code{"differenceToIdeal", "entropy",
#'  "highAndStandard", "standard"}. Default ist \code{differencetoIdeal}.
#'
#'@param gamma numeric and between 0 and 1. It is a parameter used for the function
#'  \code{\link{weight.highAndStandard}} and
#'  \code{\link{highAndStandard}}, please the former to know its use.
#'
#'@details Default value of FUN uses \code{\link{differenceToIdeal}} to get the
#'  \code{weight} vector.
#'
#'  \code{attr} This function does handles different amount of attributes.
#'
#'  \code{cost_ids} Are passed along to the function you chosee, not directly
#'  handled here.
#'
#'  If you want to know more about the other parameters, look at
#'  \code{decisionMatrix}.
#'
#'@return Calculated weights according to the chosen weight function
#'
#' @examples #Not Runnable yet
#' getAttrWeights(pc_data, 11)
#' getAttrWeights(myData, 11, weightFUN = "entropy")
#' getAttrWeights(my_data, userid = 10:10, attr=1:3, cost_ids = 3)
#' getAttrWeights(monitor_data, 50, rounds="all", weightFUN="highAndStandard", gamma=0.8)
#' getAttrWeights(myData, userid = 9, attr= 1:5, weight=c(0.20, 0.10, 0.05, 0.40, 0.25))
#'
#'@family weight functions
#'
#'@export

getAttrWeights <- function(dataset = NULL, userid = NULL, weight = NULL,  attr = NULL, rounds = NULL, cost_ids = NULL,
                           weightFUN = "differenceToIdeal", gamma = 0.5) {
  if((is.null(dataset) | is.null(userid)) & is.null(weight)) {
    stop("You need to provide the weights ('weight =') or userids + dataset for them to be calculated.")
  }
  if(is.vector(weight) & !is.list(weight)) {
    weight <- list("oneVector" = weight)
  }
  if(!is.vector(weight) & !is.list(weight) & !is.null(weight)) {
    stop("Input in weight parameter needs to be a list of numeric vectors or just one vector.")
  }

  if(is.null(weight)) {
    # ! ToDo check if functions handle correctly inputting userids missing. Old weight_sum_value calls other f(x) that do
    ## DOCU: New functions must take into account attributes and calculate accordingly, perhaps it doesn't make sense with our data,
    ## but we have to give the choice
    if (weightFUN == "differenceToIdeal") {
      result <- weight.differenceToIdeal(dataset, userid, attr, rounds, cost_ids)
    }
    if (weightFUN == "entropy") {
      result <- weight.entropy(dataset, userid, attr, rounds, cost_ids)
    }
    if (weightFUN == "highAndStandard") {
      result <- weight.highAndStandard(dataset, userid, attr, rounds, cost_ids, gamma)
    }
    if (weightFUN == "standard") {
      result <- weight.standard(dataset, userid, attr, rounds, cost_ids)
    }
  }
  else { result <- weight}
  result
}

## DOCU: New functions must take into account attributes and calculate accordingly, perhaps it doesn't make sense with our data,
## but we have to give the choice

#' Calculates attribute weights using the 'objective approach'
#'
#' This function first normalizes a list of matrices and then calculates the
#' decision weight for each attribute, using the 'objective approach' as given
#' by [1] and [2]. The objective approach, in this case, uses only data gathered
#' from the decision matrix and it does not need a 'subjective' preference
#' matrix from the decision maker. The sum of a weight vector should always
#' equal 1.
#'
#' The result is a list of vectors, each vector with the same length as the
#' number of columns of the input matrices, i.e. each column gets a weight.
#'
#' @param dataset data.frame with the user generated data from a product
#'   configurator. See \code{decisionMatrix} for specifications of the dataset.
#' @param userid a vector of integers that gives the information of which users
#'   the matrix should be calculated. Vectorised.
#' @param attr attributes IDs, vector of integer numbers corresponding to the
#'   attributes you desire to use; attr are assumed to be 1-indexed. This
#'   function will calculate with all attributes and do the subsetting a
#'   posteriori.
#'
#'   If you want to get the weights for only two attributes you will have to
#'   first use \code{\link{decisionMatrix}} and then pass it on to
#'   \code{\link{normalize.altMethod}} and \code{\link{differenceToIdeal}}.
#' @param rounds integer vector or text option. Which steps of the configuration
#'   process should be taken into account? Defaults are "all" in order to have
#'   more data to calculate with. If \code{"first"} or \code{"last"} are entered
#'   there will be only one rounds to gather data from, consequently all
#'   attribtues will have the same weight.
#' @param cost_ids argument used to convert selected cost attributes into
#'   benefit attributes. Integer vector. This functions uses an alternative
#'   normalizing method \code{\link{normalize.altMethod}} that does not produce
#'   negative values.
#'
#' @details This function rewards attributes which values do not change much
#'   throughtout the decision matrix, even if the value is the lowest value for
#'   that attribute. For an opposite implicit effect see
#'   \code{\link{weight.entropy}}
#'
#'   \code{cost_ids} As in the other functions, if you enter a cost_ids that is
#'   not in your entered attributes, the functions will calculate the output
#'   with all attributes in your data, including the cost(s) attributes and only
#'   after the calculations does the function subset the result according to the
#'   \code{attr}. When the attributes and cost_ids differ, the function allows
#'   the calculation but it will throw a warning.
#'
#' @return a list of weight vector(s)
#'
#' @references [1]Ma, J., Fan, Z. P., & Huang, L. H. (1999). A subjective and
#'   objective integrated approach to determine attribute weights. European
#'   journal of operational research, 112(2), 397-404. [2] Fan, Z. P. (1996).
#'   Complicated multiple attribute decision making: theory and applications
#'   (Doctoral dissertation, Ph. D. Dissertation, North-eastern University,
#'   Shenyang, PRC).
#'
#' @examples # Not runnable yet
#' weight.differenceToIdeal(myData, 15:22)
#' weight.differenceToIdeal(laptop_data, 40:45, attr= c(1, 3, 4), cost_ids = 4)
#' savedWeights <- weight.differenceToIdeal(myData, c(6, 15, 18, 20, 26), attr = 1:4, cost_ids = 4, rounds=1:8)
#' lapply(savedWeights, sum) # Should return 1 for any output of this function
#'
#' @export

weight.differenceToIdeal <- function(dataset, userid = NULL , attr = NULL, rounds = "all", cost_ids = NULL) {

  # Common Errors catched in dM (Tested: attr, rounds, all.users, cost_ids)
  if (is.null(attr)) attr <- get_attrs_ID(dataset)
  if (!all(cost_ids %in% attr)) warning("One of your cost_ids is not in your attributes. Is this still your intended result?")

  decisionList <- decisionMatrix(dataset, userid, attr = NULL, rounds)
  normList <- lapply(decisionList, normalize.altMethod, attr, cost_ids)
  weightList <- lapply(normList, differenceToIdeal, attr)
  weightList <- lapply(weightList, function(t) t[attr])
  weightList
}

#' Calculates decision weights using the 'objective approach'
#'
#' This function is used as the second step in
#' \code{\link{weight.differenceToIdeal}} for calculating a decision weight for
#' each \code{\link{attr}} in the decision matrix. The methodology of the
#' 'objective approach' for determining the weights is given by references [1]
#' and [2]. See Details.
#'
#' The sum of the output of this functions should always equal 1.
#'
#' @param normalizedMatrix a numeric matrix. If indeed normalized it should only
#'   contain values between \code{0} and \code{1}.
#' @param attr attributes IDs, vector of integer numbers corresponding to the
#'   attributes you desire to use; attr are assumed to be 1-indexed. Here it
#'   represents the number of columns of the input matrix.
#' @details It measures the distance of each column value against the best value
#'   for a given attribute. A smaller difference should mean that for that attribute a
#'   high value was \emph{consistently} taking in consideration, thus resulting
#'   in a higher weight. Two unintended consequences are: 1. matrices with one
#'   row will result in the same weight for all columns and 2. for an attribute
#'   where the value does not change at all (even if it's a low value) the
#'   function will reward it somewhat disproportionately.
#'
#' @return a decision weight (numeric vector with a sum of 1)
#'
#' @references [1]Ma, J., Fan, Z. P., & Huang, L. H. (1999). A subjective and
#'   objective integrated approach to determine attribute weights. European
#'   journal of operational research, 112(2), 397-404.
#'
#'   [2] Fan, Z. P. (1996). Complicated multiple attribute decision making:
#'   theory and applications (Doctoral dissertation, Ph. D. Dissertation,
#'   North-eastern University, Shenyang, PRC).
#'
#' @examples #Runnable
#' differenceToIdeal(matrix(c(1.0, 0.85, 0.42, 0, 0.5, 0, 1, 0.7), 4, 2), attr=1:4)
#' weights <- differenceToIdeal(matrix(c(1.0, 0.85, 0.42, 0, 0.5, 0, 1, 0.7), 2, 4), attr=1:4)
#' sum(weights) should return 1
#'
#' @export

differenceToIdeal <- function(normalizedMatrix, attr) {
  vector2_0 <- apply(normalizedMatrix, 2, function(t) { b_max <- max(t); sumOfDiff <- (sum((b_max - t)^2))  })
  vector2_0 <- replace(vector2_0, vector2_0==0.0, 1/(length(attr)/2)) # Avoid divide by zero, the 2 regulates the huge impact a none-changing value has on the weights
  vector2_1 <- 1/vector2_0
  vector3 <- sum(vector2_1)
  vector3 <- replace(vector3, vector3==0.0, 1)
  weightVector <- vector2_1/vector3
  weightVector
}

#' Calculates decision weights using the entropy method
#'
#' This function first normalizes a list of matrices and then calculates the
#' decision weight for each attribute, using an entropy approach [1, 2], which
#' can be categorised as an objective approach, just as
#' \code{\link{weight.differenceToIdeal}}. This type of weight functions use
#' only the information within the decision matrix to calculate weights. It does
#' not need information about the decision maker's preferences.
#'
#' The result is a list of vectors, each vector with the same length as the
#' number of columns of the input matrices, i.e. each column gets a weight. The
#' sum of a weight vector should always equal 1.
#'
#' @param dataset data.frame with the user generated data from a product
#'   configurator. See \code{decisionMatrix} for specifications of the dataset.
#' @param userid a vector of integers that gives the information of which users
#'   the matrix should be calculated. Vectorised.
#' @param attr attributes IDs, vector of integer numbers corresponding to the
#'   attributes you desire to use; attr are assumed to be 1-indexed. This
#'   function will calculate with all attributes and do the subsetting a
#'   posteriori.
#'
#'   If you want to get the weights for only two attributes you will have to
#'   first use \code{\link{decisionMatrix}} and then pass it on to
#'   \code{\link{normalize.sum}} and \code{\link{entropy}}.
#'
#' @param rounds integer vector or text option. Which steps of the configuration
#'   process should be taken into account? Defaults are "all" in order to have
#'   more data to calculate with. If \code{"first"} or \code{"last"} are entered
#'   there will be only one rounds to gather data from, consequently all
#'   attribtues will have the same weight.
#'
#' @param cost_ids argument used to convert selected cost attributes into
#'   benefit attributes. Integer vector. This functions uses an alternative
#'   normalizing method \code{\link{normalize.sum}} that does not produce
#'   negative values. However, it has one limitation, if within a same attribute
#'   there are negative and positive values, the function will likely produce a
#'   \code{NaN}.
#'
#' @details In contrast to \code{\link{weight.differenceToIdeal}}, this function
#'   distributes lower weights to those attributes, which have similar values
#'   throughout the decision matrix.
#'
#'   Note: the normalizing function used here \code{\link{normalize.sum}} has
#'   one limitation, if within a same attribute there are negative and positive
#'   values, the function will likely produce a \code{NaN}.
#'
#'   \code{cost_ids} As in the other functions, if you enter a cost_ids that is
#'   not in your entered attributes, the functions will calculate the output
#'   with all attributes in your data, including the cost(s) attributes and only
#'   after the calculations does the function subset the result according to the
#'   \code{attr}. When the attributes and cost_ids differ, the function allows
#'   the calculation but it will throw a warning.
#'
#' @return a list of weight vector(s)
#'
#' @references [1]Hwang, C. L., & Yoon, K. (2012). Multiple attribute decision
#'   making: methods and applications a state-of-the-art survey (Vol. 186).
#'   Springer Science & Business Media.
#'
#'   [2]Shannon, C. E. (2001). A mathematical theory of communication. ACM
#'   SIGMOBILE Mobile Computing and Communications Review, 5(1), 35.
#'
#'   [3]Lotfi, F. H., & Fallahnejad, R. (2010). Imprecise Shannon’s entropy and
#'   multi attribute decision making. Entropy, 12(1), 53-62.
#'
#' @examples # Not runnable yet
#' weight.entropy(myData, 15:22)
#' weight.entropy(laptop_data, 40:45, attr= c(1, 3, 4), cost_ids = 4)
#' savedWeights <- weight.entropy(myData, c(6, 15, 18, 20, 26), attr = 1:4, cost_ids = 4, rounds=1:8)
#' lapply(savedWeights, sum) # Should return 1 for any output of this function
#'
#' @export

weight.entropy <- function(dataset, userid = NULL , attr = NULL, rounds = "all", cost_ids = NULL) {

  # Common Errors catched in dM (Tested: attr, rounds, all.users, cost_ids)
  if (is.null(attr)) attr <- get_attrs_ID(dataset)
  if (!all(cost_ids %in% attr)) warning("One of your cost_ids is not in your attributes. Is this still your intended result?")

  decisionList <- decisionMatrix(dataset, userid, attr = NULL, rounds, cost_ids)
  normList <- lapply(decisionList, normalize.sum)
  weightList <- lapply(normList, entropy)
  weightList <- lapply(weightList, function(t) t[attr])
  weightList
}

#' Calculates decision weights using the entrophy method
#'
#' This function is used as the second step in \code{\link{weight.entropy}} for
#' calculating a decision weight for each \code{\link{attr}} in the decision
#' matrix. The methodology of the entropy method [2] for determining the weights
#' out of a decision matrix is given by references [1] and [3]. See Details.
#'
#' The sum of the output of this functions should always equal 1.
#'
#' @param normalizedMatrix a numeric matrix. If indeed normalized it should only
#'   contain values between \code{0} and \code{1}.
#'
#' @details Contrasting with \code{\link{differenceToIdeal}} small differences
#'   between value attributes are rewarded a lower value and thus a relative
#'   lower weight.
#'
#'   Note: the normalizing function used here \code{\link{normalize.sum}} has
#'   one limitation, if within a same attribute there are negative and positive
#'   values, the function will likely produce a \code{NaN}.
#'
#' @return a decision weight (numeric vector with a sum of 1)
#'
#' @references [1]Hwang, C. L., & Yoon, K. (2012). Multiple attribute decision
#'   making: methods and applications a state-of-the-art survey (Vol. 186).
#'   Springer Science & Business Media.
#'
#'   [2]Shannon, C. E. (2001). A mathematical theory of communication. ACM
#'   SIGMOBILE Mobile Computing and Communications Review, 5(1), 35.
#'
#'   [3]Lotfi, F. H., & Fallahnejad, R. (2010). Imprecise Shannon’s entropy and
#'   multi attribute decision making. Entropy, 12(1), 53-62.
#'
#' @examples #Runnable
#' entropy(matrix(c(1.0, 0.85, 0.42, 0, 0.5, 0, 1, 0.7), 4, 2))
#' weights <- entropy(matrix(c(1.0, 0.85, 0.42, 0, 0.5, 0, 1, 0.7), 2, 4))
#' sum(entropy) should return 1
#'
#' @export

entropy <- function(normalizedMatrix) {
  #One column is unlikely since weight should be 1, one row is likely, catch row
  if(!is.matrix(normalizedMatrix)) stop("Input must be a matrix")
  if (nrow(normalizedMatrix)==1) {
    numberCol <- ncol(normalizedMatrix)
    weightVector <- rep(1, numberCol)
    weightVector <- weightVector/numberCol
  }
  else {
    normalizedMatrix <- replace(normalizedMatrix, normalizedMatrix==0, 1) #As in paper, log 0 should be equal 0 which is = log(1)
    kk <- 1/(nrow(normalizedMatrix)/2)
    e_j_secondTerm <- apply(normalizedMatrix, 2, function(x) sum(x*log(x)))
    e_j <- -kk*e_j_secondTerm
    d_j <- 1-e_j
    sumOfd_j <- sum(d_j)
    weightVector <- d_j/sumOfd_j
  }
  weightVector
}

#' Calculates decision weights using the standard deviation
#'
#' This function first normalizes a list of matrices and then calculates the
#' decision weight for each attribute, using the standard deviation method[1,
#' 2], which can be categorised as an objective approach, just as
#' \code{\link{weight.differenceToIdeal}} and \code{\link{weight.entropy}}.
#'
#' The result is a list of vectors, each vector with the same length as the
#' number of columns of the input matrices, i.e. each column gets a weight. The
#' sum of a weight vector should always equal 1.
#'
#' @param dataset data.frame with the user generated data from a product
#'   configurator. See \code{decisionMatrix} for specifications of the dataset.
#' @param userid a vector of integers that gives the information of which users
#'   the matrix should be calculated. Vectorised.
#' @param attr attributes IDs, vector of integer numbers corresponding to the
#'   attributes you desire to use; attr are assumed to be 1-indexed. This
#'   function will calculate with all attributes and do the subsetting a
#'   posteriori.
#'
#'   If you want to get the weights for only two attributes you will have to
#'   first use \code{\link{decisionMatrix}} and then pass it on to
#'   \code{\link{normalize.sum}} and \code{\link{entropy}}.
#'
#' @param rounds integer vector or text option. Which steps of the configuration
#'   process should be taken into account? Defaults are "all" in order to have
#'   more data to calculate with. If \code{"first"} or \code{"last"} are entered
#'   there will be only one rounds to gather data from, consequently all
#'   attribtues will have the same weight.
#'
#' @param cost_ids argument used to convert selected cost attributes into
#'   benefit attributes. Integer vector. This functions uses an alternative
#'   normalizing method \code{\link{normalize.altMethod}} that does not produce
#'   negative values.
#'
#' @details In contrast to \code{\link{weight.differenceToIdeal}}, this function
#'   distributes lower weights to those attributes, which have similar values
#'   throughout the decision matrix.
#'
#'   Note: the normalizing function used here \code{\link{normalize.sum}} has
#'   one limitation, if within a same attribute there are negative and positive
#'   values, the function will likely produce a \code{NaN}.
#'
#'   \code{cost_ids} As in the other functions, if you enter a cost_ids that is
#'   not in your entered attributes, the functions will calculate the output
#'   with all attributes in your data, including the cost(s) attributes and only
#'   after the calculations does the function subset the result according to the
#'   \code{attr}. When the attributes and cost_ids differ, the function allows
#'   the calculation but it will throw a warning.
#'
#' @return a list of weight vector(s)
#'
#' @references [1]Diakoulaki, D., Mavrotas, G., & Papayannakis, L. (1995).
#'   Determining objective weights in multiple criteria problems: the CRITIC
#'   method. Computers & Operations Research, 22(7), 763-770.
#'
#'   [2]Jahan, A., & Edwards, K. L. (2013). Multi-criteria decision analysis for
#'   supporting the selection of engineering materials in product design.
#'   Butterworth-Heinemann.
#'
#' @examples # Not runnable yet
#' weight.standard(myData, 15:22)
#' weight.standard(laptop_data, 40:45, attr= c(1, 3, 4), cost_ids = 4)
#' savedWeights <- weight.standard(myData, c(6, 15, 18, 20, 26), attr = 1:4, cost_ids = 4, rounds=1:8)
#' lapply(savedWeights, sum) # Should return 1 for any output of this function
#'
#' @export

weight.standard <- function(dataset, userid = NULL , attr = NULL, rounds = "all", cost_ids = NULL) {

  # Common Errors catched in dM (Tested: attr, rounds, all.users, cost_ids)
  if (is.null(attr)) attr <- get_attrs_ID(dataset)
  if (!all(cost_ids %in% attr)) warning("One of your cost_ids is not in your attributes. Is this still your intended result?")

  decisionList <- decisionMatrix(dataset, userid, attr = NULL, rounds)
  normList <- lapply(decisionList, normalize.altMethod, attr, cost_ids) # altMethod chosen because gives more weight to lots of changes.
  weightList <- lapply(normList, standardDeviation)
  weightList <- lapply(weightList, function(t) t[attr])
  weightList
}

#' Calculates decision weights using the standard deviation method
#'
#' This function is used as the second step in \code{\link{weight.standard}} for
#' calculating a decision weight for each \code{\link{attr}} in the decision
#' matrix. The methodology of this method for determining the weights
#' out of a decision matrix is given by references [1] and [2]. See References.
#'
#' The sum of the output of this functions should always equal 1.
#'
#' @param normalizedMatrix a numeric, normalized matrix. If indeed normalized it should only
#'   contain values between \code{0} and \code{1}.
#'
#' @details Similar to \code{\link{entropy}} small differences
#'   between value attributes are rewarded a lower value and thus a relative
#'   lower weight.
#'
#' @return a weight vector(numeric vector with a sum of 1)
#'
#' @references [1]Diakoulaki, D., Mavrotas, G., & Papayannakis, L. (1995).
#'   Determining objective weights in multiple criteria problems: the CRITIC
#'   method. Computers & Operations Research, 22(7), 763-770.
#'
#'   [2]Jahan, A., & Edwards, K. L. (2013). Multi-criteria decision analysis for
#'   supporting the selection of engineering materials in product design.
#'   Butterworth-Heinemann.
#'
#' @examples #Runnable
#' standardDeviation(matrix(c(1.0, 0.85, 0.42, 0, 0.5, 0, 1, 0.7), 4, 2))
#' weights <- standardDeviation(matrix(c(1.0, 0.85, 0.42, 0, 0.5, 0, 1, 0.7), 2, 4))
#' sum(weights) should return 1
#'
#' @export

standardDeviation <- function(aMatrix) {
  #One column is unlikely since weight should be 1, one row is likely, catch row
  if(!is.matrix(aMatrix)) stop("Input must be a matrix")
  if (nrow(aMatrix)==1) {
    numberCol <- ncol(aMatrix)
    weightVector <- rep(1, numberCol)
    weightVector <- weightVector/numberCol
  }
  else {
    deviations <- apply(aMatrix, 2, sd)
    sumOfDeviations <- sum(deviations)
    weightVector <- deviations/sumOfDeviations
  }
  weightVector
}


#' Calculates weights relative to the highest sum relative to other attributes
#'
#' Awards higher weights to those attirbutes which consistently showed higher
#' values, measured against the highest posssible. Secondary function used for
#' main function \code{\link{weight.highAndStandard}}.
#'
#' @param dataset data.frame with the user generated data from a product
#'   configurator. See \code{decisionMatrix} for specifications of the dataset.
#' @param userid a vector of integers that gives the information of which users
#'   the matrix should be calculated. Vectorised.
#' @param attr attributes IDs, vector of integer numbers corresponding to the
#'   attributes you desire to use; attr are assumed to be 1-indexed. This
#'   function will calculate with all attributes and do the subsetting a
#'   posteriori.
#'
#'   If you want to get the weights for only two attributes you will have to
#'   first use \code{\link{decisionMatrix}} and then pass it on to
#'   \code{\link{normalize.sum}} and \code{\link{entropy}}.
#'
#' @param rounds integer vector or text option. Which steps of the configuration
#'   process should be taken into account? Defaults are "all" in order to have
#'   more data to calculate with. If \code{"first"} or \code{"last"} are entered
#'   there will be only one rounds to gather data from, consequently all
#'   attribtues will have the same weight.
#'
#' @param cost_ids argument used to convert selected cost attributes into
#'   benefit attributes. Integer vector. This functions uses an alternative
#'   normalizing method \code{\link{normalize.highestValue}} that does not produce
#'   negative values.
#'
#' @return a list of weight vector(s)
#'
#' @export

weight.highestValue <- function(dataset, userid = NULL , attr = NULL, rounds = "all", cost_ids = NULL) {
  # Common Errors catched in dM (Tested: attr, rounds, all.users, cost_ids)
  fullAttr <- get_attrs_ID(dataset)
  if (is.null(attr)) attr <- get_attrs_ID(dataset)
  if (!all(cost_ids %in% attr)) warning("One of your cost_ids is not in your attributes. Is this still your intended result?")

  maxValuePerAttr <- sapply(getAttrValues(myData, fullAttr), max, USE.NAMES = F)
  minValuePerAttr <- sapply(getAttrValues(myData, fullAttr), min, USE.NAMES = F) # provides them as is, without sorting
  decisionList <- decisionMatrix(dataset, userid, attr = NULL, rounds)
  decisionList <- lapply(decisionList, function(t, maxV, minV) {rbind(maxV, minV , t)} , maxValuePerAttr, minValuePerAttr)

  normList <- lapply(decisionList, normalize.highestValue, fullAttr, cost_ids)
  weightList <- lapply(normList, highestValue)
  weightList <- lapply(weightList, function(t) t[attr])
  weightList
}

#' Calculates weights relative to the highest sum relative to other attributes
#'
#' Awards higher weights to those attirbutes which consistently showed higher
#' values, measured against the highest posssible. Secondary function used for
#' main function  \code{\link{weight.highAndStandard}}.
#'
#' @param normalizedMatrix
#'
#' @return a weight vector (numeric vector with a sum of 1)
#'
#' @examples #Runnable
#' highestValue(matrix(c(1.0, 0.85, 0.42, 0, 0.5, 0, 1, 0.7), 4, 2))
#' weights <- highestValue(matrix(c(1.0, 0.85, 0.42, 0, 0.5, 0, 1, 0.7), 2, 4))
#' sum(weights) should return 1
#' @export

highestValue <- function(normalizedMatrix) {
  #One column is unlikely since weight should be 1, one row is likely, catch row
  if(!is.matrix(normalizedMatrix)) stop("Input must be a matrix")
  if (nrow(normalizedMatrix)==1) {
    numberCol <- ncol(normalizedMatrix)
    weightVector <- rep(1, numberCol)
    weightVector <- weightVector/numberCol
  }
  else {
    sumOfColumns <- apply(normalizedMatrix, 2, sum)
    sumTotal <- sum(sumOfColumns)
    weightVector <- sumOfColumns/sumTotal
  }
  weightVector
}

#' Calculates weights using two weighted sub-functions
#'
#' This function calculates two separate weight vectors and merges them together
#' with a weighted parameter \code{gamma}.
#'
#' The first weight function \code{\link{weight.highestValue}} rewards those
#' attributes which have values (consistenly) closer to the highest possible value.
#' The second function \code{\link{weight.standard}} uses the standard deviation
#' to assign weights. The more the values differ from one another within an attribute, the better the
#' assigned weight will be.
#'
#' The \code{gamma} parameter measures the importance you want to give to the first
#' function. It acts also as a weight since the final weight vector is given by
#' \code{result = gamma * weight.highestValue + (1-gamma) * weight.standard}
#'
#' @inheritParams getAttrWeights
#'
#' @return a list of weight vectors (one per user)
#'
#' @examples #Not runnable yet
#' weight.highAndStandard(myData, userid=10)
#' weight.highAndStandard(someData, 11, rounds="all")
#' weight.highAndStandard(laptop_data, 15, cost_ids=4, gamma = 0.3)
#' weight.highAndStandard(myData, 15, attr=1:4, "all", cost_ids=4, gamma = 0.75)
#'
#' @export

#-----------------------------------------------------
# Just for testing purposes
weight.highAndStandard <- function(dataset, userid = NULL , attr = NULL, rounds = "all", cost_ids = NULL, gamma = 0.5) {
  weight1 <- weight.highestValue(dataset, userid, attr, rounds, cost_ids)
  weight2 <- weight.standard(dataset, userid, attr, rounds, cost_ids)
  weightVector <- mapply(function(w1, w2) gamma*w1 + (1-gamma)*w2, weight1, weight2, SIMPLIFY = F)
  weightVector
}

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
# Just for testing purposes
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
