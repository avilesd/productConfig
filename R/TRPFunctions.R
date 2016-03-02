#New set of functions for calculating the value matrix of the Tri Reference Point theory and the overall prospect values

## DOCU: Mention that it is a practical problem that trp doesn't use normalized values for its value function, so
## you may have different (mr, sq, g) for each attribute, so you have to run the above function with the attribute
## that have the same values and then manually bind them.

#Main Interface function as in P.10 from Notes

## DOCU cost_ids still works pretty well, but the tri.refps also have to be changed, explain it on BA
## with a nice diagram, mr and g exchange values and change from positive/negative sign.
# cost_ids, enter normal reference points, we will convert them. cost_ids has to equal the attribute you are inputting

#' DOCU: you can enter cost_ids normally, program will recognize for which attr it should use the cost_ids
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

trpValueMatrix.oneAttr <- function(dataset, userid = NULL, attr = NULL, rounds = NULL, cost_ids = NULL,
                                   mr = 0.5, sq = 1.5, g = 2.5, beta_f = 5, beta_l = 1, beta_g = 1, beta_s = 3) {

  if(length(attr)!= 1) stop("Please insert (only) one attribute ID.")

  if(!is.null(cost_ids)) {
    old.mr <- (-1)*mr
    mr <- (-1)*g
    g <- old.mr
    sq <- (-1)*sq
    print(mr)
    print(sq)
    print(g)
  }

  #if(!is.null(cost_ids) & cost_ids != attr) warning("attribute and cost_ids should be for the same attribute...")

  # First Transformation, monotonic transformation such that SQ = 0
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

trpValueFunction <- function(aMatrix, triRefps, beta_f = 5, beta_l = 1, beta_g = 1, beta_s = 3) {
  mr <- triRefps[1]
  sq <- triRefps[2]
  g <- triRefps[3]

  result <- apply(aMatrix, 1:2, trpValueFunction_extend, mr, sq, g)
  result
}

#Rename -- basic functionality
trpValueFunction_extend <- function(x, mr = 0.5, sq = 1.5, g = 2.5 , beta_f = 5, beta_l = 1, beta_g = 1, beta_s = 3) {
  if(mr >= sq | mr >= g) stop("MR cannot be greater or equal to SQ or G")

  if(sq >= g) stop("SQ cannot be greater or equal to G")
  # Add error catching for order of mr < sq < g
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

trp.overallPV_extend <- function (trp.ValueMatrix, weight = NULL) {

  if(is.null(weight) | is.null(trp.ValueMatrix)) {
    stop("You need to provide both arguments: trp.ValueMatrix and their weights")
  }

  tryCatchResult = tryCatch({
    trp.overallPV <- mapply(overall_pv_extend, trp.ValueMatrix, weight, SIMPLIFY = F) ##Perhaps mapply when data.frame, make weights as list?!

  }, warning = function(condition) {
    message("Probably amount of users differs from amount of weightVectors and they cannot be recycled.")
    message("Result most likely not accurrate, check your arguments.")
  }, error = function(condition) {
    stop("The input in the weight parameters can be flexible but check if you are entering the right amount of weightVectors, users and attributes")
  }, finally={
  })

  trp.overallPV
}

# Doesn't recquire documentation, only an auxiliary function
substract_sq <- function(x, status_quo) {
  res <- (-status_quo + x)
  res
}
