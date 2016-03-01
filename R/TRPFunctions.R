#New set of functions for calculating the value matrix of the Tri Reference Point theory and the overall prospect values

## DOCU: Mention that it is a practical problem that trp doesn't use normalized values for its value function, so
## you may have different (mr, sq, g) for each attribute, so you have to run the above function with the attribute
## that have the same values and then manually bind them.

#Main Interface function as in P.10 from Notes

## DOCU cost_ids still works pretty well, but the tri.refps also have to be changed, explain it on BA
## with a nice diagram, mr and g exchange values and change from positive/negative sign.
trpValueMatrix <- function(dataset, userid = NULL, attr = NULL, rounds = NULL, refps = NULL, cost_ids = NULL,
                            mr = 0.5, sq = 1.5, g = 2.5, beta_f = 5, beta_l = 1, beta_g = 1, beta_s = 3) {
  counter <- 0
  if (length(attr) == 1) {
    trp.list <- trpValueMatrix.oneAttr(dataset, userid, attr, rounds, refps, cost_ids,
                           mr, sq, g, beta_f, beta_l, beta_g, beta_s)
  }
  else {
    for(i in attr) {
      if (counter == 0) {
        trp.list <- trpValueMatrix.oneAttr(dataset, userid, attr=i, rounds, refps, cost_ids,
                                           mr, sq, g, beta_f, beta_l, beta_g, beta_s)
        counter <- 1
      }
      else {
        tempVariable <- trpValueMatrix.oneAttr(dataset, userid, attr=i, rounds, refps, cost_ids,
                                               mr, sq, g, beta_f, beta_l, beta_g, beta_s)
        trp.list <- mapply(cbind, trp.list, tempVariable, SIMPLIFY = F)
      }
    }
  }
  trp.list
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

substract_sq <- function(x, status_quo) {
  res <- (-status_quo + x)
  res
}
# AUxiliaary function only.
trpValueMatrix.oneAttr <- function(dataset, userid = NULL, attr = NULL, rounds = NULL, refps = NULL, cost_ids = NULL,
                           mr = 0.5, sq = 1.5, g = 2.5, beta_f = 5, beta_l = 1, beta_g = 1, beta_s = 3) {
  if(length(attr)!= 1) {
    stop("Please insert (only) one attribute ID.")
  }
  # First Transformation, monotonic transformation such that SQ = 0
  # Transform decision Matrix
  list.decMatrices <- decisionMatrix(dataset, userid, attr, rounds, cost_ids)
  list.decMatrices <- lapply(list.decMatrices, function(t) apply(t, 1:2, substract_sq, sq))

  #Transform reference points
  mr <- substract_sq(mr, sq)
  g <- substract_sq(g, sq)
  sq <- substract_sq(sq, sq)

  if(sq != 0) stop("After first transform, sq != 0, sq = ", sq)

  tri.refps <- c(mr,sq,g)

  # Second Transformation, normalize, first normalize matrices
  hmaxVector <- lapply(list.decMatrices, function(temp) apply(temp, 2, abs))
  hmaxVector <- lapply(hmaxVector, function(temp1) apply(temp1, 2, max))
  hmaxVector <- lapply(hmaxVector, function(temp2) replace(temp2, temp2==0.0, 1.00)) #remove 0 to avoid NA when dividing

  list.decMatrices <- mapply(function(aMatrix, aVector) aMatrix / aVector[col(aMatrix)], list.decMatrices, hmaxVector, SIMPLIFY = F)

  #Second-Transformartion of reference points, DOCU?? Doesn't affect the user, just how we calculate it.

  tri.refps <- lapply(hmaxVector, function(temp, temp2) temp2/temp, tri.refps)
  valueMatrix <- mapply(trpValueFunction, list.decMatrices, tri.refps, SIMPLIFY = F)
  valueMatrix
  #print(hmaxVector)
  #print(tri.refps)
  #print(list.decMatrices)

  #list.valueMatrices <- with(bothList, mapply(trpValueFunction, trpMatrix, triRefps, beta_f, beta_l, beta_g, beta_s,
  #                                              SIMPLIFY = F))

  #list.valueMatrices <- mapply(function(temp) apply(temp, 1:2, trpValueFunction, beta_f, beta_l, beta_g, beta_s), list.decMatrices, tri.refps)
  #list.valueMatrices
}


#Delete:
trpValueMatrix22 <- function(dataset, userid = NULL, attr = NULL, rounds = NULL, refps = NULL, cost_ids = NULL,
                         mr = 0.5, sq = 1.5, g = 2.5, beta_f = 5, beta_l = 1, beta_g = 1, beta_s = 3) {
  # First Transformation, monotonic transformation such that SQ = 0
  # Transform decision Matrix
  list.decMatrices <- decisionMatrix(dataset, userid, attr, rounds, cost_ids)
  list.decMatrices <- lapply(list.decMatrices, function(t) apply(t, 1:2, substract_sq, sq))

  #Transform reference points
  mr <- substract_sq(mr, sq)
  g <- substract_sq(g, sq)
  sq <- substract_sq(sq, sq)

  if(sq != 0) stop("After first transform, sq != 0, sq = ", sq)

  # Second Transformation, normalize, first normalize matrices
  hmaxVector <- lapply(list.decMatrices, function(temp) apply(temp, 2, abs))
  hmaxVector <- lapply(hmaxVector, function(temp1) apply(temp1, 2, max))
  hmaxVector <- lapply(hmaxVector, function(temp2) replace(temp2, temp2==0.0, 1.00)) #remove 0 to avoid NA when dividing

  list.decMatrices <- mapply(function(aMatrix, aVector) aMatrix / aVector[col(aMatrix)], list.decMatrices, hmaxVector, SIMPLIFY = F)

  #Second-Transformartion of reference points, DOCU?? Doesn't affect the user, just how we calculate it.
  #hmaxVector
  list.decMatrices

  #list.valueMatrices <- lapply(list.decMatrices, function(temp) apply(temp, 1:2, trpValueFunction,
  #                                        mr, sq, g, beta_f, beta_l, beta_g, beta_s))
  #list.valueMatrices
}
