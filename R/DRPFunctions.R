# Dual reference point:
# New set of functions for implementing the Dual Reference Model (SQ, Aspirations) and the overall prospect values
# x + ln(x) + ((10/(1+(1/e^(0.5*(x+ln(x)-5+ln(5))))))-(10/(1+(1/e^(0.5*(3+ln(3)-5+ln(5)))))))
# ((10/(1+(1/e^(0.5*(4+ln(4)-15+ln(15))))))-(10/(1+(1/e^(0.5*(3+ln(3)-15+ln(15))))))) x = 4, sq= 3 result = 0.143286
# ((10/(1+(1/e^(0.5*(2+ln(2)-15+ln(15))))))-(10/(1+(1/e^(0.5*(3+ln(3)-15+ln(15))))))) x = 2, sq= 3 result = -0.08188

dualValueMatrix.oneAttr <- function(dataset, userid = NULL, attr = NULL, rounds = NULL, cost_ids = NULL,
                                    dual.refps = c(sq=NA, g=NA), consumption_fun = NULL, lambda = 2.25, delta = 0.8) {
  sq <- dual.refps[1]
  g <- dual.refps[2]

  if(length(attr)!= 1) stop("Please insert (only) one attribute ID.")
  if(is.na(sq) | is.na(g)) stop("Please provide both reference points (sq, g)")

  # This function does not checks if sq < g, since not required in the model

  if (is.null(consumption_fun)) {
    # Check if negative, through peeking data and cost_ids (already applied in dM), cost_ids not need explicit.
    # When all positive procede with f1 and then f2
  }
  else {
    # Do not transform anything, cost_ids act as usual since negative values are accepted
  }

  if(!is.null(cost_ids)) {
    if(mr <= sq | mr <= g) stop("For cost attributes, since lower is better: Initial MR should be greater or equal to SQ or G")
    if(sq <= g) stop("For cost attributes, since lower is better: SQ cannot be smaller or equal to G")

    mr <- (-1)*mr
    g <- (-1)*g
    sq <- (-1)*sq
  }

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
