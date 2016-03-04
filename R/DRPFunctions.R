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

  #Delete after test
    list.decMatrices <- decisionMatrix(dataset, userid, attr, rounds, cost_ids)
    smallerEqualZeroDM <- lapply(list.decMatrices, greaterThanZero)
    smallerEqualZeroRP <- greaterThanZero(dual.refps)
    smallerEqualZero <- lapply(smallerEqualZeroDM, c, smallerEqualZeroRP)
    smallest <- lapply(smallerEqualZero, min, na.rm = T)
    #print(smallest)
    absOfSmallest <- lapply(smallest, abs)
    absOfSmallestPlus1 <- lapply(absOfSmallest, function(t) t+1)
    #Transform dm and dual.refps if
    list.decMatricesTransf <- mapply("+", list.decMatrices, absOfSmallestPlus1)
    sq_mod <- sq + absOfSmallest[[1]]
    g_mod <- g + absOfSmallest[[1]]
    #print(sq)
    #print(sq_mod)
    #print(g)
    #print(g_mod)
  ##list.decMatricesTransf
  }
  else {
    # Do not transform anything, cost_ids act as usual since negative values are accepted
  }
}
#### Delete later: THis comment marks the state before uniting dual function with smallterThanZero
smallerThanZero <- function(aMatrix, dual.refps) {
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
  gainLossMatrix
}

dualGainLossFunction <- function(aMatrix, dual.refps) {
  x <- aMatrix
  if(any(x < 0)) warning("x should not be smaller than 0.")
  sq <- dual.refps[1]
  g <- dual.refps[2]
  result <- (10/(1+(1/exp(0.5*(x+log(x)-g+log(g))))))-(10/(1+(1/exp(0.5*(sq+log(sq)-g+log(g))))))
  result
}

dualLossAversionFun <- function(...) {}

#Vectorialised if empty, returns NA
greaterThanZero <- function(x) {
  result <- x[x <= 0]
  if (length(result) == 0) result <- NA
  result
}
