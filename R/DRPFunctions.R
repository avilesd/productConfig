# Dual reference point:
# New set of functions for implementing the Dual Reference Model (SQ, Aspirations) and the overall prospect values
# x + ln(x) + ((10/(1+(1/e^(0.5*(x+ln(x)-5+ln(5))))))-(10/(1+(1/e^(0.5*(3+ln(3)-5+ln(5)))))))
# ((10/(1+(1/e^(0.5*(4+ln(4)-15+ln(15))))))-(10/(1+(1/e^(0.5*(3+ln(3)-15+ln(15))))))) x = 4, sq= 3 result = 0.143286
# ((10/(1+(1/e^(0.5*(2+ln(2)-15+ln(15))))))-(10/(1+(1/e^(0.5*(3+ln(3)-15+ln(15))))))) x = 2, sq= 3 result = -0.08188

dualValueMatrix <- function(dataset, userid = NULL, attr = NULL, rounds = NULL, cost_ids = NULL,
                            dual.refps = c(sq=NA, g=NA), consumption_fun = NULL, lambda = 2.25, delta = 0.8) {
  counter <- 0
  if (length(attr) == 1) {
    dual.list <- dualValueMatrix.oneAttr(dataset, userid, attr, rounds, cost_ids,
                                       dual.refps, consumption_fun, lambda, delta)
  }
  else {
    if (is.null(attr)) attr <- get_attrs_ID(dataset)
    for(i in attr) {
      if (counter == 0) {
        if(i %in% cost_ids) cost_ids_help <- i else cost_ids_help <- NULL
        dual.list <- dualValueMatrix.oneAttr(dataset, userid, attr=i, rounds, cost_ids_help,
                                             dual.refps, consumption_fun, lambda, delta)
        counter <- 1
      }
      else {
        if(i %in% cost_ids) cost_ids_help <- i else cost_ids_help <- NULL
        tempVariable <- dualValueMatrix.oneAttr(dataset, userid, attr=i, rounds, cost_ids_help,
                                                 dual.refps, consumption_fun, lambda, delta)
        dual.list <- mapply(cbind, dual.list, tempVariable, SIMPLIFY = F)
      }
    }
  }
  dual.list
}

dualValueMatrix.oneAttr <- function(dataset, userid = NULL, attr = NULL, rounds = NULL, cost_ids = NULL,
                                    dual.refps = c(sq=NA, g=NA), consumption_fun = NULL, lambda = 2.25, delta = 0.8) {
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

  }
  valueMatrices
}
#### Delete later: THis comment marks the state before uniting dual function
#with smallterThanZero
#@smallerThanZero, we decided to normalize just after calculating gains and
#losses, it is not 100% reliable, but much better than normalizing before (raw
#data) or after(value matrix) because you loose the information about 1.
#distance to the sq and 2. loss aversion effect, respectivly.
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
  norm.gainLossMatrix <- gainLossMatrix/hmaxVector
  norm.gainLossMatrix

  dual.vMatrix <- dualLossAversionFun(norm.gainLossMatrix, aMatrix, lambda, delta)
  dual.vMatrix

  #Call from here and return valueMatrix, give argument to return gain? or another equal function?
}

dualGainLossFunction <- function(aMatrix, dual.refps) {
  x <- aMatrix
  if(any(x < 0)) warning("x should not be smaller than 0.")
  sq <- dual.refps[1]
  g <- dual.refps[2]
  result <- (10/(1+(1/exp(0.5*(x+log(x)-g+log(g))))))-(10/(1+(1/exp(0.5*(sq+log(sq)-g+log(g))))))
  result
}

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
