#New set of functions for calculating the value matrix of the Tri Reference Point theory and the overall prospect values


#Added function pvMatrix_extend to input ngain and nloss was deprecated, but still accessible.
pvMatrix <- function(dataset, userid = NULL, attr = NULL, rounds = NULL, refps = NULL, cost_ids = NULL,
                     alpha = 0.88, beta = 0.88, lambda = 2.25) {
  result <-

  normalizedgainLoss <- norm.gainLoss(dataset, userid, attr, rounds, refps, cost_ids, binded = F)
  pvMatrixList <- with(normalizedgainLoss, mapply(pvalue_fun, gain, loss, alpha, beta, lambda, SIMPLIFY = F))
  pvMatrixList
}
## DOCU: Mention that it is a practical problem that trp doesn't use normalized values for its value function, so
## you may have different (mr, sq, g) for each attribute, so you have to run the above function with the attribute
## that have the same values and then manually bind them.
trpValueFunction <- function(x, mr = 0.5, sq = 1.5, g = 2.5, beta_f = 5, beta_l = 1, beta_g = 1, beta_s = 3) {
  if (x < mr) result <- mr*beta_f
  if (x >= mr & x < sq) result <- x*beta_l
  if (x >= sq & x < g) result <- x*beta_g
  if (x >= g) result <- g*beta_s

  result
}

trpValueMatrix <- function(dataset, userid = NULL, attr = NULL, rounds = NULL, refps = NULL, cost_ids = NULL,
                           mr = 0.5, sq = 1.5, g = 2.5, beta_f = 5, beta_l = 1, beta_g = 1, beta_s = 3) {

  list.decMatrices <- decisionMatrix(dataset, userid, attr, rounds, cost_ids)
  list.valueMatrices <- lapply(list.decMatrices, function(temp) apply(temp, 1:2, trpValueFunction,
                                          mr, sq, g, beta_f, beta_l, beta_g, beta_s))
  list.valueMatrices
}
