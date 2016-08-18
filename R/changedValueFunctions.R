norm.gainLoss_new <- function(dataset, userid = NULL, attr = NULL, rounds = NULL, refps = NULL, cost_ids = NULL, binded = T) {

  desList <- containedTables1
  refPs <- referencePoints(dataset, userid, refps, attr, cost_ids)

  #Save names
  namesOfCols <- colnames(desList[[1]])
  namesOfRows <- lapply(desList, function(temp) row.names(temp))

  tMatrixList <- lapply(desList, t)

  gainVector <- mapply(gainFunction, tMatrixList, refPs, SIMPLIFY = F)
  lossVector <- mapply(lossFunction, tMatrixList, refPs, SIMPLIFY = F)

  # Using matrix and lapply is harder, because different amount of rounds and therefore rows.
  gainList <- mapply(function(temp5, temp6) {dim(temp5) <- c(ncol(temp6), nrow(temp6)); temp5}, gainVector, desList, SIMPLIFY = F)
  lossList <- mapply(function(temp5, temp6) {dim(temp5) <- c(ncol(temp6), nrow(temp6)); temp5}, lossVector, desList, SIMPLIFY = F)

  gainList <- lapply(gainList, t) # correct form
  lossList <- lapply(lossList, t)


  gainList <- lapply(gainList, function(temp1, temp2) {colnames(temp1) <- temp2; temp1}, namesOfCols)
  gainList <- mapply(auxiliaryNameRows, gainList, namesOfRows, SIMPLIFY = F)
  lossList <- lapply(lossList, function(temp1, temp2) {colnames(temp1) <- temp2; temp1}, namesOfCols)
  lossList <- mapply(auxiliaryNameRows, lossList, namesOfRows, SIMPLIFY = F)

  bindedUnnorm <- mapply(rbind, gainList, lossList, SIMPLIFY = F)

  #Goal calculate hmax
  result4max <- lapply(bindedUnnorm, function(temp) apply(temp, 2, abs))
  hmaxVector <- lapply(result4max, function(temp1) apply(temp1, 2, max)) # returns a list with the hmax vector
  hmaxVector <- lapply(hmaxVector, function(temp2) replace(temp2, temp2==0.0, 1.00)) #remove 0 to avoid NA when dividing

  g.normMatrix <- mapply(function(aMatrix, aVector) aMatrix / aVector[col(aMatrix)], gainList, hmaxVector, SIMPLIFY = F)
  l.normMatrix <- mapply(function(aMatrix, aVector) aMatrix / aVector[col(aMatrix)], lossList, hmaxVector, SIMPLIFY = F)

  if (binded) {
    bothMatrix <-mapply(rbind, g.normMatrix, l.normMatrix, SIMPLIFY = F)
    bothMatrix
  }
  else {
    bothMatrix <- list(gain = g.normMatrix, loss = l.normMatrix) # use lapply to join gain-loss for each userid
  }
  bothMatrix
}
