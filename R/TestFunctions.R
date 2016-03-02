diego_pv_extend <- function(g_or_l_matrix, weight = NULL, wfrom_dataset) {
  if(is.null(weight)) {
    result <-get_attr_weight(wfrom_dataset)
  }
  else {
    if(length(weight) != ncol(g_or_l_matrix)) {
      warning("Length of weight does not equal amount of attributes, some recycling may have been done here.")
    }
    result <- apply(g_or_l_matrix, 1, function(my_vector) { sum(my_vector*weight)})
  }
  result
}
#'Estas eran mas que todo para el Poster, para poder ensenar los graficos.
#'Enfocarse mas en las weight_functions, mira el papel, dejar la value function
#'como esta y documentarla, esto solo como extra, ignore o play.
#'
#'as dos funciones solo son para poder sacar la value function mas simple,
#'simple weighted additive function, gain_matrix sin normalizar y multiplicarlo
#'por los pesos, no es igual que el paper pero podria ser util, decide quickly.
#'
diego_pv <- function (dataset, userid = NULL, attr = NULL, rounds = NULL, refps = NULL, cost_ids = NULL,  weight = NULL,
                      gainm = TRUE) {
  if(gainm == TRUE) {
    chosen_m <- gain_matrix(dataset, userid, attr, rounds, refps, cost_ids)
  }
  else {
    chosen_m <- loss_matrix(dataset, userid, attr, rounds, refps, cost_ids)
  }

  diego_pv <- diego_pv_extend(chosen_m, weight, dataset)

  diego_pv

}

test_function_within_function <- function(x, play) {

    second_function <- function (x,play) {
        result <- x *play
        result
    }

    result <- second_function(x, play)
    result
}

test <- function(list1) {

  help <- numeric(0)

  for(i in list1){
    help <- c(help, sum(i))
  }
  help
}

lengthp <- function(list1) {
  help <- numeric(0)

  for(i in list1){
    help <- c(help, length(i))
  }
  help
}

#' Used for test-equal-normalized-matrix-function
norm.gainLoss.sep <- function(dataset, userid = NULL, attr = NULL, rounds = NULL, refps = NULL, cost_ids = NULL) {
  desList <- decisionMatrix(dataset, userid, attr, rounds, cost_ids)
  refPs <- referencePoints(dataset, userid, refps, attr, cost_ids)

  tMatrixList <- lapply(desList, t)

  gainList <- mapply(gainFunction, tMatrixList, refPs, SIMPLIFY = F)
  lossList <- mapply(lossFunction, tMatrixList, refPs, SIMPLIFY = F)

  #desList <- lapply(result, dim, nrow = rounds, ncol= length(attr), byrow = T)
  gainList <- mapply(function(temp5, temp6) {dim(temp5) <- c(ncol(temp6), nrow(temp6)); temp5}, gainList, desList, SIMPLIFY = F)
  lossList <- mapply(function(temp5, temp6) {dim(temp5) <- c(ncol(temp6), nrow(temp6)); temp5}, lossList, desList, SIMPLIFY = F)
  #Until this point is good,, -3 on the corner
  gainList <- lapply(gainList, t)
  lossList <- lapply(lossList, t)

  vectorBoth <- mapply(rbind, gainList, lossList, SIMPLIFY = F)

  result4max <- lapply(vectorBoth, function(temp) apply(temp, 2, abs))
  hmaxVector <- lapply(result4max, function(temp1) apply(temp1, 2, max)) # returns a list with the hmax vector
  hmaxVector <- lapply(hmaxVector, function(temp2) replace(temp2, temp2==0.0, 1.00)) #remove 0 to avoid NA when dividing


  gainLoss <- mapply(rbind, gainList, lossList, SIMPLIFY = F)
  bothMatrix <- lapply(gainLoss,t)
  bothMatrix <- mapply("/", bothMatrix, hmaxVector, SIMPLIFY = F)
  bothMatrix <- lapply(bothMatrix, t)
  bothMatrix

}
