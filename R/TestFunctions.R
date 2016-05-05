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

#Used for test-equal-normalized-matrix-function
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

# Old functions, not vectorialized that are still used for tests, delete for deployment or hide in another folder


decision_matrix <- function(data, userid = NULL, attr = NULL, rounds = NULL, cost_ids = NULL) {

  if(is.null(userid)) {
    stop("You need to specify one userid.")
  }
  ## Check if given userid is in the data
  if(!userid %in% get_all_userids(data)) {
    print(userid)
    stop("The userid you specified is not contained in your data.")
  }
  ##Works even if you give the already usid-filtered table
  dataset <- get_table_by_ID(data, userid)

  if(is.null(attr)) {
    ##Get the attributes of given ID. Default = get all attributes.
    attr <- get_attrs_ID(dataset)
    attr <- sort(attr)
  }
  else {
    var1 <- length(attr)
    var2 <- attr %in% get_attrs_ID(dataset)
    var2 <- var2[var2 == TRUE]
    var2 <- length(var2)
    if(var1 == var2) {
      attr <- sort(attr)
    }
    else {
      rest <- var1 - var2
      stop(paste(rest ,"of the attributes you entered in attr are not to be found in your data."))
    }
  }
  ##Default setting first and last round
  if(is.null(rounds)) {
    all_rounds <- get_rounds_by_ID(dataset, userid)
    rounds <- c(all_rounds[1], all_rounds[length(all_rounds)])
  }

  else if (rounds == "all"){
    rounds <- get_rounds_by_ID(dataset, userid)
  }
  else if (rounds == "last") {
    all_rounds <- get_rounds_by_ID(dataset, userid)
    rounds <- all_rounds[length(all_rounds)]
  }
  else if (rounds == "first") {
    all_rounds <- get_rounds_by_ID(dataset, userid)
    rounds <- c(all_rounds[1])
  }
  ##TODO create for first and last

  ## Create dummy matrix
  result_matrix <- matrix(0, length(rounds), length(attr))

  ## Create column names vector
  col_names <- character(0)
  for(a in attr){
    col_names <- c(col_names, paste("attr", a, sep="", collapse=""))
  }

  ## Create rows names vector
  row_names <- character(0)
  for(b in rounds){
    row_names <- c(row_names, paste("round", b, sep="", collapse=""))
  }

  ## Name columns and rows
  colnames(result_matrix) <- col_names
  rownames(result_matrix) <- row_names

  m <- 1

  for(i in rounds) {

    table_round <- dataset[dataset$round == i, ]
    table_attribute <- table_round[table_round$atid %in% attr ,]
    row_complete <- table_attribute$selected
    result_matrix[m,] <- row_complete
    m <- m + 1

  }
  ## Convert cost attribute/s into benefit attribute/s.
  n <- 1
  if(!is.null(cost_ids)) {
    for(n in 1:length(cost_ids)) {
      if(!is.null(cost_ids)) {
        result_matrix[,cost_ids[n]] <- result_matrix[,cost_ids[n]] * (-1)
      }
    }
  }

  ##result <- list(matrix1 = matrix1)
  ##append(result, what,)

  result_matrix

}

get_table_by_ID <- function(x, userid = NULL,...) {
  if(is.null(userid)) {
    stop("You need to specify one userid.")
  }
  ## Check if given userid is in the data
  if(!userid %in% get_all_userids(x)) {
    print(userid)
    stop("The userid you specified is not contained in your data.")
  }

  result <- x[x$usid == userid, ]
  result
}
get_rounds_by_ID <- function(x, userid = NULL) {
  if(is.null(userid)) {
    stop("You need to specify at least one userid.")
  }
  ## Check if given userid is in the data
  if(!userid %in% get_all_userids(x)) {
    print(userid)
    stop("The userid you specified is not contained in your data.")
  }

  table_by_ID <- get_table_by_ID(x, userid)
  result <- unique(table_by_ID$round)
  result
}
get_all_userids <- function(dataset) {
  table_unique <- sapply(dataset, unique)
  result <- table_unique$usid
  result
}
get_all_default_rps <- function(dataset, userid) {
  if(is.null(userid)) {
    stop("You need to specify one userid.")
  }
  ## Check if given userid is in the data
  if(!userid %in% get_all_userids(dataset)) {
    print(userid)
    stop("The userid you specified is not contained in your data.")
  }

  table_unique <- get_table_by_ID(dataset, userid)
  table_0 <- table_unique[table_unique$round == 0, ]
  result <- table_0$selected

  ##Give Reference Points names according to attribute they belong to
  m <- 1
  help <- get_attrs_ID(dataset)
  rp_names <- character(0)

  for(rp in result){
    rp_number <- help[m]
    rp_names <- c(rp_names, paste("rp", rp_number, seq="", collapse=""))
    m <- m + 1
  }
  names(result) <- rp_names

  result
}
get_normalized_vec <- function(num_vector) {
  if(is.vector(num_vector, mode="numeric")) {
    sum <- sum(abs(num_vector))
    result <- num_vector/sum
    result
  }
  else warning("Entered argument not a numeric vector.")
}
get_attr_values <-function(dataset, attrid = NULL) {
  if(is.null(attrid)) {
    stop("You need to specify one attrid")
  }
  if(is.null(dataset)) {
    stop("You need to provide the dataset")
  }
  ## Check if given userid is in the data
  if(!attrid %in% get_attrs_ID(dataset)) {
    print(attrid)
    stop("The attrid you specified is not contained in your data.")
  }

  help1 <- tapply(dataset$selected, dataset$atid == attrid, unique)
  result <- help1$'TRUE'
  result
}

gain_matrix <- function(data, userid = NULL, attr = NULL, rounds = NULL, refps = NULL, cost_ids = NULL) {
  ## Calculate decision matrix based on inputs
  des_matrix <- decision_matrix(data, userid, attr, rounds, cost_ids)


  ## Get reference points(aspiration-levels) from input
  refps_vector <-ref_points(data, userid, refps, attr, cost_ids)

  ## Create empty result Gain matrix
  dim_matrix <- dim(des_matrix)
  gain_matrix <- matrix(2, dim_matrix[1],dim_matrix[2])

  ## TODO: 1. name the matrix
  m <- 1
  ## Fill the matrix
  for(n in 1:dim_matrix[1]) {
    gain_matrix[m, ] <- mapply(gain_fun_a, des_matrix[m, ], refps_vector)
    m <- m + 1
  }

  gain_matrix

}
loss_matrix <- function(data, userid = NULL, attr = NULL, rounds = NULL, refps = NULL, cost_ids = NULL) {
  ## Calculate decision matrix based on inputs
  des_matrix <- decision_matrix(data, userid, attr, rounds, cost_ids)


  ## Get reference points(aspiration-levels) from input
  refps_vector <-ref_points(data, userid, refps, attr, cost_ids)

  ## Create empty result Gain matrix
  dim_matrix <- dim(des_matrix)
  loss_matrix <- matrix(2, dim_matrix[1],dim_matrix[2])

  ## TODO: 1. name the matrix
  m <- 1
  ## Fill the matrix
  for(n in 1:dim_matrix[1]) {
    loss_matrix[m, ] <- mapply(loss_fun_a, des_matrix[m, ], refps_vector)
    m <- m + 1
  }
  loss_matrix
}
gain_loss_matrices <- function(data, userid = NULL, attr = NULL, rounds = NULL, refps = NULL, result_type = NULL, cost_ids = NULL) {

  g_matrix <- gain_matrix(data, userid, attr, rounds, refps, cost_ids)
  l_matrix <- loss_matrix(data, userid, attr, rounds, refps, cost_ids)

  ##Depending on result_type return matrices accordingly
  if(is.null(result_type)){
    result <- list(gain = g_matrix, loss = l_matrix)
  }
  else if(result_type == "rbind") {
    result <- rbind(g_matrix, l_matrix)
  }
  else if(result_type == "cbind") {
    result <- cbind(g_matrix, l_matrix)
  }
  result
}
norm_g_l_matrices <- function(data, userid = NULL, attr = NULL, rounds = NULL, refps = NULL, cost_ids = NULL) {
  g_l_matrix <- gain_loss_matrices(data, userid, attr, rounds, refps, result_type="rbind", cost_ids)

  n_gain <- gain_matrix(data, userid, attr, rounds, refps, cost_ids)
  n_loss <- loss_matrix(data, userid, attr, rounds, refps, cost_ids)

  hmax_vector <- numeric(0)
  number_col <-  dim(g_l_matrix)[2]

  for(n in 1:number_col) {
    hmax_vector <- c(hmax_vector, max(abs(g_l_matrix[,n])))
    if(hmax_vector[n] == 0) {

    }
    else {
      n_gain[,n] <- n_gain[,n]/hmax_vector[n]
      n_loss[,n] <- n_loss[,n]/hmax_vector[n]
    }
  }
  result <- list(ngain = n_gain, nloss = n_loss)
  result
}

powerful_function <- function(dataset, userid = NULL, FUN = decision_matrix, attr = NULL, rounds = NULL, refps = NULL, cost_ids = NULL,
                              weight = NULL, alpha = 0.88, beta = 0.88, lambda = 2.25, gainm = TRUE, result_type = NULL) {
  if(!is.null(userid)) {
    result_list <- list()

    for(i in userid) {

      usid <- paste("usid", i, sep = "")

      ##Create conditions
      con1 <- (identical(FUN, get_attrs_ID) | identical(FUN, get_all_userids))
      con2 <- (identical(FUN, get_table_by_ID) | identical(FUN, get_rounds_by_ID) | identical(FUN, get_all_default_rps)
               | identical(FUN, get_attr_values))
      con3 <- identical(FUN, decision_matrix)
      con4 <- identical(FUN, ref_points)
      con5 <- (identical(FUN, gain_matrix) | identical(FUN, loss_matrix) | identical(FUN, norm_g_l_matrices))
      con6 <- identical(FUN, gain_loss_matrices)
      con7 <- identical(FUN, pvalue_matrix)
      con8 <- identical(FUN, overall_pv)
      con9 <- identical(FUN, diego_pv)
      con10 <- identical(FUN, weight_higher_sum_value)
      con11 <- identical(FUN, get_attr_weight)

      if(con1) {
        result_list[[usid]] <- FUN(dataset)
      }
      else if(con2) {
        result_list[[usid]] <- FUN(dataset, userid = i)
      }
      else if(con3) {
        result_list[[usid]] <- FUN(dataset, userid = i, attr, rounds, cost_ids)
      }
      else if(con4) {
        result_list[[usid]] <- FUN(dataset, userid = i, attr, refps, cost_ids)
      }
      else if(con5) {
        result_list[[usid]] <- FUN(dataset, userid = i, attr, rounds, refps, cost_ids)
      }
      else if(con6) {
        result_list[[usid]] <- FUN(dataset, userid = i, attr, rounds, refps, cost_ids, result_type)
      }
      else if(con7) {
        result_list[[usid]] <- FUN(dataset, userid = i, attr, rounds, refps, cost_ids, alpha, beta, lambda)
      }
      else if(con8) {
        result_list[[usid]] <- FUN(dataset, userid = i, attr, rounds, refps, cost_ids, weight, alpha, beta, lambda)
      }
      else if(con9) {
        result_list[[usid]] <- FUN(dataset, userid = i, attr, rounds, refps, cost_ids, weight, gainm)
      }
      else if(con10) {
        result_list[[usid]] <- FUN(dataset, userid = i, rounds, cost_ids)
      }
      else if(con11) {
        result_list[[usid]] <- FUN(dataset, userid = i, weight, attr, rounds, cost_ids)
      }
      else {
        print("It appears the function (FUN) you gave is not to be use here, or not implemented yet.")
      }
    }
    result_list
  }
  else {
    stop("You need to provide at least 1 userid.")
  }
}

overall_pv <- function (dataset, userid = NULL, attr = NULL, rounds = NULL, refps = NULL, cost_ids = NULL,  weight = NULL,
                        alpha = 0.88, beta = 0.88, lambda = 2.25) {
  if(is.null(weight) & is.null(dataset)) {
    stop("Unable to get weights. You need to enter the weights or provide the dataset for us to calculate them. ")
  }
  if(is.null(weight) & !is.null(dataset)) {
    weight <-get_attr_weight(dataset, userid, weight,  attr, rounds, cost_ids)
  }

  v_matrix <- pvalue_matrix(dataset, userid, attr, rounds, refps, cost_ids, alpha, beta, lambda)

  overall_pv <- overall_pv_extend(v_matrix, weight)

  overall_pv

}
pvalue_matrix <- function(dataset, userid = NULL, attr = NULL, rounds = NULL, refps = NULL, cost_ids = NULL,
                          alpha = 0.88, beta = 0.88, lambda = 2.25) {
  ngain <- norm_g_l_matrices(dataset, userid, attr, rounds, refps, cost_ids)$ngain
  nloss <- norm_g_l_matrices(dataset, userid, attr, rounds, refps, cost_ids)$nloss

  v_matrix <- prospect_value_matrix_extend(ngain, nloss, alpha, beta, lambda)
  v_matrix

}

ref_points <- function(dataset, userid, refps = NULL, attr = NULL, cost_ids = NULL, ...) {
  ## Handle attributes
  attrnull <- is.null(attr)
  if(attrnull) {
    ##Get all the attributes. Default = get all attributes.
    attr <- get_attrs_ID(dataset)
  }
  else {
    var1 <- length(attr)
    var2 <- attr %in% get_attrs_ID(dataset)
    var2 <- var2[var2 == TRUE]
    var2 <- length(var2)
    if(var1 == var2) {
      attr <- sort(attr)
    }
    else {
      rest <- var1 - var2
      stop(paste(rest ,"of the attribute IDs you entered in attr are not to be found in your data."))
    }
  }
  ## Actual getting of the Reference Points begins here
  fullattr <- identical(sort(get_attrs_ID(dataset)), sort(attr))

  if(is.null(refps) & fullattr) {
    refps <- get_all_default_rps(dataset, userid)
  }
  else if(is.null(refps) & !fullattr) {
    refps <- get_all_default_rps(dataset, userid)
    refps <- refps[attr]
  }
  else {
    #Bug fixed
    if(length(attr) != length(refps)){
      if(attrnull) {
        stop("Amount of RefPoints entered doesn't equal amount of attributes in your table. Enter equal amount of attributes and RefPoints or all RefPoints.")
      }
      else {
        stop("Amount of RefPoints entered doesn't equal amount of attributes you entered. Enter equal amount of attributes and RefPoints or none.")

      }
    }

    m <- 1
    rp_names <- character(0)

    for(rp in refps){
      rp_names <- c(rp_names, paste("rp", m, seq="", collapse=""))
      m <- m + 1
    }
    names(refps) <- rp_names
  }

  n <- 1
  if(!is.null(cost_ids)) {
    for(n in 1:length(cost_ids)) {
      if(!is.null(cost_ids)) {
        refps[cost_ids[n]] <- refps[cost_ids[n]] * (-1)
      }
    }
  }
  refps

}

transform4 <- function(dataset, userid=users.withRanks, attr=NULL, rounds="all", cost_ids=4) {
  dM <- decisionMatrix(dataset, userid, attr, rounds, cost_ids)
  roundedDM <- lapply(dM, round, digits=5)
  roundedDM <- lapply(roundedDM, function(temp) apply(temp, 2, function(temp2) {replace(temp2, temp2==-0.46528, 0)}))
  roundedDM <- lapply(roundedDM, function(temp) apply(temp, 2, function(temp2) {replace(temp2, temp2==-0.61389, 0)}))
  roundedDM <- lapply(roundedDM, function(temp) apply(temp, 2, function(temp2) {replace(temp2, temp2==-0.31667, 1)}))
  roundedDM <- lapply(roundedDM, function(temp) apply(temp, 2, function(temp2) {replace(temp2, temp2==-0.16806, 2)}))
  roundedDM <- lapply(roundedDM, function(temp) apply(temp, 2, function(temp2) {replace(temp2, temp2==-0.01944, 3)}))
  roundedDM
}

containsVectors <- function(allMatrices, datenbankMatrix) {
  j <- 0
  for (vec in 1:nrow(datenbankMatrix)) {
    if(vec==1) nameIndex <- "3"
    if(vec==2) nameIndex <- "9"
    if(vec==3) nameIndex <- "16"
    if(vec==4) nameIndex <- "19"
    if(vec==5) nameIndex <- "25"
    if(vec==6) nameIndex <- "35"
    if(vec==7) nameIndex <- "36"
    if(vec==8) nameIndex <- "59"

    temp <- datenbankMatrix[vec, ]
    allMatrices <- lapply(allMatrices, unname)

    if (j==0) {
      newMatrices <- lapply(allMatrices, function(tempData) someFunction(tempData, temp, nameIndex))
      j <- 1
    }
    else {
      newMatrices <- lapply(newMatrices, function(tempData) someFunction(tempData, temp, nameIndex))
    }

  }
  newMatrices
}

someFunction <- function(allMatrix, vecToTest, nameIndex) {
  for (someIndex in 1:nrow(allMatrix)) {
    tempAllVec <- allMatrix[someIndex, ]
    someBoolean <- all.equal(tempAllVec, vecToTest, check.names=FALSE)
    someBoolean
    if(!is.na(someBoolean) & someBoolean==TRUE) {
      allMatrix <- as.data.frame(allMatrix)
      rownames(allMatrix)[someIndex] <- paste(someIndex, "--prid", nameIndex, sep = "")
      allMatrix <- as.matrix(allMatrix)
    }

  }
  allMatrix
}
