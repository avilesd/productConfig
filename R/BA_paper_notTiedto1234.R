##Apply to sorted rankings!! we just need the info about the prid and which user
listOfNames <- function(aListofVectors) {
  result <- sapply(aListofVectors, names)
  onlyPrids <- lapply(result, function(temp) {temp[grep("prid", temp)] })

  regularExp <- lapply(onlyPrids, function(temp) {gsub("\\d\\-\\-", "", temp) })
  regularExp <- lapply(regularExp, function(temp) {gsub("^.", "", temp) })
  regularExp <- lapply(regularExp, function(temp) {gsub("p", "", temp) })
  regularExp <- lapply(regularExp, function(temp) {gsub("rid", "", temp) })
  regularExp <- lapply(regularExp, function(temp) {as.numeric(temp)})
  uniquePrids <- lapply(regularExp, function(x) unique(x))
  uniquePrids <- lapply(uniquePrids, function(prids) { rbind(prids, "ranking" = 1:length(prids)) })
  uniquePrids
}

tidyUpData <- function(listOfMatricesPV, listOfMatricesDRP, listOfMatricesTRP) {
  #PT
  listOfTargetsPV <- lapply(listOfMatricesPV, function(vector) sort(vector[1,], decreasing=F))
  resultPV <- mapply(function(targetVector, matrix) {t(matrix[ , match(targetVector, matrix[1,])])},
                     listOfTargetsPV, listOfMatricesPV)
  #DRP
  listOfTargetsDRP <- lapply(listOfMatricesDRP, function(vector) sort(vector[1,], decreasing=F))
  resultDRP <- mapply(function(targetVector, matrix) {t(matrix[ , match(targetVector, matrix[1,])])},
                      listOfTargetsDRP, listOfMatricesDRP)

  #TRP
  listOfTargetsTRP <- lapply(listOfMatricesTRP, function(vector) sort(vector[1,], decreasing=F))
  resultTRP <- mapply(function(targetVector, matrix) {t(matrix[ , match(targetVector, matrix[1,])])},
                      listOfTargetsTRP, listOfMatricesTRP)
  # Bind all three matices together column-wise
  result <- mapply(function(pt, drp, trp) {cbind(pt, drp[ ,2], trp[ ,2])}, resultPV, resultDRP, resultTRP)

  #Get the names of the list, to use on each matrix
  matrixNames <- names(listOfMatricesPV)
  namesAsNumbers <- as.numeric(matrixNames)

  # Take userid out of list element and put it as a column for each row of the matrices
  result <- mapply(function(aNumber, matrix) {cbind("userid" = rep(aNumber, nrow(matrix)), matrix)}, namesAsNumbers, result)
  #Rename prospect theory column
  result <- lapply(result, function(matrix) {colnames(matrix) <- c("userid", "prid" , "pt", "dual", "tri"); matrix})
  result
}

deployTable <- function(listOfMatricesPV, listOfMatricesDRP, listOfMatricesTRP) {
  cleanData <- tidyUpData(listOfMatricesPV, listOfMatricesDRP, listOfMatricesTRP)
  #rbind all data together and remove rownames
  bindedData <- do.call("rbind", cleanData)
  rownames(bindedData) <- NULL

  bindedData

}

## Here start the Get functions and Prids helpers
getAllPrids <- function(dataset) {
  table_unique <- sapply(dataset, unique)
  result <- table_unique$prid
  result
}

getAllValues <- function (dataset) {
  table_unique <-  sapply(dataset, unique)
  result <- table_unique$value
  result
}

getPridValues <-function(dataset, prid = NULL) {
  if(is.null(dataset)) {
    stop("You need to provide the dataset")
  }

  allPrids <- getAllPrids(dataset)

  if(is.null(prid)) {
    prid <- allPrids
  }
  if(FALSE %in%(prid %in% allPrids)) {
    allPrids <- paste(allPrids, sep=",", collapse = " ")
    stop("One of the attrid you specified is not contained in your data. Valid Attibute Ids are: ", allPrids)
  }

  help1 <- split(dataset, f = dataset$prid)
  result <- lapply(help1[prid], function(tempData) unique(tempData$value))
  result
}


getTableByPrid <- function(dataset, prid = NULL,...) {
  if(is.null(prid)) {
    stop("You need to specify at least one userid.")
  }

  if(FALSE %in%(prid %in% getAllPrids(dataset))) {
    logicalVector <-!(prid %in% getAllPrids(dataset))
    fatalUserid <- prid[logicalVector]
    fatalUserid <- paste(fatalUserid, collapse = " ")
    stop("At least one userid you specified is not contained within your data: ", fatalUserid)
  }

  result <- split(dataset, f = dataset$prid)
  result <- result[as.character(prid)]
  result
}

pridVectors <- function(dataset, prid = NULL) {
  allTables <- getTableByPrid(dataset, prid)
  result <- lapply(allTables, function(x) {x$value})
  result
}

convertValues <- function(dataset, usefulPrids, group0, group1, group2, group3) {
  workTable <- dataset[dataset[,1] %in% usefulPrids,]
  thirdColumn <- lapply(workTable, echo, group0, group1, group2, group3)
  workTable
}

echo <- function(x, group0, group1, group2, group3) {
  if(x %in% group0) {
    result <- 0
  }
  if(x %in% group1) {
    result <- 1
  }
  if(x %in% group2) {
    result <- 2
  }
  if(x %in% group3) {
    result <- 3
  }
  result
}

