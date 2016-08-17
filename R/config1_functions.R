transform4_config1 <-function(dataset, userid= NULL, attr=NULL, rounds="all", cost_ids=3) {
  dM <- decisionMatrix(dataset, userid, attr, rounds, cost_ids)
  roundedDM <- lapply(dM, round, digits=5)
  roundedDM <- lapply(roundedDM, function(temp) apply(temp, 2, function(temp2) {replace(temp2, temp2==-0.58879, 0)}))
  roundedDM <- lapply(roundedDM, function(temp) apply(temp, 2, function(temp3) {replace(temp3, temp3==-0.45157, 0)}))
  roundedDM <- lapply(roundedDM, function(temp) apply(temp, 2, function(temp4) {replace(temp4, temp4==-0.31434, 1)}))
  roundedDM <- lapply(roundedDM, function(temp) apply(temp, 2, function(temp5) {replace(temp5, temp5==-0.17711, 2)}))
  roundedDM <- lapply(roundedDM, function(temp) apply(temp, 2, function(temp6) {replace(temp6, temp6==-0.03989, 3)}))
  roundedDM
}

containsVectors1_allPrids <- function(allMatrices, datenbankMatrix) {
  j <- 0
  for (vec in 1:nrow(datenbankMatrix)) {
    if(vec==1) nameIndex <- "1"
    if(vec==2) nameIndex <- "2"
    if(vec==3) nameIndex <- "3"
    if(vec==4) nameIndex <- "4"
    if(vec==5) nameIndex <- "5"
    if(vec==6) nameIndex <- "6"
    if(vec==7) nameIndex <- "7"
    if(vec==8) nameIndex <- "8"

    if(vec==9) nameIndex <- "9"
    if(vec==10) nameIndex <- "10"
    if(vec==11) nameIndex <- "11"
    if(vec==12) nameIndex <- "12"
    if(vec==13) nameIndex <- "13"
    if(vec==14) nameIndex <- "14"
    if(vec==15) nameIndex <- "15"
    if(vec==16) nameIndex <- "16"

    if(vec==17) nameIndex <- "17"
    if(vec==18) nameIndex <- "18"
    if(vec==19) nameIndex <- "19"
    if(vec==20) nameIndex <- "20"
    if(vec==21) nameIndex <- "21"
    if(vec==22) nameIndex <- "22"
    if(vec==23) nameIndex <- "23"
    if(vec==24) nameIndex <- "24"

    if(vec==25) nameIndex <- "25"
    if(vec==26) nameIndex <- "26"
    if(vec==27) nameIndex <- "27"
    if(vec==28) nameIndex <- "28"
    if(vec==29) nameIndex <- "29"
    if(vec==30) nameIndex <- "30"
    if(vec==31) nameIndex <- "31"
    if(vec==32) nameIndex <- "32"

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

## function for part2, to code the allTables1
codifyAllTables1 <- function(listOfVec) {
  # for laziness not renamed table2
  result <- lapply(listOfVec, function(table2) replace(table2, table2==6 | table2== 1.5 | table2 ==850 , 0))
  result <- lapply(result   , function(table2) replace(table2, table2==8 | table2== 2.0 | table2 ==700  , 1))
  result <- lapply(result   , function(table2) replace(table2, table2==10 | table2== 2.5 | table2 == 550, 2))
  result <- lapply(result   , function(table2) replace(table2, table2==12 | table2== 3.0 | table2 == 400 , 3))

  result
}


