## --DONE for 2
transform4_config2 <-function(dataset, userid= NULL, attr=NULL, rounds="all", cost_ids=3) {
  dM <- decisionMatrix(dataset, userid, attr, rounds, cost_ids)
  roundedDM <- lapply(dM, round, digits=2)
  #roundedDM <- lapply(roundedDM, function(temp) apply(temp, 2, function(temp2) {replace(temp2, temp2==-0.58879, 0)}))
  roundedDM <- lapply(roundedDM, function(temp) apply(temp, 2, function(temp3) {replace(temp3, temp3==-0.61, 0)}))
  roundedDM <- lapply(roundedDM, function(temp) apply(temp, 2, function(temp3) {replace(temp3, temp3==-0.48, 0)}))
  roundedDM <- lapply(roundedDM, function(temp) apply(temp, 2, function(temp4) {replace(temp4, temp4==-0.46, 0)}))

  roundedDM <- lapply(roundedDM, function(temp) apply(temp, 2, function(temp5) {replace(temp5, temp5==-0.34, 1)}))
  roundedDM <- lapply(roundedDM, function(temp) apply(temp, 2, function(temp3) {replace(temp3, temp3==-0.31, 1)}))
  roundedDM <- lapply(roundedDM, function(temp) apply(temp, 2, function(temp3) {replace(temp3, temp3==-0.19, 1)}))

  roundedDM <- lapply(roundedDM, function(temp) apply(temp, 2, function(temp4) {replace(temp4, temp4==-0.17, 2)}))
  roundedDM <- lapply(roundedDM, function(temp) apply(temp, 2, function(temp5) {replace(temp5, temp5==-0.16, 2)}))
  roundedDM <- lapply(roundedDM, function(temp) apply(temp, 2, function(temp3) {replace(temp3, temp3==-0.14, 2)}))

  roundedDM <- lapply(roundedDM, function(temp) apply(temp, 2, function(temp3) {replace(temp3, temp3==-0.04, 3)}))
  roundedDM <- lapply(roundedDM, function(temp) apply(temp, 2, function(temp4) {replace(temp4, temp4==-0.02, 3)}))
  roundedDM <- lapply(roundedDM, function(temp) apply(temp, 2, function(temp5) {replace(temp5, temp5==-0.01, 3)}))

  roundedDM <- lapply(roundedDM, function(temp) apply(temp, 2, function(temp6) {replace(temp6, temp6==-0.00, 3)}))
  roundedDM

}
# -- DONE for 2
containsVectors2_allPrids <- function(allMatrices, datenbankMatrix) {
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

    if(vec==33) nameIndex <- "33"
    if(vec==34) nameIndex <- "34"
    if(vec==35) nameIndex <- "35"
    if(vec==36) nameIndex <- "36"
    if(vec==37) nameIndex <- "37"
    if(vec==38) nameIndex <- "38"
    if(vec==39) nameIndex <- "39"
    if(vec==40) nameIndex <- "40"

    if(vec==41) nameIndex <- "41"
    if(vec==42) nameIndex <- "42"
    if(vec==43) nameIndex <- "43"
    if(vec==44) nameIndex <- "44"
    if(vec==45) nameIndex <- "45"
    if(vec==46) nameIndex <- "46"
    if(vec==47) nameIndex <- "47"
    if(vec==48) nameIndex <- "48"

    if(vec==49) nameIndex <- "49"
    if(vec==50) nameIndex <- "50"
    if(vec==51) nameIndex <- "51"
    if(vec==52) nameIndex <- "52"
    if(vec==53) nameIndex <- "53"
    if(vec==54) nameIndex <- "54"
    if(vec==55) nameIndex <- "55"
    if(vec==56) nameIndex <- "56"

    if(vec==57) nameIndex <- "57"
    if(vec==58) nameIndex <- "58"
    if(vec==59) nameIndex <- "59"
    if(vec==60) nameIndex <- "60"
    if(vec==61) nameIndex <- "61"
    if(vec==62) nameIndex <- "62"
    if(vec==63) nameIndex <- "63"
    if(vec==64) nameIndex <- "64"

    if(vec==65) nameIndex <- "65"
    if(vec==66) nameIndex <- "66"
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

## function for part2, to code the allTables1 -- DONE for 2
codifyAllTables2 <- function(listOfVec) {
  # for laziness not renamed table2
  result <- lapply(listOfVec, function(table2) replace(table2, table2 ==1000 | table2==6 | table2== 1.5 | table2 ==850 , 0))
  result <- lapply(result   , function(table2) replace(table2, table2 ==750  | table2==8 | table2== 2.0 | table2 ==700  , 1))
  result <- lapply(result   , function(table2) replace(table2, table2 ==500  |table2==10 | table2== 2.5 | table2 == 550, 2))
  result <- lapply(result   , function(table2) replace(table2, table2 ==320  |table2==12 | table2== 3.0 | table2 == 400 , 3))

  result
}
