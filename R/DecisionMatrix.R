#' Create the decision matrix
#'
#' Creates a decision matrix, which is necessary to create the 'Gains' and
#' 'Losses' matrices thereafter. The decision matrix is basically a
#' representation of the interaction of one user with the product configurator
#' through each step.
#'
#' @param data data.frame with the user generated data from a product
#'   configurator. Please see Details for specifications of the data.frame.
#'
#' @param userid User ID: an integer that gives the information of which user
#'   the matrix should be calculated
#'
#' @param attr attribute IDs, vector of integer numbers corresponding to the
#'   attributes you desire to use.
#'
#' @param rounds integer vector. Which steps of the configuration process should
#'   be shown? See Details.
#'
#' @param cost_ids argument used to convert selected cost attributes into
#'   benefit attributes. Integer vector.
#'
#' @details \code{data} We assume the input data.frame has following columns
#' usid = User IDs, round = integers indicating which round the user is in
#' (0-index works best for 'round'), atid = integer column for referring the
#' attribute ID (1 indexed), selected = numeric value of the attribute for a
#' specific, given round, selectable = amount of options the user can chose at a
#' given round, with the current configuration. This is a necessary parameter.
#'
#' \code{userid} is a necessary parameter, without it you'll get a warning.
#' Default is NULL.
#'
#' \code{attr} Default calculates with all attributes. Attributes are
#' automatically read from provided table, it is important you always provide
#' the complete dataset so that the package functions properly. Moreover the
#' attribute will be sorted in ascending order, i.e. if you input attr=
#' c(1,3,2), the decision matrix resulting will display the columns in order:
#' attr1 attr2 attr3.
#'
#' \code{rounds} Default calculates first round(initia product config) and last
#' round of the session. Default calculates with first and last attributes
#' (initial and final product configuration). To choose all give "all" as
#' argument for rounds, see example. "first" and "last" are also possible
#' argument values. You can give a vector of arbitrarily chosen rounds as well.
#'
#' \code{cost_ids} Default assumes all your attributes are of benefit type, that
#' is a higher value in the attribute means the user is better of than with a
#' lower value. If one or more of the attributes in your data is of cost type,
#' e.g. price, so that lower is better then you should identify this attributes
#' as such, providing their id, they'll be converted to benefit type (higher
#' amount is better).
#'
#' General: Through the matrix you can observe the value of each attribute at
#' any given moment or round. The number of columns of the matrix will always be
#' the same for all users. The number of rows depends on how much clicks or
#' rounds the user made in the product configurator. At the very least, you will
#' get always'round 0' meaning the initial, default configuration and 'round 1'
#' the last or final configuration, this is assuming the user just clicked once
#' or not at all.
#'
#' This function is for one user only, for more or all users see
#' \code{\link{powerful_function}}
#'
#'
#' @return A decision matrix for selected user with rows.length =
#'   length(\code{rounds}) and column.length = length(\code{attr}). Colnames =
#'   attrIDs and rownames = chosen rounds.
#' @examples
#' decision_matrix(camera2_config_data, 11) # Necessary arguments dataset and userid
#' decision_matrix(my_data, userid = 11, attr = c(1,3,5))
#' decision_matrix(another_data, userid = 80, rounds = c(1,2,3,7,8,9))
#'
#' decision_matrix(data2, 2, rounds = "all")
#' decision_matrix(data2, 120, rounds = "first", cost_ids = 1)
#' decision_matrix(data1, userid = 5, attr = c(1,4), rounds = "all", cost_ids="c(1,2)") #All possible parameters are in use here.
#'
#' @references ProductConfig Github page:
#'   https://github.com/avilesd/productConfig
#'
#' @export



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

#New function: different than previous version is for example by the handling of the cost_ids
# argument, e.g. decision_matrix(myData, 60, attr=c(1,2,4) ,rounds="all", cost_ids = 3),
# previous function cuts matrix first and then wrongly applies cost_ids to the third attribute,
# in this case attr4. Cost ids will be calculated before, so dont' have to consider anything
# before their input.
#' DOCU New if only inputtin one vector in rounds, it will be converted into a list, if you need
#' different rounds for different userids, you need to provide them as a list of vectors.

decisionMatrix <- function(dataset, userid = NULL, attr = NULL, rounds = NULL, cost_ids = NULL) {
  if(is.null(attr)) {
    attr <- sort(get_attrs_ID(dataset))
  }
  completeTables <- getTableById(dataset, userid)

  orderedLists <- lapply(completeTables, function(tempData) tapply(tempData$selected, tempData$round, FUN = "["))
  bindedTables <- lapply(orderedLists, function(tempData2) do.call(rbind, tempData2))

  costifiedTables <- benefitToCostAttr(dataset, bindedTables, cost_ids)
  costifiedTables

  #Consider names beforing cutting

  #Rounds
  all_rounds <- getRoundsById(dataset, userid)

  j <- 1
  if(is.null(rounds)) {
    rounds <- list()
    for (i in userid) {
      rounds[[j]] <- c(1, length(all_rounds[[j]]))
      j <- j + 1
    }
  }

  else if (!is.character(rounds) & !is.null(rounds)) {
    if (is.vector(rounds) & !is.list(rounds)) {
      rounds <- list("$roundsVector" = rounds)
    }
  }
  else if (is.character(rounds)) {
    if (rounds == "all"){
      rounds <- lapply(all_rounds, function(tempDataX) tempDataX+1)
    }
    else if (rounds == "last") {
      rounds <- lapply(all_rounds, length)
    }
    else if (rounds == "first") {
      rounds <- lapply(all_rounds, function(tempDataY) 1)
    }
  }

  else {
    warning("Input in 'rounds' not recognized, calculated with default: first and last rounds")
  }

  ## Name columns and rows, test inputting list of vectors other than the strings in 'rounds' argument.
  ## Should work as in list(0:2), test list out of bounds, and DOCU.
  tableRows <- lapply(costifiedTables, rownames)
  round.RowNames <- lapply(tableRows, paste0, "round")
  namedResult <- lapply(costifiedTables, function(tempData5) {colnames(tempData5) <-colnames(tempData5, do.NULL = F, prefix = "attr"); tempData5})
  namedResult <- mapply(namedResult[1:length(namedResult)], FUN =  function(tempDataC, tempDataD) {rownames(tempDataC) <- tempDataD; tempDataC}, round.RowNames, SIMPLIFY = FALSE)

   # CUT according to attr, not finished, need some rest, return later to it, write it down
  # current default behavior is getting all rounds.
  attribute.cut <- lapply(namedResult[1:length(namedResult)], function(tempData3) tempData3[,attr, drop=FALSE])

  #round.cut <- lapply(attribute.cut[1:length(attribute.cut)], function(tempData4) tempData4[rounds[j], , drop=FALSE])
  tryCatchResult = tryCatch({
    round.cut <- mapply(attribute.cut[1:length(attribute.cut)], FUN = function(tempDataA, tempDataB) tempDataA[tempDataB, , drop = FALSE], rounds, SIMPLIFY = FALSE)

  }, warning = function(condition) {
    message("Unknown warning in decisionMatrix by round cutting.")
  }, error = function(condition) {
    stop("One round in your 'rounds' input is not contained on your data: subscript out of bounds")
  }, finally={
  })

  #Get Dimensions -  move to if else cases ??

  round.cut
  }
