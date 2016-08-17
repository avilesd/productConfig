#' Creates the decision matrix
#'
#' The decision matrix is the basic object for analyzing data from product
#' configurators (pC). The decision matrix is basically a representation of the
#' interaction of one user with a product configurator through each step. Each
#' columns stands for one attribute of the pC, i.e. for one specific product
#' setting. Each row represents different configurations of a product. The
#' rounds vary between users, since they are generated each time the user
#' interacts with the pC. See Details for more info.
#'
#' @param dataset a \code{data.frame} with the user generated data from a
#'   product configurator. See Details of \code{\link{decisionMatrix}} for more
#'   information about which data should included in this argument.
#'
#' @param userid an integer vector indicating for which user the output of this
#'   function should be calculated. This functions is vectorised in this
#'   argument, i.e. you may enter more userIDs simultaneously.
#'
#' @param attr attribute IDs, vector of integer numbers corresponding to the
#'   attributes (columns) you desire to use.
#'
#' @param rounds integer vector, text option or a list of integer vectors. Which
#'   steps of the configuration process should be shown? Defaults are first and
#'   last step. Text options are \code{all, first, last}. Alternatively, a
#'   vector of arbitrarily chosen rounds can be entered as well.
#'
#' @param cost_ids argument used to convert selected cost attributes into
#'   benefit attributes. Integer vector. Cost type attributes have the
#'   characteristic, that a lower value means the user is better off than with a
#'   higher value. E.g. price is often considered a cost type attribute. Should be equal to
#'   \code{attr} input or a subset of it.
#'
#' @details \code{dataset} We assume the input data.frame has following columns
#'   usid = User IDs, round = integers indicating which round the user is in
#'   (0-index works best for 'round'), atid = integer column for referring the
#'   attribute ID (1 indexed), selected = numeric value of the attribute for a
#'   specific, given round, selectable = amount of options the user can chose at
#'   a given round, with the current configuration. This is a necessary
#'   parameter.
#'
#'   \code{userid} is a necessary parameter, without it you'll get a warning.
#'   Default is NULL.
#'
#'   \code{attr} Default calculates with all attributes. Attributes are
#'   automatically read from provided table, it is important you always provide
#'   the complete dataset so that the package functions properly. Moreover the
#'   attributes will not be sorted. Output columns are returned in the ordered
#'   they were inputed.
#'
#'   \code{rounds} If you need to compute different rounds for each user you enter, this
#'   argument accepts a list of integer vectors indicating which rounds should be used for each user.
#'   The function does not read names, it works in the order the list was given.
#'
#'   \code{cost_ids} If \code{attr} and \code{cost_ids} differ, the functions will first
#'   compute the entire decision matrix using the \code{cost_ids} and only in the end will
#'   it 'subset' the result to the desired \code{attr}.
#'
#'   Default assumes all your attributes are of benefit type,
#'   that is a higher value in the attribute means the user is better of than
#'   with a lower value. If one or more of the attributes in your data is of
#'   cost type, e.g. price, so that lower is better then you should identify
#'   this attributes as such, providing their ID, they'll be converted to
#'   benefit type (higher amount is better).
#'
#'   General: Through the matrix you can observe the value of each attribute at
#'   any given moment or round. The number of columns of the matrix will always
#'   be the same for all users. The number of rows depends on how much clicks or
#'   rounds the user made in the product configurator. At the very least, you
#'   will always get 'round 0' meaning the initial, default configuration and
#'   'round 1' the last or final configuration, this is assuming the user just
#'   clicked once or not at all.
#'
#' @return A decision matrix for selected users with rows.length =
#'   length(\code{rounds}) and column.length = length(\code{attr}). Colnames =
#'   attrIDs and rownames = chosen rounds.
#'
#' @examples #Not Runnable yet
#' decisionMatrix(camera2_config_data, 11) # Necessary arguments dataset and userid
#' decisionMatrix(my_data, userid = 11:15, attr = c(1,3,5))
#' decisionMatrix(another_data, userid = c(80,90,100,110), rounds = c(1,2,3,7,8,9))
#'
#' decisionMatrix(data2, 2:200, rounds = "all")
#' decisionMatrix(data2, 120, rounds = "first", cost_ids = 1)
#' decisionMatrix(data1, userid = 5, attr = c(1,4), rounds = "all", cost_ids="c(1,2)") #All possible parameters are in use here.
#'
#' @references ProductConfig Github page:
#'   https://github.com/avilesd/productConfig
#'
#' @export

decisionMatrix <- function(dataset, userid = NULL, attr = NULL, rounds = NULL, cost_ids = NULL) {
  if(is.null(attr)) {
    attr <- sort(get_attrs_ID(dataset))
  }
  completeTables <- getTableById(dataset, userid)

  catchCommonErrors(dataset, userid, attr, rounds, cost_ids)

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
  round.RowNames <- lapply(tableRows, paste0)
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

#Auxiliary function, not necessary to document
catchCommonErrors <- function(dataset, userid = NULL, attr = NULL, rounds = NULL, cost_ids = NULL) {
  # USERID catched by getTableID in dM
  # ATTR
  allAttr <- get_attrs_ID(dataset)
  if(FALSE %in%(attr %in% allAttr)) {
    logicalVector <-!(attr %in% allAttr)
    fatalAttr <- attr[logicalVector]
    fatalAttr <- paste(fatalAttr, collapse = " ")
    stop("At least one 'attr' you specified is not contained within your data: ", fatalAttr)
  }
  # COST_IDS
  if(FALSE %in%(cost_ids %in% allAttr)) {
    logicalVector <-!(cost_ids %in% allAttr)
    fatalCostId <- cost_ids[logicalVector]
    fatalCostId <- paste(fatalCostId, collapse = " ")
    stop("At least one 'cost_ids' you specified is not contained within your data: ", fatalCostId)
  }

}
