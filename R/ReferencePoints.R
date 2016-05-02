#' Delivers the Reference Points
#'
#' This function delivers reference points for other functions to use,
#' specifically for functions based on prospect theory. With the reference
#' points you can calculate gains and losses relative to a single reference
#' point. We base our knowledge and assumptions to calculate gains and losses
#' from a paper, please see source and references [1].
#'
#' Reference Points can be many things, such as defaults, aspiration levels,
#' status quo, etc. Here we provide the tools to read the reference points from
#' the given data, i.e. reference points are assumed to be default (first)
#' values in the decision matrix.
#'
#' @inheritParams decisionMatrix
#'
#' @param forceRefps logical, default value is \code{TRUE}. If values are given
#'   to \code{refps}, it forces the user to enter a value for all attributes,
#'   regardless if you entered just one attribute ID in \code{attr.} If you wish
#'   to manually enter only one refps value for one \code{attr} and let the
#'   function calculate the other ones, you have to give \code{NA} as input for
#'   each attribute you want calculated. See the second example.
#'
#'   Alternatively, you can set forceRefps and avoid given a value (NA or
#'   numeric) for each attribute. However, do this at your own risk, since the
#'   argument is intended for development only.
#'
#' @param refps a list of numeric vectors, one for each user. Reference Points:
#'   each point corresponds to one attribute, therefore the amount of attributes
#'   and of refps entered, should be equal. Default assumes the refps as the
#'   default values of the initial product configuration for each user. You may
#'   fully or partially enter your own reference points, check below for more
#'   info.
#'
#' @details \code{refps} If values are given to \code{refps}, the function
#'   forces the user to enter a value for all attributes, regardless if you
#'   entered just one attribute ID in \code{attr.} If you wish to manually enter
#'   only one refps value for one \code{attr} and let the function calculate the
#'   other ones, you have to give \code{NA} as input for each attribute you want
#'   calculated. See the second example.
#'
#'   \code{dataset} We assume the input data.frame has following columns usid =
#'   User IDs, round = integers indicating which round the user is in (0-index
#'   works best for round), atid = integer column for referring the attribute ID
#'   (1 indexed), selected = numeric value of the attribute for a specific,
#'   given round, selectable = amount of options the user can chose at a given
#'   round, with the current configuration. This is a necessary parameter.
#'
#'   \code{userid} is a necessary parameter, without it you'll get a warning.
#'   Default is NULL.
#'
#'   \code{rounds} If you need to compute different rounds for each user you
#'   enter, this argument accepts a list of integer vectors indicating which
#'   rounds should be used for each user. The function does not read names, it
#'   works in the order the list was given.
#'
#'   \code{cost_ids} If \code{attr} and \code{cost_ids} differ, the functions
#'   will first compute the entire decision matrix using the \code{cost_ids} and
#'   only in the end will it 'subset' the result to the desired \code{attr}.
#'
#' @return a list of numeric vectors representing a reference point for each
#'   attribute. Length of the vectors will be the amount of attributes IDs in
#'   \code{attr}.
#'
#' @references [1] Fan, Z. P., Zhang, X., Chen, F. D., & Liu, Y. (2013).
#'   Multiple attribute decision making considering aspiration-levels: A method
#'   based on prospect theory. Computers & Industrial Engineering, 65(2),
#'   341-350.
#'
#' @examples #Not Runnable yet
#' referencePoints(myData, 9:11, attr=1:3)
#' referencePoints(myData, 9:11, attr=1:3, refps=(NA, 10, NA))
#' # Example above calculates refps for attr 1,3 and 4. For attr 2 it returns 20.
#' referencePoints(myData, 9:11, attr=1:3, cost_ids = 4) #Attr 4 will not be outputted
#' referencePoints(data, 6, refps=c(0,-10,9,2))
#' referencePoints(myData, 9:10, attr = c(1,4), cost_ids=4)
#'
#' #Some examples that throw an error
#' referencePoints(myData, 9:11, attr=1:3, cost_ids = 4, refps=100) #Error:Not enough refps entered
#' #' referencePoints(data, refps=c(0,0,0), attr=c(1,4)) # Same error as previous example
#'
#' @export
#'

## DOCU check notes, but NA refps will be calculated
referencePoints <- function(dataset, userid, refps = NULL, attr = NULL, cost_ids = NULL, forceRefps = TRUE) {
  # Check decision tree
  attrnull <- is.null(attr)
  fillAfterCut <- FALSE

  if(attrnull) {
    attr <- get_attrs_ID(dataset) #Get all the attributes: Default behavior.
  }
  else {
    if(FALSE %in% (attr %in% get_attrs_ID(dataset))) {
      attr <- get_attrs_ID(dataset)
      attr <- paste(attr, sep=",", collapse = " ")
      stop("of the attribute IDs you entered in attr are not to be found in your data.
           Valid attr Ids are: ", attr)
    }
  }
  fullattr <- identical(sort(get_attrs_ID(dataset)), sort(attr)) #Check if there are fullattr, either inputed or as default (line150)
  defaultRefps <- getDefaultRefps(dataset, userid) #Handels userid input errors

  if(forceRefps) {
    if (!is.null(refps)) {
      lengthDefValues <- length(defaultRefps[[1]])
      if(lengthDefValues != length(refps)) {
        warning("Alternatively set forceRefps to FALSE", call. = FALSE)
        stop("You need to enter a numeric or NA value for all refps, total amount of attributes: ", lengthDefValues, call. = FALSE)
      }
      boolean.vector <- !is.na(refps)
      defaultRefps <- lapply(defaultRefps, FUN = replaceNotNA, refps, boolean.vector)
    }
    else { # refps is null
      defaultRefps <- defaultRefps
    }
  }

  if(!forceRefps) {
    if(length(attr) != length(refps) & !is.null(refps)) {
      stop("Amount of RefPoints entered doesn't equal amount of attributes you entered. Enter equal amount of attributes and RefPoints or none.")
    }
    else {
      fillAfterCut <- TRUE
    }
  }

  defAndCostIdRefps <- benefitToCostAttr(dataset, defaultRefps, cost_ids)

  #CUT according to attribute
  if (fullattr) {
    cutTable <- defAndCostIdRefps
  }
  else {
    cutTable <- defAndCostIdRefps
    cutTable <- lapply(cutTable[1:length(cutTable)], "[", attr)
  }

  if(fillAfterCut & !is.null(refps)) {
    boolean.vector2 <- !is.na(refps)
    result <- lapply(cutTable, FUN = replaceNotNA, refps, boolean.vector2)
    print("here is the error, Na + forceRefps = T")
    costCharacter <- paste("rp", cost_ids)
    result <- benefitToCostAttr(dataset, result, costCharacter)

    if(length(cost_ids) > length(attr)) {
      warning("You have entered more cost_ids than attributes", call. = FALSE)
    }
  }
  else {
    result <- cutTable
  }
  result
}
