#' Delivers the Reference Points
#'
#' This function delivers reference points for other functions to use. With the
#' reference points you can calculate gains and losses relative to a reference
#' point. We base our knowledge and assumptions to calculate gains and losses
#' from a paper, please see source and references. Reference Points can be many
#' things, such as defaults, aspiration levels, status quo, etc. Here we provide
#' the tools to read the reference points from the given data, i.e. reference
#' points as default values. We also provide the tools for the user of this
#' package to provide the refps to the funciton and use those to calculate
#' further matrices and value functions.
#'
#' @param dataset cdata.frame with the user generated data from a product
#'   configurator. Please see \code{decision_matrix} for specifications of the
#'   data.frame.
#'
#' @param userid an integer that gives the information of which user the
#'   reference points should be determined.
#'
#' @param refps numeric vector. Reference Points: each point corresponds to one
#'   attribute, i.e. each attribute has only one aspiration level. Default
#'   setting assumes the aspiration levels as the default values of the initial
#'   product configuration for each user.
#'
#' @param attr attributes IDs, vector of integer numbers corresponding to the
#'   attributes you desire to use; attr are assumed to be 1-indexed.
#'
#' @param cost_ids argument used to convert selected cost attributes into
#'   benefit attributes. Integer vector.
#'
#' @details \code{dataset} We assume the input data.frame has following columns
#' usid = User IDs, round = integers indicating which round the user is in
#' (0-index works best for round), atid = integer column for referring the
#' attribute ID (1 indexed), selected = numeric value of the attribute for a
#' specific, given round, selectable = amount of options the user can chose at a
#' given round, with the current configuration. This is a necessary parameter.
#'
#' \code{userid} is a necessary parameter, without it you'll get a warning.
#' Default is NULL.
#'
#' \code{refps} The most important parameter for this function. If you only want
#' to see the results for one attribute you may enter only a couple of reference
#' points but you have to tell the function which attributes you want to use
#' those referene points for. So the amount of attr and of refps should be the
#' same. Moreover the functions always orders de attr, so be sure to input the
#' reference point also in an ascending order corresponding to their attributes.
#' (refps will not be ordered)
#'
#' \code{attr} Default calculates with all attributes. Attributes are
#' automatically read from provided table, it is important you always provide
#' the complete dataset so that the package functions properly. Moreover the
#' attribute will be sorted in ascending order, i.e. if you input attr=
#' c(1,3,2), the decision matrix resulting will display the columns in order:
#' attr1 attr2 attr3. You may input fewer attributes than there are on your
#' data.
#'
#' \code{cost_ids} To identify cost type attributes, i.e. a lower value is
#' better for the user of the product configurator.
#'
#' Keep in mind you will probably not use this function on itself, it will
#' receive its parameters from a parent funciton, that is why is it not fully
#' documented.
#'
#' @return reference points. A numeric vector with default values if nothing is
#'   given or the ones inputed by the user. For one specific user only.
#'
#' @examples
#' ref_points(play_data, 9)
#' ref_points(play_data, 9, attr=c(1,3)) # returns numeric vector with length 2.
#' ref_points(play_data, 9, attr=c(1,3,4,2)) # returns numeric vector with length 4.
#' ref_points(data, 6, refps=c(0,-10,9,2)) # since there are exactly 4 attributes in 'data' this input is accepted.
#' ref_points(data, 20, refps=c(1,1), attr=c(1,2)) # Also works, since equal amount of refps and attr elements.
#'
#' #Some examples that throw an error
#' ref_points(data, 4, refps=c(1,2)) # Does not work since number of attributes in this case (4) does not equal number of refps.
#' ref_points(data, refps=c(0,0,0), attr=c(1,4)) # Same error as previous example
#'
#' @export
#'

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

  print(refps)
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

referencePoints <- function(dataset, userid, refps = NULL, attr = NULL, cost_ids = NULL, ...) {
  attrnull <- is.null(attr)

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
  ## Actual getting of the Reference Points begins here
  fullattr <- identical(sort(get_attrs_ID(dataset)), sort(attr)) #Check if there are fullattr, either inputed or as default (line150)
  defaultRefps <- getDefaultRefps(dataset, userid) #Handels userid input errors

  ## Act on cost_ids
  defAndCostIdRefps <- benefitToCostAttr(dataset, defaultRefps, cost_ids)

  if(is.null(refps) & fullattr) {
    refps <- defAndCostIdRefps
  }
  else if(is.null(refps) & !fullattr) {
    refps <- defAndCostIdRefps
    refps <- lapply(refps[1:length(refps)], "[", attr)
    print("I am here 2")}
  else {
    #Bug fixed
    if(length(attr) != length(refps)){
      if(attrnull) {
        refps <- defAndCostIdRefps
        print("I am here 3..replacing the given refps into the lists, alike with benefitToCostAttr function")
      }
      else {
        stop("Amount of RefPoints entered doesn't equal amount of attributes you entered. Enter equal amount of attributes and RefPoints or none.")

      }
    }
    #refpsNames <- paste("rp", attr)
    #refps <- lapply(refps, setNames, refpsNames)
  }
  refps

}
