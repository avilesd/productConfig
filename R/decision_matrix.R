#' Create the decision matrix
#'
#' Creates a decision matrix, which is necessary to create the 'Gains' and 'Losses' matrices after. If no attribute vector is given,
#' containing the attributes IDs and if no alternatives are passed on as rounds, to be considered in the decision matrix, the function
#' will go with the defaults and extract them from the dataset. Will be used only for one user (one userid).
#'
#' @param dataset (CHANGED, can give both tables: small and originial) SHOULD ONLY BE THE SMALL MATRIX FROM 1 USER.  PERHAPS LATER WE CHANGE IT.
#'
#' @param userid an integer, that identifies which rows of the whole given dataset (x) should be extracted.
#'
#' @param attr vector of integer numbers corresponding to the attributes IDs you desire to use.
#' Default calculates with all attributes. Length := j.
#'
#' @param rounds vector of integer numbers corr esponding to the alternatives to use for the decision matrix. Default calculates
#' first round(initia product config) and last round of the session. Length:= i.
#' Default calculates with first and last attributes (initial and final product configuration). To choose all give "all" as argument
#' for rounds, see example.
#'
#' @param cost_ids vector of integer numbers. To convert selected cost attributes into benefit attributes. If one or more of the
#' attributes in your data is of cost type, e.g. price, so that lower is better then you should identify this attributes as such,
#' providing their id, they'll be converted to benefit type (higher amount is better). Default assumes all are benefit type.
#'
#'
#' @return A decision matrix with j amount of columns and i amount of rows. Colnames = attrIDs and rownames = chosen rounds.\code{user_id}.
#' @examples
#' decision_matrix(camera2_config_data, 11)
#' decision_matrix(my_data, userid = 11, attr = c(1,3,5))
#' decision_matrix(another_data, userid = 80, rounds = c(1,2,3,7,8,9))
#' decision_matrix(data2, 2, rounds = "all")
#' @export
#'

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
