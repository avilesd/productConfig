#' Get the Aspiration Leves (Reference Points) UNITL HERE: not complete.
#'
#' Description
#'
#' @param dataset copy later
#'
#' @param userid copy later
#'
#' @param refps numeric vector. Reference Points: each point corresponds to one attribute, i.e. each attribute has only one
#'        aspiration level. Default setting assumes the aspiration levels as the attr-levels of the initial product configuration
#'        for each user.
#'
#'
#' @return d \code{user_id}.
#' @examples fadfad
#'
#' @export
#'

ref_points <- function(dataset, userid, refps = NULL, cost_ids = NULL, ...) {

  if(is.null(refps)) {
    refps <- get_all_default_rps(dataset, userid)
  }

  else {
    ##HUGE BUG, WHAT HAPPENS IF YOU ONLY WANT TO COUNT WITH ONE ATTR, RIGHT FROM THE BOTTOM GET REAL REFPS DEPENDING ON
    ## GIVEN ATTR AND IF GIVEN REFPS or remove warning altogether, but then user has to provide SAME  ATTR and SAME REFPS
    ##Prove if amount of given RP equals amount of attributes
    if(length(get_attrs_ID(dataset)) != length(refps)){
      warning("Amount of RefPoints entered doesn't equal amount of attributes in your table.")
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
