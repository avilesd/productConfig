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
