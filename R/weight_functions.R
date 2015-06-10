## See if it does something useful or delete

weight_overall_freq <- function(dataset, userid = NULL , attr =NULL, rounds = NULL, refps =NULL, cost_ids =NULL) {
  all_dec_matrices <- powerful_function(dataset, userid, FUN=decision_matrix, attr, rounds, refps, cost_ids)

  if(is.null(attr)) {
    length_attr <- length(get_attrs_ID(dataset))
  }
  else {
    length_attr <- length(attr)
  }
  sum_help <- rep.int(0, length_attr)

  for(i in all_dec_matrices) {

    help <- apply(i, 2, sum)
    sum_help <- sum_help + help
  }
  almost <- sum_help * c(1,1,1,10.5147)
  result <- almost/sum(almost)
  result

}
