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
#'Estas eran mas que todo para el Poster, para poder ensenar los graficos.
#'Enfocarse mas en las weight_functions, mira el papel, dejar la value function
#'como esta y documentarla, esto solo como extra, ignore o play.
#'
#'as dos funciones solo son para poder sacar la value function mas simple,
#'simple weighted additive function, gain_matrix sin normalizar y multiplicarlo
#'por los pesos, no es igual que el paper pero podria ser util, decide quickly.
#'
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

test_function_within_function <- function(x, play) {

    second_function <- function (x,play) {
        result <- x *play
        result
    }

    result <- second_function(x, play)
    result
}

test <- function(list1) {

  help <- numeric(0)

  for(i in list1){
    help <- c(help, sum(i))
  }
  help
}

lengthp <- function(list1) {
  help <- numeric(0)

  for(i in list1){
    help <- c(help, length(i))
  }
  help
}
