#' TODO
#'
#' crisp and intervals (really later)
#' with param EACH, calculate for each round and the next the gains and losses, in decision matrix,
#'  an if at the end, if TRUE then another result.
#'
#'  (After Seminar) get_attr_ -> improve, do not assume that all users have the same amount of attributes, consequences?
#'
#' (FIXED) Error Catch: easy: by decision_matrix -- prove is provided userid is in the userid of the data.
#'
#'  (pending, IMPORTANT) Test cost_ids and if refps should also be given with -. Test with paper matrices current2.
#'
#' (pending, IMPORTANT) FIXED WEIGHTS AND WEIGHTS FUNCTION
#'
#' (pending) Alternatives 2c) a function that gives you all G & L matrices between chosen alternatives and their next
#' one. If rounds = c(x, x+1, x+2, x+3) then give all following G & L: G&L(x, x+1) ; G&L(x+1, x+2) ; G&L(x+2, x+3). Normalized?
#'
#' (pending, NECESSARY ASAP, last is implemented) allow for rounds= "first", "last".
#'
#' (pending, important) suppressWarning when giving rounds= vector not NULL and not "all", try a way around.
#'
#' (pending, IMPORTANT) Change all 'data' args to dataset or other!
#'
#' (pending, IMPORTANT) Change ref_ps function when called, add attr parameter and change function as well. attr already added
#' powerful_f.
#'
#' (DONE, IMPORTANT) Add powerful_functions to the powerful_function conditions (con7,8), think which ones.
#'
#' WORKS HEATMAP with the right data! multiply attr4 by 6.545 and then take 4 - that!
#'
#' heatmap.2(tprueba,col=my_palette, symm=F,symkey=F,symbreaks=T, scale="none", dendrogram="none", Colv= F, Rowv=F,
#' trace="none", breaks =colbr, margins(6,5))
#'
#' You can hack RStudio with Inspect!!!
#'
#' Invert prices for plots for example table with column 3 with normal prices:
#'  1) help <- newt[,3]*6.51
#'  2) help <- 4-help
#'  3)cbind(new, help)
#'
#'  ggplot(data = new, aes(x = xt, y = yt, color = pt)) + geom_line(size=1.1) + geom_point(size = 2)
#'
#'  (FINAL, going to Poster) ggplot(data = t2last, aes(x = x2, y = y2, color = p2last)) + geom_line(size=1.1) + geom_point(size = 2) + scale_colour_gradient(low="red", high="green")
#'
#'  ##### FIXED
#'








