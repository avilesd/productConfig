##' NEWTODO -
##'
##' BA-Liste
#' MUst do
##'
##' +++ @'ReferencePoints.R' Rethink Reihenfolge, which parameter has more weight, and according to that, execute
##' partition of refps. Before partitioning do calculations, e.g. cost_ids. Paper first -->Fix 'referencePoints' cost_ids colliding with attr, quick fix. Common denomninator?
##' ++ Fix 'referenecPoints' what happens to list-result when user inputs refps parameter?
##' ++ Try and catch dataset parameter input on low level functions.
##'
##' current: MostImportant--Vectorize functions, meaning that when given two or more IDs,
##'       then able to calculate it, without powerful_function.
##'
#' Should do
##'
##' ++Allow the possibility to get attrIds by userid. Would work for data with different attributes per user.
##' Low-level functions
##'
##'
#' COuld do
##'
##' -Do a test environment: Asses. Test if results from old functions are identical to the one of new functions.
##'
##'
##' Won't do
##'

##'
##'
##'
#'  FIXED
##' --Give labels to some function outputs, e.g. getDefaultRefpsById, old function gave labels. But not that important.
##'  in powerful_function it was with loops (sort of cheating and way it was built). [setNames]
##'
##'  + Create new function to costify the lists, initially for 'referencePoints' but also to be use later.
##'
##'
##'
