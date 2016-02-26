##' NEWTODO -
##'
##' BA-Liste
#' MUst do
##'
##' +++ Test overallPV further and then write new value functions.
##'
##' ++ Move to WRITING TODO: vectorized vs non-vectorized, latter faster for small amount of data, but for all users, first is a
##' lot faster, e.g. system.time(replicate(50,powerful_function(myData, all.users, FUN = gain_loss_matrices))) : user 30.78
##' system.time(replicate(50, gainLoss(myData, all.users)) : user 7.88
##'
##' Writing TODO: DOCU: Weights will only accept a weight vector which equals length of all attributes, if you want to ignore an
##' attribute, you have to set its weight to 0.0.
##'
##' Writing TODO:  DOCU: New functions must take into account attributes and calculate accordingly, perhaps it doesn't make sense with our data,
##  but we have to give the choice
##'
##' ++ Try and catch dataset parameter input on low level functions.
##'
##' current: MostImportant--Vectorize functions, meaning that when given two or more IDs,
##'       then able to calculate it, without powerful_function.
##'
##' -when input @referencePoints refps contains NA and forceRefps = F, weird things happen
##'
#' Should do
##'
##' +Allow the possibility to get attrIds by userid. Would work for data with different attributes per user.
##' Low-level functions
##'
##' ++Benchmarking execution time and compare with older version. (powerful_function)
##'
##'
#' COuld do
##'
##' +Decide: name rows/cols? Missing in gains and losses.
##'
##' Optimize gain and loss vector-functions @gainFunction to reduce processing time. Compared to powerful, slower for
##' small quantites of data userid = 9:11 but a lot faster for AllUsers.
##'
##' -Test if using less variables and reusing (eg @decisionMatrix) performs faster.
##'
##' -Do a test environment: Asses. Test if results from old functions are identical to the one of new functions.
##'
##' ++Allow data.frame input in refps parameter, not only same vector for all users.
##'   helpful code: create boolean.list from input: lapply(someList, function(tempData) !is.na(tempData))
##'                 see if vector: if(is.vector(someVector) & !is.list(someVector))
##'                 todo: lapply and change each list element[bool.list] with refps-list[bool.list]
##'
##'
##' Won't do
##'
##' Write: Instead of doing everything on lists, do it on a single data.frame or matrix with an id column,
##' perhaps faster.
##'
##'
##'
#'  FIXED
##' --Give labels to some function outputs, e.g. getDefaultRefpsById, old function gave labels. But not that important.
##'  in powerful_function it was with loops (sort of cheating and way it was built). [setNames]
##'
##'  + Create new function to costify the lists, initially for 'referencePoints' but also to be use later.
##'
##' +++ @'ReferencePoints.R' Rethink Reihenfolge, which parameter has more weight, and according to that, execute
##' partition of refps. Before partitioning do calculations, e.g. cost_ids. Paper first -->Fix 'referencePoints'
##' cost_ids colliding with attr, quick fix. Common denomninator?
##'
##' ++ Fix 'referenecPoints' what happens to list-result when user inputs refps parameter?
##'
##'
##'
