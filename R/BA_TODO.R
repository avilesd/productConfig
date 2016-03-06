##' NEWTODO -
##'
##' BA-Liste
#' MUst do
##'
##' ++ Move overallPV_interface to ProspectValuefunctions and change arguments name and docu for dual and trp.
##'
##' ++ Make consumption_fun in dualVM.oneAttr useful, do not transform, just run the function over it.
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
##' + Correct naming when only one attr selected in decMatrix().
##'
##' +Allow the possibility to get attrIds by userid. Would work for data with different attributes per user.
##' Low-level functions
##'
##' ++Benchmarking execution time and compare with older version. (powerful_function)
##'
##'
#' COuld do
##'
##' +Throw a warning when beta_f > beta_l > beta_g > beta_s is not the case, perhaps in trpValueFunction.
##' Do a quick test for refps and gains/losses (although this is tested in oPV).
##'
##' -Make examples that are able to run, change all data names and somehow make the user load it.
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
##' +++ For later iterations: Allow to enter data.matrix refps or as a list, to avoid ...oneAttr functions, make
##' it al quicker and to run it only with one function, instead of with overallPV_interace.
##'
##' Write: Instead of doing everything on lists, do it on a single data.frame or matrix with an id column,
##' perhaps faster.
##'
##'
##'
#'  FIXED
##'
##' +++ Add error catching for order of mr < sq < g (05.03.16)
##'
##' +++ Test overallPV further and then write new value functions.
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
