source("R/config2_functions.R")
source("R/DecisionMatrix.R")
source("R/GetFunctions.R")
source("R/TestFunctions.R")
source("R/PT-Functions.R")
source("R/DRPFunctions.R")
source("R/TRPFunctions.R")
source("R/WeightFunctions.R")
source("R/ProspectValueFunctions.R")
source("R/Normalizing.R")

# AClculate allTables1 with all prids as DM tables
allTables2 <- pridVectors(prids2, getAllPrids(prids2))

# Codify AllTables from PRIDs to 0,1,2,3 to put into !containsVector_...!!!!!
codifiedAllTables2 <- codifyAllTables2(allTables2)

# IMPORTANT -- adjust cost_ids --transformed cost ids
transformed2 <- transform4_config2(config2, approvedUsers2, rounds="all", cost_ids = 4)

#signalize rounds with prid with
containedTables2 <- containsVectors2_allPrids(transformed2, do.call(rbind, codifiedAllTables2))

# First if you cleaned from UserBase, need to clean CONFIG2 to
cleanConfig2<-config2[!(config2$usid==99),] # IFFF
cleanConfig2<-config2[!(config2$usid==85),] # IFFF

#################
# Preparation for Overall Values
pridDRPnames2 <- lapply(containedTables2, function(x) {rownames(x)})
#######

## PT -- changed norm.gainLoss?
fullOverallPV_2 <- overallPV(cleanConfig2, approvedUsers2, attr = 1:4, rounds="all", cost_ids = 4,
                             refps = c(1.5, 1.5, 1.5, 1.5))
#DRP
prefullOverallDRP_2 <- overallDRP(cleanConfig2, approvedUsers2, attr = 1:4, rounds="all", cost_ids = 4,
                                  dual.refps=dualRefPs)
fullOverallDRP_2 <- mapply(function(pvalues, names) {names(pvalues) <- names; pvalues},
                           prefullOverallDRP_2, pridDRPnames2)
#TRP
prefullOverallTRP_2 <- overallTRP(cleanConfig2, approvedUsers2, attr = 1:4, rounds="all", cost_ids = 4,
                                  tri.refps=triRefPs)

fullOverallTRP_2 <- mapply(function(pvalues, names) {names(pvalues) <- names; pvalues}, prefullOverallTRP_2,
                           pridDRPnames2)

## SORT all
sorted.fullOverallPV2 <- sapply(fullOverallPV_2, sort, decreasing=T)
sorted.fullOverallDRP2 <- sapply(fullOverallDRP_2, sort, decreasing=T)
sorted.fullOverallTRP2 <- sapply(fullOverallTRP_2, sort, decreasing=T)

listRankedPrids_PV2 <- listOfNames(sorted.fullOverallPV2)
listRankedPrids_DRP2 <- listOfNames(sorted.fullOverallDRP2)
listRankedPrids_TRP2 <- listOfNames(sorted.fullOverallTRP2)



