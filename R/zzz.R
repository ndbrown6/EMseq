.EMenv = new.env()

isFlag = c("isPaired" = TRUE, "isProperPair" = TRUE, "isUnmappedQuery" = FALSE, "hasUnmappedMate" = FALSE,
	   "isMinusStrand" = NA, "isMateMinusStrand" = NA, "isFirstMateRead" = NA, "isSecondMateRead" = NA,
	   "isSecondaryAlignment" = FALSE, "isNotPassingQualityControls" = FALSE, "isDuplicate" = FALSE,
	   "isSupplementaryAlignment" = FALSE)

for (i in 1:length(isFlag)) {
	assign(names(isFlag)[i], isFlag[i], envir=.EMenv)
}

rm(isFlag)
