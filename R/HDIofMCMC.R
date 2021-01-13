'HDIofMCMC' <- function(x, HDI)
{
	sortedPts = sort(x)
	ciIdxInc = ceiling(HDI * length(sortedPts))
	nCIs = length(sortedPts) - ciIdxInc
	ciWidth = rep(0, nCIs)
	for (i in 1:nCIs) {
		ciWidth[i] = sortedPts[i+ciIdxInc] - sortedPts[i]
	}
	HDImin = sortedPts[which.min(ciWidth)]
	HDImax = sortedPts[which.min(ciWidth)+ciIdxInc]
	HDIlim = c(HDImin, HDImax)
	return(invisible(HDIlim))
}
#EOF