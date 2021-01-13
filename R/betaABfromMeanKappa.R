'betaABfromMeanKappa' <- function(mean, kappa)
{
	if (mean <= 0 | mean >= 1) {
		stop("must have 0 < mean < 1")
	}
	if (kappa <= 0) {
		stop("kappa must be > 0")
	}
	a = mean*kappa
	b = (1.0-mean)*kappa
	return(invisible(list(a = a, b = b)))
}
#EOF