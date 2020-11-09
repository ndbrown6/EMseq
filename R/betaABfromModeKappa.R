'betaABfromModeKappa' <- function(mode, kappa)
{
	if (mode<=0 | mode>=1) {
		stop("must have 0 < mode < 1")
	}
	if (kappa<=2) {
		stop("kappa must be > 2 for mode parameterization")
	}
	a = mode*(kappa-2)+1
	b = (1.0-mode)*(kappa-2)+1
	return(invisible(list(a=a, b=b)))
}
