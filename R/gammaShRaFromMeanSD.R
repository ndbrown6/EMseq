'gammaShRaFromMeanSD' <- function(mean, sd)
{
	if (mean <= 0) {
		stop("mean must be > 0")
	}
	if (sd <= 0) {
		stop("sd must be > 0")
	}
	shape = mean^2/sd^2
	rate = mean/sd^2
	return(invisible(list(shape = shape, rate = rate)))
}
#EOF