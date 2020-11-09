'gammaShRaFromModeSD' <- function(mode, sd)
{
	if (mode<=0) {
		stop("mode must be > 0")
	}
	if (sd<=0) {
		stop("sd must be > 0")
	}
	rate = (mode+sqrt(mode^2+4*sd^2))/(2*sd^2)
	shape = 1+mode*rate
	return(invisible(list(shape=shape, rate=rate)))
}
