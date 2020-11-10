'LL' <- function(p, n, a_hat, b_hat)
{
	ll = 1
	x = c(rep(0, round((1-p)*n)), rep(1, round(p*n)))
	for (i in 1:n) {
		ll = ll * exp(lgamma(x[i]+a_hat) + lgamma(1-x[i]+b_hat) - lgamma(a_hat+b_hat+1) - lbeta(a_hat, b_hat))
	}
	return(invisible(ll))
}
