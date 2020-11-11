'LL' <- function(p, n, a_hat, b_hat)
{
	ll = 1
	r = c(rep(0, round((1-p)*n)), rep(1, round(p*n)))
	for (j in 1:n) {
		ll = ll * exp(lgamma(r[j]+a_hat) + lgamma(1-r[j]+b_hat) - lgamma(a_hat+b_hat+1) - lbeta(a_hat, b_hat))
	}
	return(invisible(ll))
}
