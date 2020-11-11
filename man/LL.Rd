\name{LL}
\alias{LL}
\title{
Likelihood of cfDNA read being tumor or nomal derived
}
\description{
Returns the likelihood of a cfDNA read being tumor or normal derived
}
\usage{
LL(p, n, a_hat, b_hat)
}
\arguments{
\item{p}{scalar between 0 and 1 describing the observed proportion of methylated CpG
		 sites in the cfDNA read}
\item{n}{integer greater than 1 indicating the number of CpG sites being considered}
\item{a_hat}{scalar greater than 0 indicating the shape parameter of beta distribution}
\item{b_hat}{scalar greater than 0 indicating the shape parameter of beta distribution}
}
\note{
\code{a_hat} and \code{b_hat} describe the shape parameters of a \code{Beta(a, b)} distribution
for a set \code{n} CpG sites. The choice of values for \code{a_hat} and \code{b_hat} should be
informed by the proprotion of methylated CpG sites in a training set of tumor and normal
derived gDNA. Please refer to Figure 3 in Li et al. (2018) \url{https://doi.org/10.1093/nar/gky423}.
Reasonable estimates can be obtained using the utility functions in this package.
}
\value{
Invisibly the value of the likelihood
}
\seealso{
\code{\link{betaABfromMeanKappa}}
\code{\link{betaABfromMeanSD}}
\code{\link{betaABfromModeKappa}}
}
\author{
\cr
David Brown\cr
\email{brownd7@mskcc.org}\cr
}
\keyword{}
