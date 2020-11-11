\name{betaABfromModeKappa}
\alias{betaABfromModeKappa}
\title{
Beta parameters from mode and concentration
}
\description{
Returns the parameters (a, b) of a beta distribution from estimates of the mode and
concentration
}
\usage{
betaABfromModeKappa(mode, kappa)
}
\arguments{
\item{mode}{scalar between 0 and 1}
\item{kappa}{scalar greater than 2}
}
\value{
Invisibly a named list with parameters a and b
}
\seealso{
\code{\link{betaABfromMeanKappa}}
\code{\link{betaABfromMeanSD}}
}
\author{
\cr
David Brown\cr
\email{brownd7@mskcc.org}\cr
}
\keyword{}
