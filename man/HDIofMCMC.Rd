\name{HDIofMCMC}
\alias{HDIofMCMC}
\title{
Highest Density Interval
}
\description{
Returns the Highest Density Interval (HDI) from a sample of representative values
estimated as the shortest credible interval
}
\usage{
HDIofMCMC(x, HDI)
}
\arguments{
\item{x}{vector of representative values from a probability distribution}
\item{HDI}{scalar between 0 and 1 indicating the mass within the credible interval that is
		   to be estimated}
}
\value{
Invisibly a vector containing the limits of the HDI
}
\author{
\cr
David Brown\cr
\email{brownd7@mskcc.org}\cr
}
\keyword{}
