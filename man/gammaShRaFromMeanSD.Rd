\name{gammaShRaFromMeanSD}
\alias{gammaShRaFromMeanSD}
\title{
Gamma parameters from mean and standard deviation
}
\description{
Returns the shape and rate parameters of a gamma distribution from estimates of the mean
and standard deviation
}
\usage{
gammaShRaFromMeanSD(mean, sd)
}
\arguments{
\item{mean}{scalar greater than 0}
\item{sd}{scalar greater than 0}
}
\value{
Invisibly a named list with parameters shape and rate
}
\seealso{
\code{\link{gammaShRaFromModeSD}}
}
\author{
\cr
David Brown\cr
\email{brownd7@mskcc.org}\cr
}
\keyword{}
