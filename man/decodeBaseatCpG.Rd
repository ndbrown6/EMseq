\name{decodeBaseatCpG}
\alias{decodeBaseatCpG}
\title{
Transforms the consensus sequence data frame into binary
}
\description{
Transfiorms teh consensus sequence data frame into binary based on the type
of treatment `mode` used to convert C>T
}
\usage{
decodeBaseatCpG(alignment, mode)
}
\arguments{
\item{alignment}{list containing a GAlignmentPairs metadata of the sequences and two data frames of sequences and base qualities}
\item{mode}{character string indicating the treatment used to convert C>T. `TCC` corresponds to bisulfite treatment}
}
\value{
Invisibly a named matrix with 0 and 1. NA are positions which which were paddded `+` or where another base was observed
}
\author{
\cr
David Brown\cr
\email{brownd7@mskcc.org}\cr
}
\keyword{}
