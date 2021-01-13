\name{nucleotideAlignmentatCpG}
\alias{nucleotideAlignmentatCpG}
\title{
Returns the sequence of nucleotides and base qualities along a read
}
\description{
Returns the sequence of nucleotides and base qualities at speicific positions along a read
}
\usage{
nucleotideAlignmentatCpG(alignment, cpg_island, cpg_position)
}
\arguments{
\item{alignment}{list containing a GAlignmentPairs metadata of the sequences and two DNAStringSet objects of sequences and base qualities}
\item{cpg_island}{list containing the chomosome, start, end and strand of the CpG island}
\item{cpg_position}{numeric vector for the genomic coordinates to query}
}
\value{
Invisibly a list containing a GAlignmentPairs metadata of the sequences and two data frames of sequences and base qualities
}
\author{
\cr
David Brown\cr
\email{brownd7@mskcc.org}\cr
}
\keyword{}
