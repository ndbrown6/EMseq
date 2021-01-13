\name{readAlignmentatCpGIsland}
\alias{readAlignmentatCpGIsland}
\title{
Reads a sequence alignment from a BAM file at the given coordinates of a CpG Island
}
\description{
Reads paired-end sequences from a BAM file given the coordinates of a CpG Isalnd and returns
the corresponding sequence alignment and base qualities
}
\usage{
readAlignmentatCpGIsland(bam, bai, chromosome, start, end, strand)
}
\arguments{
\item{bam}{character string specifying path to BAM file}
\item{bai}{character string specifying path to index file. Should not contain extension}
\item{chromosome}{character [X, Y, MT] or numeric [1-22] string of query chomosome}
\item{start}{numeric string defining start of genomic coordinate to query}
\item{end}{numeric string defining end of genomic coordinate to query}
\item{strand}{character string specifying which reads to return. Forward (+) or reverse (-)}
}
\value{
Invisibly a list containing a GAlignmentPairs metadata of the sequences returned and two DNAStringSet objects of sequences and base qualities 
}
\author{
\cr
David Brown\cr
\email{brownd7@mskcc.org}\cr
}
\keyword{}
