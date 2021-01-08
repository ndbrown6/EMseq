'readAlignmentatCpG' <- function(bam_file, bai_file, chromosome, start, end, strand)
{
	url_bam = BamFile(file = bam_file, index = bai_file, asMates = TRUE)
	cpg_island = GRanges(seqnames = chromosome, ranges = IRanges(start, end), strand = strand)
	scan_flags = scanBamFlag(isPaired = TRUE,
				 isProperPair = TRUE,
				 isUnmappedQuery = FALSE, 
				 hasUnmappedMate = FALSE,
				 isMinusStrand = NA,
				 isMateMinusStrand = NA,
				 isFirstMateRead = NA,
				 isSecondMateRead = NA,
				 isSecondaryAlignment = FALSE,
				 isNotPassingQualityControls = FALSE,
				 isDuplicate = FALSE,
				 isSupplementaryAlignment = FALSE)
	what = c("seq", "qual")
	aln_metadata = readGAlignmentPairs(file = url_bam,
					   use.names = TRUE,
					   strandMode = 1,
					   param = ScanBamParam(which = cpg_island, flag = scan_flags))
	seq_quals = readGAlignments(file = url_bam,
				    use.names = TRUE,
				    param = ScanBamParam(which = cpg_island, flag = scan_flags, what = what))
	stacked_seqs = stackStringsFromGAlignments(x = seq_quals, region = cpg_island, what = what[1])
	stacked_quals = stackStringsFromGAlignments(x = seq_quals, region = cpg_island, what = what[2])
	return(invisible(list(metadata,
			      seqs = stacked_seqs,
			      quals = stacked_quals)))
}
