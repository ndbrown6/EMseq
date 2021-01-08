'readAlignmentatCpG' <- function(bam, bai, chromosome, start, end, strand)
{
	url_bam = BamFile(file = bam, index = bai, asMates = TRUE)
	cpg_island = GRanges(seqnames = chromosome, ranges = IRanges(start, end), strand = strand)
	scan_flags = scanBamFlag(isPaired = EMseq::.EMenv$isPaired, isProperPair = EMseq::.EMenv$isProperPair, isUnmappedQuery = EMseq::.EMenv$isUnmappedQuery, 
				 hasUnmappedMate = EMseq::.EMenv$hasUnmappedMate, isMinusStrand = EMseq::.EMenv$isMinusStrand,
				 isMateMinusStrand = EMseq::.EMenv$isMateMinusStrand, isFirstMateRead = EMseq::.EMenv$isFirstMateRead,
				 isSecondMateRead = EMseq::.EMenv$isSecondMateRead, isSecondaryAlignment = EMseq::.EMenv$isSecondaryAlignment,
				 isNotPassingQualityControls = EMseq::.EMenv$isNotPassingQualityControls, isDuplicate = EMseq::.EMenv$isDuplicate,
				 isSupplementaryAlignment = EMseq::.EMenv$isSupplementaryAlignment)
	what = c("seq", "qual")
	metadata = readGAlignmentPairs(file = url_bam,
				       use.names = TRUE,
				       strandMode = 1,
				       param = ScanBamParam(which = cpg_island, flag = scan_flags))
	seq_quals = readGAlignments(file = url_bam,
				    use.names = TRUE,
				    param = ScanBamParam(which = cpg_island, flag = scan_flags, what = what))
	stacked_seqs = stackStringsFromGAlignments(x = seq_quals, region = cpg_island, what = what[1])
	stacked_quals = stackStringsFromGAlignments(x = seq_quals, region = cpg_island, what = what[2])
	return(invisible(list(metadata = metadata,
			      seqs = stacked_seqs,
			      quals = stacked_quals)))
}
