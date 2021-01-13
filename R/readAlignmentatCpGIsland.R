'readAlignmentatCpGIsland' <- function(bam, bai, chromosome, start, end, strand)
{
	bam_url = BamFile(file = bam, index = bai, asMates = TRUE)
	cpg_island = GRanges(seqnames = chromosome, ranges = IRanges(start, end))
	scan_flags = scanBamFlag(isPaired = EMseq::.EMenv$isPaired, isProperPair = EMseq::.EMenv$isProperPair,
				 isUnmappedQuery = EMseq::.EMenv$isUnmappedQuery, hasUnmappedMate = EMseq::.EMenv$hasUnmappedMate,
				 isMinusStrand = EMseq::.EMenv$isMinusStrand, isMateMinusStrand = EMseq::.EMenv$isMateMinusStrand,
				 isFirstMateRead = EMseq::.EMenv$isFirstMateRead, isSecondMateRead = EMseq::.EMenv$isSecondMateRead,
				 isSecondaryAlignment = EMseq::.EMenv$isSecondaryAlignment, isDuplicate = EMseq::.EMenv$isDuplicate,
				 isNotPassingQualityControls = EMseq::.EMenv$isNotPassingQualityControls,
				 isSupplementaryAlignment = EMseq::.EMenv$isSupplementaryAlignment)
	what = c("seq", "qual")
	metadata = readGAlignmentPairs(file = bam_url, use.names = TRUE, strandMode = 1, param = ScanBamParam(which = cpg_island, flag = scan_flags))
	seq_quals = readGAlignments(file = bam_url, use.names = TRUE, param = ScanBamParam(which = cpg_island, flag = scan_flags, what = what))
	seq_quals = seq_quals[names(seq_quals) %in% names(metadata)[data.frame(metadata)[,"strand.first"] == strand],]
	metadata = metadata[unique(names(seq_quals)),]
	seqs = stackStringsFromGAlignments(x = seq_quals, region = cpg_island, what = what[1])
	quals = stackStringsFromGAlignments(x = seq_quals, region = cpg_island, what = what[2])
	return(invisible(list(metadata = metadata, seqs = seqs, quals = quals)))
}
#EOF