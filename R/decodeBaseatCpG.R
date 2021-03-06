'decodeBaseatCpG' <- function(alignment, mode)
{
	decoded = matrix(NA, nrow = nrow(alignment$seqs), ncol = ncol(alignment$seqs)-1)
	if (mode == "TCC") {
		decoded[alignment$seqs[,-1,drop=FALSE] == "C"] = 0
		decoded[alignment$seqs[,-1,drop=FALSE] == "T"] = 1
		rownames(decoded) = alignment$seq[,"read_name"]
		colnames(decoded) = colnames(alignment$seq)[-1]
	}
	return(invisible(decoded))
}
#EOF