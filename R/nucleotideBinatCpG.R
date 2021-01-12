'nucleotideBinatCpG' <- function(pileup, mode = "TCC")
{
	if (mode == "TCC") {
		ctmat = matrix(NA, nrow=nrow(pileup$seqs), ncol=ncol(pileup$seqs)-1)
		ctmat[pileup$seqs[,-1,drop=FALSE]=="C"] = 0
		ctmat[pileup$seqs[,-1,drop=FALSE]=="T"] = 1
		rownames(ctmat) = pileup$seq[,"read_name"]
		colnames(ctmat) = colnames(pileup$seq)[-1]
	}
	
	return(invisible(ctmat))
}
