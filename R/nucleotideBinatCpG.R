'nucleotideBinatCpG' <- function(pileup, mode = "TCC")
{
	if (mode == "TCC") {
		# BS-seq and EM-seq
		ctmat = matrix(NA, nrow=nrow(pileup$seqs), ncol=ncol(pileup$seqs)-1)
		ctmat[pileup$seqs[,-1,drop=FALSE]=="C"] = 0
		ctmat[pileup$seqs[,-1,drop=FALSE]=="T"] = 1
		rownames(ctmat) = pileup$seq[,"read_name"]
		colnames(ctmat) = colnames(pileup$seq)[-1]
	} else if (mode == "CTT") {
		# TAP-seq
		
	} else if (mode == "TTC") {
		# TAB-seq
		
	} else if (mode == "CCT") {
		# CAP-seq
		
	}

	return(invisible(ctmat))
}
