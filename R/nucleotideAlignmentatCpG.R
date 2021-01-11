'nucleotideAlignmentatCpG' <- function(alignment, cpg_island, cpg_position)
{
	pileup = list()
	for (i in 1:length(cpg_position)) {
		pileup[[i]] = data.frame(subseq(alignment$seqs, start=cpg_position[i]-cpg_island[["start"]]+1, end=cpg_position[i]-cpg_island[["start"]]+1))
	}
	pileup = do.call(cbind, pileup)
	colnames(pileup) = paste0("cg", cpg_position)
	npileup = cbind("read_name" = names(alignment$seqs), pileup)
	
	pileup = list()
	for (i in 1:length(cpg_position)) {
		pileup[[i]] = data.frame(subseq(alignment$quals, start=cpg_position[i]-cpg_island[["start"]]+1, end=cpg_position[i]-cpg_island[["start"]]+1))
	}
	pileup = do.call(cbind, pileup)
	colnames(pileup) = paste0("cg", cpg_position)
	qpileup = cbind("read_name" = names(alignment$seqs), pileup)
	
	return(invisible(list(metadata = alignment$metadata,
			      seqs = npileup,
			      quals = qpileup)))
}

