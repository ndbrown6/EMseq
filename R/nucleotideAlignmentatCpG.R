'nucleotideAlignmentatCpG' <- function(alignment, cpg_island, cpg_position)
{
	seq_align = list()
	for (i in 1:length(cpg_position)) {
		seq_align[[i]] = data.frame(subseq(alignment$seqs, start=cpg_position[i]-cpg_island[["start"]]+1, end=cpg_position[i]-cpg_island[["start"]]+1))
	}
	seq_align = do.call(cbind, seq_align)
	colnames(seq_align) = paste0("cg", cpg_position)
	seq_align = cbind("read_name" = names(alignment$seqs), seq_align)
	
	qual_align = list()
	for (i in 1:length(cpg_position)) {
		qual_align[[i]] = data.frame(subseq(alignment$quals, start=cpg_position[i]-cpg_island[["start"]]+1, end=cpg_position[i]-cpg_island[["start"]]+1))
	}
	qual_align = do.call(cbind, qual_align)
	colnames(qual_align) = paste0("cg", cpg_position)
	qual_align = cbind("read_name" = names(alignment$seqs), qual_align)
	return(invisible(list(metadata = alignment$metadata, seqs = seq_align, quals = qual_align)))
}
