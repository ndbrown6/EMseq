'consensusBaseatCpG' <- function(alignment)
{
	uindex = !duplicated(alignment$seqs[,"read_name"])
	if (sum(!uindex)!=0) {
		useqs = alignment$seqs[uindex,,drop=FALSE]
		uquals = alignment$quals[uindex,,drop=FALSE]
		dseqs = alignment$seqs[!uindex,,drop=FALSE]
		dquals = alignment$quals[!uindex,,drop=FALSE]
		for (i in 1:nrow(dseqs)) {
			index = which(useqs[,1]==dseqs[1,"read_name"])
			for (j in 2:ncol(dseqs)) {
				if (useqs[index,j] == "+" & dseqs[i,j] != "+") {
					useqs[index,j] = dseqs[i,j]
					uquals[index,j] = dquals[i,j]
				} else if (uquals[index,j] < dquals[i,j]) {
					useqs[index,j] = dseqs[i,j]
					uquals[index,j] = dquals[i,j]
				}
			}
		}
		alignment$seqs = useqs
		alignment$quals = uquals
	}
	alignment$metadata = alignment$metadata[alignment$seqs[,"read_name"]]
	return(invisible(alignment))
}
