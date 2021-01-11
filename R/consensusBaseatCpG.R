'consensusBaseatCpG' <- function(pileup)
{
	uindex = !duplicated(pileup$seqs[,"read_name"])
	if (sum(!uindex)!=0) {
		useqs = pileup$seqs[uindex,,drop=FALSE]
		uquals = pileup$quals[uindex,,drop=FALSE]
		dseqs = pileup$seqs[!uindex,,drop=FALSE]
		dquals = pileup$quals[!uindex,,drop=FALSE]
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
		pileup$seqs = useqs
		pileup$quals = uquals
	}
	pileup$metadata = pileup$metadata[pileup$seqs[,"read_name"]]
	
	return(invisible(pileup))
}