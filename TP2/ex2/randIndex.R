randindex <- function(P1, P2)
{
	# evaluates the similarity between the partitions P1 and P2, 
	# by computing the adjusted Rand Index 
	# Benjamin Quost, 2009.04.17 

	nbIn <- length(P1)
	if (length(P2) != nbIn)	{
    error;
	}

	cont1 <- matrix(rep(as.matrix(P1),nbIn),nrow=nbIn,byrow=T) ==
        matrix(rep(as.matrix(P1),nbIn),nrow=nbIn,byrow=F)
	cont2 <- matrix(rep(as.matrix(P2),nbIn),nrow=nbIn,byrow=T) ==
        matrix(rep(as.matrix(P2),nbIn),nrow=nbIn,byrow=F)

	res <- NULL
	res$comp <- cont1*cont2 + (!cont1)*(!cont2)
	res$rate <- sum(res$comp*lower.tri(res$comp))/sum(lower.tri(res$comp))
	return(res)
}
