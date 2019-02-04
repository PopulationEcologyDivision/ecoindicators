largeSpeciesIndicator <- function(X,lmax=85,linf.data.table='indiseas_max_length',metric='BIOMASS') {
	ol <- sqlQuery(channel,paste("select * from ",linf.data.table,";",sep=""))
	ss <- ol$SPECIES[ol$MAXLEN99>lmax]
	uI <- unique(X$ID)
	out <- list()
	for(i in 1:length(uI)) {
	                Y <- X[X$ID==uI[i],]
	                A <- sum(Y[Y$SPECIES %in% ss, metric])
	                B <- sum(Y[, metric])
	            out[[i]] <- cbind(uI[i],A/B)
		}
		out <- as.data.frame(do.call(rbind,out))
		out[,2] <- as.numeric(out[,2])
		names(out) <- c('ID','LSI')
	return(out)
		}
