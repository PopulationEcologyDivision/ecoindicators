predatoryFish <- function(X,pred.data.table='indiseas_pred_retained_wss',metric='BIOMASS') {
		#ol <- sqlQuery(channel,paste("select species from ",pred.data.table," where predatory=1;",sep=""))
		#ss <- ol$SPECIES
		ss <- c(10,11,12,13,14,15,16,19,30,31,40,41,42,43,44,50,51,52,111,112,114,119,122,
			142,143,149,150,156,200,201,202,203,204,220,221,241,300,301,303,304,307,320,400,410,
			411,412,414,500,501,604,619,620,621,623,626,640,641,642,647,712,741,880,2526,4511,141,
			49,25,59,32,33,747,72,190,192,71,172,256,73,230,238,246,231,233,4514,27)
	
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
			names(out) <- c('ID','pF')
		return(out)
	}
		