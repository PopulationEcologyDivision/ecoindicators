#----------------------------------
LandingsDiversity <- function(land=dat,grps=groups) {
	a <- unique(land[,c('YEAR','NAMES')])
	a$NSP <- NA
	for ( i in 1:nrow(a)) {
		h <- land[land$YEAR==a[i,1] & land$NAMES==a[i,2],]
		a[i,'NSP'] <- length(unique(h$SPECIES))
	}
	return(a)

}
