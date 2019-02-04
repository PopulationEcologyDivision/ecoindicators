rename <- function(path,oldnames,ind) {
	a 			<- read.csv(file.path(path,"extra info","variableNameRecode.csv"),header=F)
	n 			<- length(oldnames) 
	ol 			<-  strsplit(oldnames,ind)
	oldnames 	<- numeric()
	for(i in 1:length(ol)) {
		oldnames[i] <- paste(ol[[i]],collapse="")
		}
	newnames 	<- oldnames
	for(i in 1:n) {
			j <- which(a[,2] == oldnames[i])
			if(length(j)>0) newnames[i] = a[j,1]
		}
	return(newnames)
}