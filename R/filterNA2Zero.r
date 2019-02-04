filterNA2Zero <- function(x=NULL,path=NULL,file.name=NULL,indicators.to.zero,overwrite=F,save=F)
    {
  if(is.null(x)){
  a = file.path(path,file.name)
  aa = read.csv(a)
  }
  aa = x
  b = names(aa)
  for(i in 1:length(indicators.to.zero)) {
    bi = grep(indicators.to.zero[i],b)
      for(j in bi) {
        if(indicators.to.zero[i]=='BiomassClupeids') browser()
        aa[which(is.na(aa[,j])),j] <- 0
      }
  }
  if(save) {
  if(overwrite) write.csv(aa,file=a,row.names = F)
  if(!overwrite) write.csv(aa,file=file.path(path,paste('NAZero',file.name)),row.names = F)
print('saved')
  }
  return(aa)
}