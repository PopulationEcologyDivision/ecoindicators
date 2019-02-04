#'Filter Indicators
#'The selected indicators will be set to NA's for a given data frame

filterIndicators <- function(x=NULL, path,file.name,indicators.to.NA,overwrite=F,save=F)
    {
  if(is.null(x)) {
  a = file.path(path,file.name)
  aa = read.csv(a)
  }
  aa =x
  b = names(aa)
  for(i in 1:length(indicators.to.NA)) {
    bi = grep(indicators.to.NA[i],b)
    aa[,bi] <- NA
  }
  if(save) {
  if(overwrite) write.csv(aa,file=a,row.names = F)
  if(!overwrite) write.csv(aa,file=file.path(path,paste('NArm',file.name)),row.names = F)
print('saved')
  }
  return(aa)
}