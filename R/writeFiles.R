writeFiles <- function(path,x,na,ind=ind,stratifiedQ) {
		fp <- file.path(path,"output","Combined Indicators")
	    dir.create(fp,recursive=T,showWarnings=F)
	    #fp1 <- file.path("R:","Shared","ACook","SPERAOutput")
        #dir.create(fp1,recursive=T,showWarnings=F)        
 				if(stratifiedQ) h <- paste(ind,'stratifiedq',sep="")       
 				if(!stratifiedQ) h <- paste(ind,'setq',sep="")       
		        if(na=="csv.ewq"   ) na <- paste(h,".csv",sep="")
                if(na=="csv.ewnq"  ) na <- paste(ind,"nonq.csv",sep="")
        write.csv(x,file.path(fp,na),row.names=F)        
        #write.csv(x,file.path(fp1,na),row.names=F)        
                }