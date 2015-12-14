require(xts)

# plot all xts objects in the defined workspace and save to pdf ----------------
plotxts <- function(x=.GlobalEnv,debug=F){
  if(is.environment(x)==F)  
    warning("x not an environment, will atempt to plot all xts in .GlobalEnv")
  lst <- ls(x);
  op <- options(digits.secs=6)
  for (j in lst){
    if(debug==T){
      print("entered outer loop")
      print(x);
      print(j);
      print(paste("is.environment(x[[j]]):",is.environment(x[[j]]),sep=""))
      print(paste("is.xts(x[[j]]):",is.xts(x[[j]]),sep=""))
      scan();
    }
    
    if(is.environment(x[[j]])){
      if(debug==T){
        print("about to call self");
        scan();
      }
      plotxts(x[[j]],debug)
    } else if(is.xts(x[[j]])){
      f <- paste(as.character(names(x[[j]][,1])),"_",format(Sys.Date(),"%Y%m%d"),"_",format(Sys.time(),"%H%M%S"),"_",j,sep="")
      
      if(debug==T){
        print("entered else if")
        scan();
        cat("f set to:",f,sep="")
      }
      
      dev.new()
      
      pdf(file=file.path("plots",paste(f,".pdf",sep="")))
      
      parold <- par(mfrow=c(2,2))
      for(i in 1:dim(x[[j]])[2]){
        if(debug==T)
          cat(paste(i,dim(x[[j]][2]),"\n",sep=" "))
        
        plot(x[[j]][,i],type='l', main=names(x[[j]][,i]))
      }
      
      if(debug==T) 
        print("finished plot loop")
      
      par(parold)
      dev.off()
      cat(paste("created <WD>/plots/",f,".pdf\n",sep=""))
    }
  }
  options(op);
}