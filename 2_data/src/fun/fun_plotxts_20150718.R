plotxts<-function(x=.GlobalEnv,debug=F){
  if(is.environment(x)==F)  warning("x is not an environment, will atempt to plot all xts in .GlobalEnv")
  lst<-ls(x);
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
      plotxts(x[[j]])
      }
    else if(is.xts(x[[j]])){
      f<-as.character(names(x[[j]][,1]))
      
      if(debug==T){
      print("entered else if")
      scan();
      cat(paste("f set to:",f,sep=""))
      }
      
      dev.new()
      pdf(file=paste("plots/",f,".pdf",sep=""))
      parold<-par(mfrow=c(2,2))
      for(i in 1:dim(x[[j]])[2]){
        if(debug==T) print(paste("entered plot loop",i,"of",dim(x[[j]][2])))
        plot(x[[j]][,i],type='l', main=names(x[[j]][,i]))
      }
      if(debug==T) print("finished plot loop")
      par(parold)
      dev.off()
      if(debug==T) cat(paste("created <WD>/plots/",f,".pdf",sep=""))
    }
  }
  rm(x,i,f,parold)
}