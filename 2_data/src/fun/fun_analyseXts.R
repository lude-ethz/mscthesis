## @knitr analyze
### script for performing descriptive statistics
#Timeframes/Meanmedian/Stdev/acorreltions/ccorrelations..

require(xts)

AnalyzeXts <- function(x=.GlobalEnv,debug=F){
  if(is.environment(x)==F)  warning("x is not an environment, will atempt to plot all xts in .GlobalEnv")
  lst<-ls(x);
  op<-options(digits.secs=6)
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
      ## @knitr ------
      cat("#############\nData for environment ",j, "\n#############\n")
      
      analyze(x[[j]],debug)
    }
    else if(is.xts(x[[j]])){
    
      if(debug==T){
        print("entered else if for xts")
        scan();
      }
      
      cat("$",j,"\n")
      
      
      for (c in 1:ncol(x[[j]])){
        cat("$",j,"$", colnames(x[[j]][,c]),"\n")
        if(anyNA(x[[j]][,c])){
          cat("Attention: found NA's in series, will strip for statistics\n")
        }
        v<-subset(x[[j]][,c],complete.cases(x[[j]][,c]))
        mean <- mean(v)
        median <- median (v)
        std <- sd(v)
        minimum<- min(v)
        maximum<- max(v)
        acf<-acf(v, plot=F)
       
        plot(v, main=colnames(v))
        cat("####################\n")
        cat("  mean:  ",mean,"\n")
        cat("  median: ",median,"\n")
        cat("  Std.Dev: ",std,"\n")
        cat("  Min: ",minimum,"\n")
        cat("  Max: ",maximum,"\n")
        cat("####################\n")
        cat("ACF factors:\n")
        print(t(data.frame(lag=acf$lag,acf=acf$acf)))
        plot(acf,main=c("ACF for",colnames(v)))
        #scan()
      }

    }
  }
  options(op);

}