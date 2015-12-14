## @knitr myfunctions

# D.L: custom functions for MSc thesis
# list of custom functions
# 

#display start & end of (xts) object.
xtsdates<-function(x){
  if(is.xts(x)){
  s<-start(x); e<-end(x); s<-c(s,e);s
  }
  else stop("argument x not a xts object")
}

#adjust xts series to desired time window STARt-to-END
adjw<-function(x,START,END){

  if(is.xts(x)){
    if(START<=start(x)||END>=end(x)) warning("START/END date out of range of available time series data, kept max available time window for")
    WNDW<- paste(as.Date(START),"/",as.Date(END),sep="")
    x<-x[WNDW];x
  }
  else if(is.environment(x)){
    n<-ls(x); #list of elements in environment
    for (i in 1:length(n)){
      if(is.environment(x[[n[i]]])||is.xts(x[[n[i]]])) x[[n[i]]]<-adjw(x[[n[i]]],START,END)
      else cat("skipped",as.character(ls(x)[i]),": not a xts or environment\n")
    }
    x
  }
  else stop("x has to be of type environment/xts")
}

# plot all xts objects in the defined workspace and save to pdf
plotxts<-function(x=.GlobalEnv,debug=F){
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
      plotxts(x[[j]],debug)
    }
    else if(is.xts(x[[j]])){
      f<-paste(as.character(names(x[[j]][,1])),"_",format(Sys.Date(),"%Y%m%d"),"_",format(Sys.time(),"%H%M%S"),"_",j,sep="")
      
      if(debug==T){
        print("entered else if")
        scan();
        cat("f set to:",f,sep="")
      }
      
      dev.new()
      #######################
      pdf(file=file.path("plots",paste(f,".pdf",sep="")))
      #######################
      parold<-par(mfrow=c(2,2))
      for(i in 1:dim(x[[j]])[2]){
        if(debug==T){
          cat(paste(i,dim(x[[j]][2]),"\n",sep=" "))
        }
        plot(x[[j]][,i],type='l', main=names(x[[j]][,i]))
        #cat("finished plot")
      }
      if(debug==T) print("finished plot loop")
      par(parold)
      dev.off()
      cat(paste("created <WD>/plots/",f,".pdf\n",sep=""))
    }
  }
  options(op);
}

#### config workspace ####
# thesiswd<-"D:/GoogleDrive/ETH/2015_FS/MSc_Thesis/2_data"
# myconfig<-function(wd){
#   if(missing(wd)||!is.character(wd)) {
#     cat(paste("Argument wd not specified or not of type character,will continue with current work directory: ", getwd(),sep=""))
#     wd<-getwd();
#   }
#   if(file.exists(wd)) try(setwd(wd))
#   else {dir.create(wd); setwd(wd); cat(paste("created new directory",wd,sep=""))}
#   
#   if(file.exists(file.path(wd,"rsetup","pkg.csv"))) pkg <- read.csv(file.path(wd,"rsetup","pkg.csv"),header=T)
#   else {
#     cat("Did not find required packages, will configure for packages\n ggplot2\n xts")
#     file.create(file.path(wd,"rsetup","pkg.csv"))
#     write.csv(c("ggplot2","xts"),file=file.path(wd,"rsetup","pkg.csv"),row.names=F)
#   }
#   print(pkg);
#   scan();
#   
#   if (length(setdiff(pkg[,1],rownames(installed.packages())))) {
#     install.packages(setdiff(pkg[,1],rownames(installed.packages())))
#     cat(paste("installed packages:",setdiff(pkg[,1],rownames(installed.packages())),sep=""))
#   }
#   lapply(pkg,require)
# }
