require(plm)

PlotPanelPDF <- function(df, f, mfrow){
  # required format of df: colums = region | time | variables | values
  if(missing(df)) stop("PlotPanelPDF: dataframe df not specified")
  
  ### 11/11/2015:
  ## works but doesnt entirely make sense.
  ## Directory check sequence messed up
  if(missing(f)) {
    if(dir.exists(file.path(getwd(),"output","plots"))){
    f <- file.path("output", "plots", 
                   paste("PlotPanelPDF_", 
                         format(Sys.time(), "%Y%m%d_%H%M%S"),
                         ".pdf", sep =""))
    } else {
      f <- paste("PlotPanelPDF_", format(Sys.time(), "%Y%m%d_%H%M%S"),".pdf", sep ="")
    }
    
  } else {
    f <- file.path("output", "plots", f)
  }
  ###  

  if(missing(mfrow)){
    mfrow <- c(1,1)
  }
    
  found.NA <- FALSE
  pdf(file = f)
  parold <- par(mfrow = mfrow)
  for(i in unique(df[,1])){ #for each region
    temp.subset <- subset(df, df[1] == i)
    for (j in unique(temp.subset[,3])){
      temp.subset2 <- subset(temp.subset, temp.subset[3] == j)
      if(any(is.na(temp.subset2[,4]))){
        found.NA <- TRUE
        temp.subset2[is.na(temp.subset2[,4]),4] <- 0
      }
      plot(x = temp.subset2[,2],
           y = temp.subset2[,4],
           type = "o",  
           main = paste("Plot of variable \"", j , "\"\nin region \"", i,"\"" ),
           xlab = colnames(temp.subset[2]),
           ylab = colnames(temp.subset2[4]))
    }
  }
  par(parold)
  dev.off() 
  
  cat("Plotted to file: \n", file.path("<WD>",f),"\n")
  if(found.NA == TRUE) 
    cat(" -- found NA in plot data, setting to 0 in plot\n")
}