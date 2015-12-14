# adjust xts series to desired time window STARt-to-END -------------------------

require(xts)

adjw <- function(x,START,END){
  
  if(is.xts(x)){
    
    if(START<start(x)||END>end(x)) 
      warning("START/END date out of range of available time series data, kept 
              max available time window for")
    WNDW <-  paste(as.Date(START),"/",as.Date(END),sep="")
    (x <- x[WNDW])
    
  } else if(is.environment(x)){
    n <- ls(x); #list of elements in environment
    for (i in 1:length(n)){
      if(is.environment(x[[n[i]]]) || is.xts(x[[n[i]]]))
        x[[n[i]]] <- adjw(x[[n[i]]],START,END)
      else {
        cat("skipped",as.character(ls(x)[i]),": not a xts or environment\n")
      }
    }
    
  } else {
    stop("x has to be of type environment/xts")
  }
}