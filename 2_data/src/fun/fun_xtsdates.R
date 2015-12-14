# display start & end of (xts) object. -----------------------------------------
xtsdates <- function(x){
  if(is.xts(x)){
    s <- start(x)
    e <- end(x)
    (s <- c(s,e))
  } else {
    stop("argument x not a xts object")
  }
}