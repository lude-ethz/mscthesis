Reconstruct <- function(X,dx){
  # reconstructs a series from 
  #   X...an initial value of the series
  #   dx...changes in series X; dx[i] is x[i]- x[i-1]
  # (same as x(i)=x(i-1) + dx(i))
  
  # returns
  #   X .. which is Series X(0),X(1), ... , X(length(c))
  for(i in 1:length(dx)){
    X<-c(X, last(X) + dx[i])
  }
  return(X)
}