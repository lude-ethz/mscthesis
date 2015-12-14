require(urca)
panel.urtest <- function(DF, conf.ival = c(10, 5, 1)){
  # Description
  # perform unit root testing for a melted data frame of panel data.
  
  # input type:
  #   DF = [region | time | variable | value ]
  
  # output type: 
  #   has.unit.root = list with three elements. each element represents one type
#                   trend adjustment for the  ADF test "no.trend", "w.drift", 
  #                   "w.time.trend". Each list element is a Data frame of 
  #                   True or false.
  #                   True = cannot reject Unit root!
  #                   False = can reject Unit root.
  if(conf.ival == 10){
    conf.ival <- 3
  } else {
    if(conf.ival == 5){
      conf.ival <- 2
    } else {
      if(conf.ival == 1){
        conf.ival <- 1 
      } else {
        warning("Confidence Interval not recognized, setting to 5%")
        conf.ival <- 2 
      }
    }
  }
  
  # initialize result data frames, one for each type of unit root test:
  # no trend adjustment, with drift, with trend
  result.nrow <- length(unique(DF$region))
  result.ncol <- length(unique(DF$variable))
  result.none <- data.frame(
                  matrix(data = NA, 
                         nrow = result.nrow, 
                         ncol = result.ncol+1, 
     dimnames = list(1:result.nrow, c("region", as.character(unique(DF[,3]))))))
  result.none$region <- unique(DF$region)
  result.drift <- result.none
  result.trend <- result.none
  
  # perform unit root test for each region, each variable, three types
  for(.i in unique(DF[,1])){
    for(.n in unique(DF[,3])){
      # create subeset for region - variable combination
      temp.subset <- DF[(DF[,1] == .i &DF[,3] == .n),]
      # run three different UR tests
      ur.adf.none <- ur.df(temp.subset[,4], type = "none")
      ur.adf.drift <- ur.df(temp.subset[,4], type = "drift")
      ur.adf.trend <-ur.df(temp.subset[,4], type = "trend")
      # do we reject at confidence conf.ival for each type of test?
      if(ur.adf.none@teststat[1] < ur.adf.none@cval[conf.ival]){
        result.none[result.none$region == .i,as.character(.n)] <- FALSE
      } else { 
        result.none[result.none$region == .i,as.character(.n)] <- TRUE
      }
      if(ur.adf.drift@teststat[1] < ur.adf.drift@cval[1,conf.ival]){
        result.drift[result.drift$region == .i,as.character(.n)] <- FALSE
      } else { 
        result.drift[result.drift$region == .i,as.character(.n)] <- TRUE
      }
      if(ur.adf.trend@teststat[1] < ur.adf.trend@cval[1,conf.ival]){
        result.trend[result.trend$region == .i,as.character(.n)] <- FALSE
      } else { 
        result.trend[result.trend$region == .i,as.character(.n)] <- TRUE
      }
    }
  }
  has.unit.root <- list(no.trend = result.none, 
                        w.drift = result.drift, 
                        w.time.trend = result.trend)
  return(has.unit.root)
}