# manual cointegration tests
# Engle Granger two step estimation

EGtest <- function(x, y, trend = c("trend", "none"), lag = 4){
  cat("--- EGtest() start ---------------------------------\n")
  
  # x <- national$hp
  # y <- national$hstock
  lag <- 4
  trend <- match.arg(trend)
  
  EGcrit <- data.frame(no_time_trend = c(-3.04,-3.34, -3.59, -3.90),
                        time_trend = c(-3.50, -3.78, -4.03, -4.32),
                        row.names = c("10pct", "5pct", "2pct", "1pct"))
  
  
  # prep
  if (length(x) != length(y))
    stop("x,y not of equal length")
  t <- 1:length(x)
  
  # step 1)
  # estimate y_t = a + b*x_t + u_t
  if(trend == "none"){
    lm1 <- lm(y ~ 1 + x, na.action = NULL)
  } else if(trend == "trend"){
    lm1 <- lm(y ~ 1 + x + t, na.action = NULL)
  } else {
    stop("trend input must be of type \"none\"/\"trend\"")
  }
  
  # step 2) check for unit root in u: 
  # estimate change_u_t = a + b1*u_(t-1) + b2*change_u_(t-1) + ... + e 
  u <- matrix(coredata(lm1$residuals))
  dftest_u <- ur.df(u, type = trend , lags = lag)
  # check test value against critical values in EG_crit
  # DO NOT COMPARE TO CRIT VALUES GIVEN IN ur.df(), as those are onyl for ADF test
  
  # result: if testvalue < crit in EG_crit -> b1 = 0, 
  #   then u_t has unit root ( u_t = a + 1* u_(t-1) + ...), 
  #   therefore x/y NOT cointegrated 
  
  # result: if testvalue < crit in EG.crit -> if b1 !=0, 
  #   then u _t does not have a unit root
  #   therefore x/y ARE COINTEGRATED.
  
  # output
  cat("Result of ADF on _residuals_ of cointegration relationship:")
  print(dftest_u)
  if(trend == "trend"){
    cat("using model WITH time trend\n\nCritical values:\n")
    print(EGcrit["time_trend"])
  }else{ 
    cat("using model WITHOUT time trend\n\nCritical values:\n")
    print(EGcrit["no_time_trend"])
  }
  cat("(remark: if test value below critical value -> evidence for cointegration)\n")
  cat("--- EGtest() end ---------------------------------\n\n\n")
} # end of function



# TODO: check use of diff() for lagged variables
# comment: diff(x, 1, 1) = cx
# comment: diff(x, 2, 2) = 
