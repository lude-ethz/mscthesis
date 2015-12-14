# POSSIBLE ERROR SOURCE?: currently not taking any extra effort to handle input
# format of variables (whether log or level). 
# The final output is desired to be log likelihood. The formula for the log
# likelihood should already be assuming log input format. Therefore no
# additional handling should be necessary. However a double check would be good.
require(plm)
require(xts)
pLogL <- function(input, testing.environment = FALSE){
#### Description: <OUTDATED 02.12.2014> ####
#   function to create concentrated log likelihood with constraints of
#   homogeneous long run coefficients (theta_i = - beta_i' * psi_i = -beta' *
#   psi = theta for all i), for Anundsen 2014 working paper
# assumptions:
#   balanced panel, number of observations T_i = T for all i,
#   errors ~ Normal(0,sigma^2)
# required formatting
#   reg.in = [region | time | y | x1 | x2 | x3 |...| xn]... reg.in with all panel data
#   VAR = [region | sigma | phi | theta ]... dataframe with all likelihood 
#         variables of each region in each row
#### General Function structure: ####
# 1. extract variables and shape to useable form
# 2. calc LL for each region
# 3. return and cleanup
#### TODO: ####
# homogeneous requirement for theta - as per (pesaran1999, pooled mean
# group estimator), not yet implemented.
####
  
# 1. extract variables and shape to useable form -------------------------------
  pll <- 0 #initialize log likelihood value
  P <- input$args$P
  Q <- input$args$Q
  Logs <- input$args$Logs
  reg.in <- input$reg.in
  N.Indepvars <- length(input$args$model.factors)   # no. of independent vars
  reg.out <- input$reg.out
  .I <- length(unique(reg.in[,1]))  # no. of regions i
  .T <- length(unique(reg.in[,2]))  # no. of time periods t
  Name.Indepvars <- colnames(reg.in)[1:N.Indepvars+3]  # names of independent vars
  Name.Depvar <- colnames(reg.in)[3]  # name of dependent var
  
  # check input
  if(nrow(reg.in) != .I*.T)
    warning("reg.in doesn't seem to be a balanced panel: nrow(reg.in) != .N*.T\n")
  # apply log form for input data
  for(i in 1:length(Logs)){
    if(Logs[i])
      reg.in[,i+2] <- log(reg.in[,i+2])
  }
  
  for(region in unique(reg.in[,1])){
    SS <- reg.in[(reg.in[,1] == region),]
    SS.T <- nrow(SS) # no. time periods in region, for future unbalanced panels
    SS.idx <- rownames(SS)  # row index of SS
    SS.fitted <- reg.out$details$gm.pmg$fitted.values[SS.idx]
    SS.resid <- reg.out$details$gm.pmg$residuals[SS.idx]
    if(testing.environment){ #workaround for test_fun_pLogL.R:
      # in test, the rownames for fitted.values and residuals are numeric,
      # but rownames for SS are characters (as is normally the case with data)
      SS.fitted <- reg.out$details$gm.pmg$fitted.values[as.numeric(SS.idx)]
      SS.resid <- reg.out$details$gm.pmg$residuals[as.numeric(SS.idx)]
    }
    # to be able to use plm::lag and plm::diff, convert to pdata.frame()
    # order: 1./pdata.frame, 2./subset; otherwise lose region attrib. bc. const.
    SS<- pdata.frame(data.frame(reg.in))[(as.character(reg.in[,1]) == region),]
      # the convoluted pdata.frame(data.frame()) is an attempt at making sure
      # reg.in is in data frame format. It should already be as per reg.out
      # scripting, but to be sure for future
    
    # create dependent variable + lags & diffs, incl region and time index
    y <- SS[,1:3]
    y[[paste(Name.Depvar, ".l1", sep = "")]] <- lag(y[,3], 1) # pdata.frame lag!
    y[[paste(Name.Depvar, ".d1", sep = "")]] <- diff(y[,3], 1)# pdata.frame diff!
    
    # create indep variables + lags & diffs, incl. region and time index
    X <- SS[,-3]
    for(i in Name.Indepvars){
      X[[paste(i, ".l1", sep="")]] <- lag(X[[i]], k = 1)  # pdata.frame lag!
      X[[paste(i, ".d1", sep="")]] <- diff(X[[i]], lag = 1) # pdata.frame diff!
    }
    
    # create matrix W, consisting of lagged-differences of y, X
    #  (P and Q lags resp.)
    W <- SS[,1:2]
      # lagged-diffs of y
      for(j in 1:(P-1)){
        W[[paste(Name.Depvar,".d1.l",j,sep="")]] <-
          lag(y[[paste(Name.Depvar, ".d1", sep = "")]],j)
      }
      # lagged diffs of X
      for(j in 1:(Q-1)){
        for (i in Name.Indepvars){
          W[[paste(i,".d1.l",j, sep="")]] <- lag(X[[paste(i,".d1", sep="")]], j)
        }   
      }

    sigma.i <- sd(SS.resid, na.rm = TRUE)
    phi.i <- reg.out$details$gm.pmg$coefficients[2]  
      # coeff of lagged DEpendent variable
    theta.i <- reg.out$details$gm.pmg$coefficients[1:N.Indepvars+1]  
      # coeff of lagged INdependent variable
      # as per pesaran1999, theta = -(beta_i / phi_i)
    theta.hom <- NA # currently not needed. in future if using homogeneous longrun coefficients
    
    # trim beginning NAs in W,X,y
    #   careful, this should only be trimming values from the first few periods due to lags and differences
    #   currently, there souldnt be any trimming inbetween or at end of W
    #   POSSIBE FUTURE ERROR
    W <- na.trim(as.matrix(W[,-(1:2)]))
    X <- X[rownames(W),]
    y <- y[rownames(W),]
    
    
    # 2. calc LL for each region ---------------------------------------------------
    # calc some auxiliary terms for panel Log Likelihood:
    # eta := (y.l1-X*theta)
    eta <- cbind(
      y[,1:2],  # region and time
      eta = y[[paste(Name.Depvar, ".l1", sep = "")]] -
             as.matrix(X[,1:N.Indepvars + 2]) %*% theta.i)
    # term := (y.d1 - phi.i* eta)
    term <- cbind(
      y[,1:2],
      term = y[[paste(Name.Depvar, ".d1", sep="")]] - 
              phi.i * eta$eta)
      # term: y is ((.I*.T)-by-1) vector, phi.i is scalar, 
      # eta is ((.I*.T)-by-1)vector
    
    # H := Identity - W'*(W'*W)^(-1)*W'
    H <- diag(nrow(W))-W%*%solve(t(W)%*%W)%*%t(W)
    
    # calc log-likelihood of entire panel
    pll1 <- -.T / 2 * sum(log(2*pi) + log(sigma.i^2))
    pll2 <- -0.5 * sum(t(term$term) %*% H %*% term$term / sigma.i^2)
    
    pll <- pll + pll1 + pll2  
  }
  pll
}