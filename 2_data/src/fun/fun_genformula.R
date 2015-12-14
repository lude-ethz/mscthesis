# Function Gen_formula ---------------------------------------------------------
GenFormula <- function(vars, logs,
                       p = 5, q = 5, form = c("plm", "dynlm")){
  # Function to create formula for ARDL/ECM model 
  # input:
  #   vars...list with variable names as characters
  #           assume data is sorted in increasing time index for increasing rownumber
  #   logs...vector, whether or not to use log form for variables,
  #   p......integer, number of lags of dependent variable
  #   q......integer, number of lags of independent variables
  #   form...character, formatting for plm() or dynlm(); 
  #          for plm use "lag()" and "diff()" for dynlm use "L()" and "d()" 
  #          to represent lagged and differenced variable in the formula
  # output:
  #   return...a string which can be interpreted as a formula for plm() or dynlm()
  #           depending on form
  
  
  # Todo: check variable input type:
  # ps, q...should be integer only
  # vars...should be list
  
  form <- match.arg(form)
  if(form == "plm"){
    dif <- "diff("
    lag <- "lag("
  } else {
    dif <- "d("
    lag <- "L("
  }
  
  if(p%%1 != 0){
    warning("# GenFormula():\n#  arg \"p\" not integer, dropping decimals \n")
    p <- p%%1
  }
  
  if(q%%1 != 0){
    warning("# GenFormula():\n#  arg \"q\" not integer, dropping decimals \n")
    q <- q%%1
  }
  
  if (missing(logs)){
    logs <- rep(FALSE, ncol(vars) - 1)
    warning("# GenFormula():\n#  arg \"logs\" missing, defaulting to TRUE  
            for all variables\n")
  }
  
  if(typeof(vars) != "list"){
    warning("# GenFormula():\n#  arg \"vars\" not a list,
            attempting to cast\n")
    vars <-as.list(vars)
  }

  
  # create funcitonal form log/level
    logs1 <- mapply(logs, FUN = function(x){r<-"";if(x == TRUE)r<-"log(";r})
    logs2 <- mapply(logs, FUN = function(x){r<-"";if(x == TRUE)r<-")";r})
  
    for (i in 1:length(vars)){
      vars[[i]] <- paste(logs1[i], vars[[i]],logs2[i], sep="")
    }
    rm(logs1, logs2)
  
  # create formula left hand side
    eq <- paste(dif, vars[[1]],") ~ 1")
  
  # create ECM term : alpha*( y_t-1 + beta* x_t-1)
  for (i in 1:length(vars)){
    tmp <- paste(" + ", lag, vars[[i]], ", 1)",sep = "")
    eq <- paste(eq, tmp, sep = "")
  }
  
  # create lagged dependent variable y
  if (p > 1){
    for (i in 1:(p-1)){
      tmp <- paste(" + ", lag, dif, vars[[1]], "), ",i,")", sep = "")
      eq <- paste(eq, tmp, sep = "")
    }
  }
  
  # add lagged independent variable x
  if (q > 1){
    for (i in 2:length(vars)){ #for each x
      for (j in 0:(q-1)){ # create q-1 lagged derences
        tmp <- paste(" + ", lag, dif, vars[[i]], "), ", j, ")", sep = "")
        eq <- paste(eq, tmp, sep = "")
      }
    }
  }
  as.formula(eq)
}