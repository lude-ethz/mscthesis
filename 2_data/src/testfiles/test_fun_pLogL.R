## TITLE:      test_fun_pLogL.R
## DESCRIPTION
# Purpose:    Test for the panel Log Likelihood pLogL() funciton
# Remark:     In this test, all model variables are set to be in logs format
## Notes:
# likelihood ratio testing:
# D = -2 ln(likelihood of H_0) + 2ln(likelihood of H_alt)
# H_0 = restircted model
# H_alt = unrestricted model
# -> distribution of D ~ chi^2 with df(H_alt model) - df(H_0 model)
####

#### SCRIPT START ##############################################################
require(plm)
require(ggplot2)
require(reshape2)

WD <- "D:/GoogleDrive/Work/MSc_Thesis/2_data"
setwd(WD)
source(file.path("src", "fun", "fun_pLogL.R"))

# DEFINE PARAMETERS ------------------------------------------------------------
# model parameters
factors.test <- c("x1", "x2", "x3", "x4")# names of independent variables  
.I <- 2  # [MIN = 2] regions/groups in test 
.T <- 100  # time periods per region in test, >= .P+2
.P <- 5  # no. of lags of dependent variable (in levels) 
.Q <- 5  # no. of lags of independent variable (in levels) 
# test parameters
N.test <- 25 # number of iterations to run
coef.sd <- 1  # sd used for generating random coefficients
coef.mean <- 0  # mean used for generating random coefficients
coef.scale <- 0.1 # coef scaling factor, for coef.scale*rnorm()
  # ~0.1 seems to be robust for reconstrucing Y based on ~100 iterations
coef.y.scale <- 0.1 # coef for lag(log(y)) term, multiplicative w/ coef.scale
  # if = 1, seemingly always causes exponential decrease of Y to 0, if 0.1, roughly random walk
y.stoch.mean <- 0  # mean used for generating stoch. part of y
y.stoch.sd <- 1  # sd used for generating stoch part of y
y.stoch.scale <- 1  # y = f(y,x1,... ,xn) + y.stoch.scale*rnorm()
plot.y <- TRUE  # plot overview of al generated Y plots?
plot.iterations <- FALSE # review each iterations' plots? mostly for debug
set.seed(22)  # set seed for RNG if needed for reproducible results 
  # (how does this influence each iterations random number generation?
  #  e.g. are the N-iterations of the test still different from each other? (should be?))
## end parameters ---


#### TEST START ################################################################
Rprof("profie_pLogL_test.out") # profile code below for debugging

# derive some more parameters
N.Indepvar.test <- length(factors.test)  # no. of independent variables
N.regression.terms <-   # number of regression terms:
  1+1+N.Indepvar.test+.P-1+(.Q)*N.Indepvar.test
  # diff(y)~const +lag y +lag(xn) +(p-1)*lag(diff(y)) +(q-1)*lag(diff(xn))
Logs.test <- rep(TRUE, N.Indepvar.test)  # use logs for all y, x1, ..., xn

# Initialize variables
N.exp.growth <- 0
N.stable <- 0
N.exp.decay <- 0
if(plot.y) all.iterated.y <- numeric()

LL.ratio.all <- vector(mode = "numeric")  # Log likelihood ratio of all test iterations
input <- list()  # initialize artificial regression input list
input$args <- list(P = .P,  # populate params
                   Q = .Q,
                   model.factors = factors.test,
                   Logs = Logs.test)
DF <- data.frame(  # initialize artificail regression input data
  i = rep(1:.I, each = .T),
  t = rep(1:.T, times = .I))

# GENERATE COEFFICIENTS ----------------------------------------------------------
t.start <- Sys.time()  
  # coef.test...data frame, columns are regions, rows are regression terms
  
  # unrestricted model: generate random coeficients
    coef.test <- data.frame(matrix(NA, ncol = .I, nrow = N.regression.terms))
    for(i in 1:.I){
      coef.test[,i]<- 
        coef.scale * rnorm(N.regression.terms, sd = coef.sd, mean = coef.mean)
    }
    colnames(coef.test) <- paste("i.", 1:.I, sep = "")
    rownames(coef.test) <- c(
      "intercept",
      "lag(log(y))",
      paste("lag(log(x", 1:N.Indepvar.test,"))", sep=""),
      paste("lag(diff(log(y)),", 1:(.P-1),")", sep=""),
      paste("lag(diff(log(x", rep(1:N.Indepvar.test,each = .Q),")),",
            rep(0:(.Q-1), times = N.Indepvar.test),")", sep="")
    )
    # constrain AR coefficient, so its a stable AR process:
    # alpha*y_(t-1) 
    # trial and error yielded [-0.001,0.001] as stable
      #coef.test["lag(log(y))",] <- coef.y.scale * coef.test["lag(log(y))",]
      coef.test["lag(log(y))",] <- sample(seq(- 0.001, 0.001, by = 0.0001), .I)
  # restricted model
    coef.r.test <- coef.test
    coef.r.test[c(1,2+(1:N.Indepvar.test)),] <- 0
    chisqdf <- 5 # set equal to number of 0 parameters


# SIMULATE INPUT/OUTPUT OF TestcoefHomogen ------------------------------------
  for(iteration in 1:N.test){
    # y.calc <- numeric() # storage for calculated y series from x1...xn
    y.calc <- vector(mode = "list", length = N.test)
    # create .P number of initial values for y in levels, 100 + random
 
    tmp.y <- vector(mode = 'list', length = .I)
    for(i in 1:.I){
      tmp.y[[i]] <- c(100+cumsum(rnorm(.P)),rep(NA,.T-.P))
    }
    DF$y <- unlist(tmp.y)
#     tmp.y <- numeric()
#     for(i in 1:.I){
#       tmp.y <- c(tmp.y, c(100+cumsum(rnorm(.P)),rep(NA,.T-.P)))
#     }
#     DF$y <- tmp.y
#     rm(tmp.y)

    # create independent variables in levels, 100 + random
    for(j in factors.test){
      tmp.x <- vector(mode = "list", length = .I)
      for(i in 1:.I){
        tmp.x[[i]] <- 100+cumsum(rnorm(.T))
      }
      DF[j] <- unlist(tmp.x)
    }
    rm(tmp.x)
#     for(j in factors.test){
#       tmp.x <- vector(mode = "numeric")
#       for(i in 1:.I){
#         tmp.x <- c(tmp.x,100+cumsum(rnorm(.T)))
#       }
#       DF[j] <- tmp.x
#     }
#     rm(tmp.x)
    
    # convert to panel-DF for easy handling of lagging and differencing
    # create lagged and differenced log-sequences in new panel-DF "model"

    pDF <- pdata.frame(DF)
    model <- pDF[,1:2]
    model <- cbind(model, diff(log(pDF$y)))
    colnames(model)[3] <- "diff(log(y))"
    for(i in 0:N.Indepvar.test){ # lagged long run variables (y, x1, ..., xn)
      model <- cbind(model,lag(log(pDF[[3+i]])))
      colnames(model)[ncol(model)] <- rownames(coef.test)[2+i]
    }  
    
    for(i in 1:(.P-1)){ # lagged dependent variable
      model <- cbind(model, lag(diff(log(pDF$y)),i))
      colnames(model)[ncol(model)] <- paste("lag(diff(log(y)),",i,")", sep = "")
    } # at this point, this series is mostly NA, will back out later
    
    for(i in 1:N.Indepvar.test){  # lagged differences of all independent variables
      for(j in 0:(.Q-1)){
        model <- cbind(model, lag(diff(log(pDF[[3+i]])),j))
        colnames(model)[ncol(model)] <- 
          paste("lag(diff(log(", colnames(pDF)[3+i] ,")),",j,")", sep = "")
      }
    }
    rm(pDF)

    
    # construct y given independent variables and initial y values
    t1 <- Sys.time()
    for(j in 1:.I){ # for each region
      SS <- model[model$i == j,] # take out regional subset
      # for each period in region(except .P periods in beginning due to lag)
      for(k in (.P+1):nrow(SS)){ 
        # calc y given x1...xn, lag(x1...xn) and lag(diff(y), 1...P)
        y.k <- sum(SS[k,4:ncol(SS)] * coef.r.test[2:N.regression.terms,j])
        # update SS with new iteration of y.k
        SS[k, 3] <- y.k  # update diff(log(y)) in SS
        for(l in 1:(.P-1)){ # update lag periods lag(diff(log(y)),l)
          if(k+l <= nrow(SS)) # if not out of bound at end
            SS[k+l,2+2+N.Indepvar.test+l] <- y.k  
        }
        if(k < nrow(SS))  # if not last row
          SS[k + 1, 4] <- SS[k, 4] + y.k  # update lag(log(y))
      }
      model[model$i == j,] <- SS # write back current region into model
      
      # back out level y for current region
      SS.y <- exp(SS[,4])[2:length(SS[,4])]
      # back out last period for level y of this region
      SS.y.last <- exp(SS[nrow(SS),4] + SS[nrow(SS),3])
      # append to y.calc for later use
      y.calc[[iteration]] <- c(SS.y, SS.y.last)
      
      if(any(SS.y == Inf) || SS.y.last > (SS.y[1] * 10)){
        N.exp.growth <- N.exp.growth + 1
      } else if(SS.y.last < SS.y[1] * 0.1){
        N.exp.decay <- N.exp.decay + 1
      } else {
        N.stable <- N.stable + 1
      }
      # check if not shrinking more than 10x initial value

      rm(SS.y.last)
    }
    rm(SS, y.k)
    y.calc <- unlist(y.calc)
    t2 <- Sys.time()
    t2 - t1
    # generate a stochastic part of level y
    y.stoch <- rnorm(length(y.calc), mean = y.stoch.mean, sd = y.stoch.sd)
    
    # assign final to DF
    DF$y <- y.calc + y.stoch.scale * y.stoch  # assign reconstructed
    
    # save current iteration y for later plotting
    all.iterated.y <- cbind(all.iterated.y, DF$y)
    
    # optional stats: check if not growing more than 10x initial value
    # OPTIONAL TODO: this should also be done on a regional basis
#     DFy.check <- DF$y
#     DFy.check[is.na(DFy.check)] <- 0
#     if(any(abs(DFy.check) > (DF$y[1] * 10))){
#       N.exp.growth <- N.exp.growth + 1
#     } else {
#       N.stable <- N.stable + 1
#     }
#     # check if not shrinking more than 10x initial value
#     if(DF$y[nrow(DF)] <= DF$y[1]/10) N.exp.decay <- N.exp.decay + 1
#     rm(DFy.check)
    
    # if enabled, plot y,x1,...,xn for this test iteration
    if(plot.iterations){
      ggDF <- ggplot(melt(DF, id.vars = c("i", "t")))
      plot(ggDF +
             geom_line(aes(x = t, y = value, color = variable)) + 
             facet_wrap(facets = "i"))
    }
    
    ### NEXT TODO:
    ### need to finish input for the CoefTest
    ### run pLogL on results
    ### check that Likelihood ratio test is approx Chi-squared distributed

    
    
    input$reg.in <- DF
    fitted.residuals <- rnorm(nrow(DF))
    input$reg.out <- list(
          details = list(
            gm.pmg = list(
              fitted.values = DF$y + fitted.residuals , 
                # fitted = DF$y + fitted.residuals
              residuals = fitted.residuals,
              ind.coef = coef.test,
              coefficients = rowSums(coef.test)/ncol(coef.test))
            ))  
    input.r <- input
    input.r$reg.out$details$gm.pmg$ind.coef <- coef.r.test
    input.r$reg.out$details$gm.pmg$coefficients <- rowSums(coef.r.test)/ncol(coef.test)
    
    LL.ur <- pLogL(input, testing.environment = TRUE)
    LL.r <- pLogL(input.r, testing.environment = TRUE)
    LL.ratio <-  -2*LL.r +2*LL.ur
    LL.ratio.all <- cbind(LL.ratio.all, LL.ratio)
    
    cat(iteration, " of ", N.test, " done | LL.ratio = ", LL.ratio, "\n", sep = "")
  }
  LL.ratio.all
  t.end <- Sys.time()
  # end profiling
  Rprof(NULL) # print summary at end of code
#### end test simulations ###
####

# Test iteration summaries -----------------------------------------------------
t.tot <- t.end-t.start

# dispaly some stats
cat("##### TESTING DONE",
    "\n Iterations = ", N.test, ", Duration = ", t.tot,
    "\n # exp. growth Y series = ", N.exp.growth,
    "\n # stable Y series= ", N.stable, 
    "\n # exponential decrease Y series = ", N.exp.decay ,
    "\n####\n")
if( N.exp.growth != 0 )
  cat("WARNING: plotting of generated Y likely to fail because\n",
      "some have exponential growth to infinity\n",
      " -> TRY: adjusting parameter \"coef.scale\" and \"coef.y.scale\"\n")

# vizualise example of generated data, from last test iteration
ggDF <- ggplot(melt(DF, id.vars = c("i", "t")))
plot(ggDF +
       geom_line(aes(x = t, y = value, color = variable)) + 
       facet_wrap(facets = "i") +
       ggtitle(paste("Time Series for each panel in last (", iteration,"th) iteration")))


# vizualise all generated y
if(plot.y){
  plot.ys <- as.data.frame(all.iterated.y)
  colnames(plot.ys) <- paste("k.",1:ncol(plot.ys), sep = "")
  plot.ys <- melt(plot.ys)
  plot.ys <- 
    cbind(series = rep(1:.I*N.test,each = .T) ,t = 1:.T,plot.ys)
  plot(ggplot(plot.ys) +
         geom_point(aes(x = t , y = value, color = series), alpha = 1/2) +
         ggtitle(paste("Plot of all generated Y-series\n Total #series:",
                       .I*N.test, sep="")))
         #facet_wrap(facets = "variable")
}

# qqplot of LogLikelihood ratio
qqplot(qchisq(ppoints(1000), df = chisqdf), LL.ratio.all,
       main = expression("Q-Q plot for" ~~ {chi^2}[nu == chisqdf]))
qqline(LL.ratio.all, distribution = function(p) qchisq(p, df = chisqdf),
       prob = c(0.25, 0.75), col = 2)
mtext("qqline(*, dist = qchisq(., df=5), prob = c(0.25, 0.75))")

# histogram of LogLikelihood ratio
histogram <- data.frame(LL.ratio.all = as.numeric(LL.ratio.all), 
                        rchisq = rchisq(10000, df = 5))
plot(ggplot(histogram) + 
  geom_histogram(aes(LL.ratio.all, fill = "red", alpha = 0.2)) + 
  geom_histogram(aes(rchisq, fill = "blue", alpha = 0.2)) + 
  ggtitle("Histogram of LL.ratio.all vs a chi-square distributed random sample\n mu = 5"))

cat("To view code profiling exec \"summaryRprof(\"profile_pLogL_test.out\")\"")
