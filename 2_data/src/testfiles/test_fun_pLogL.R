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

set.seed(21)  # set seed for RNG if needed for reproducible results 

#### SCRIPT START ##############################################################
require(plm)
require(ggplot2)
require(reshape2)

WD <- "D:/GoogleDrive/Work/MSc_Thesis/2_data"
setwd(WD)
source(file.path("src", "fun", "fun_pLogL.R"))
source(file.path("src", "fun", "fun_testcoeffhomogen.R"))
source(file.path("src", "fun", "fun_genformula.R"))

# DEFINE PARAMETERS ------------------------------------------------------------
# model parameters
  factors.test <- c("x1", "x2", "x3", "x4")# names of independent variables  
  factors.test <- c("x1", "x2", "x3")# names of independent variables  
  .I <- 4  #  regions/groups in test [min = 2]
  .T <- 100  # time periods per region in test, >= .P+2
  .P <- 5  # no. of lags of dependent variable (in levels) 
  .Q <- 5  # no. of lags of independent variable (in levels) 
# test parameters
  N.test <- 100 # number of iterations to run [min = 1]
  coef.sd <- 1  # sd used for generating random coefficients
  coef.mean <- 0  # mean used for generating random coefficients
  coef.scale <- 0.1 # coef scaling factor, for coef.scale*rnorm()
    # ~0.1 seems to be robust for reconstrucing Y based on ~100 iterations
  coef.y.scale <- 0.01 # coef for lag(log(y)) term, multiplicative w/ coef.scale
    # if = 1, seemingly always causes exponential decrease of Y to 0, if 0.1, roughly random walk
  y.stoch.mean <- 0  # mean used for generating stoch. part of y
  y.stoch.sd <- 1  # sd used for generating stoch part of y
  y.stoch.scale <- 1  # y = f(y,x1,... ,xn) + y.stoch.scale*rnorm()
  plot.y <- TRUE  # plot overview of al generated Y plots?
  plot.iterations <- FALSE # review each iterations' plots? mostly for debug

# additional debug parameters 
# WORK IN PROGRESS , currently not 100% bug free #
  add.y.lin.trend <- FALSE  # add a linear trend on generated y and x1?
    # idea: if TRUE, then x1 should explain majority of y, therefore restricting
    # the model to have coef_x1 = 0 should yields significant difference in 
    # explanatory power
## end parameters ---


#### TEST START ################################################################
Rprof("profile_pLogL_test.out") # profile code below for debugging

# derive some more parameters
N.Indepvar.test <- length(factors.test)  # no. of independent variables
N.regression.terms <-   # number of regression terms:
  1+1+N.Indepvar.test+.P-1+(.Q)*N.Indepvar.test
  # diff(y)~const +lag y +lag(xn) +(p-1)*lag(diff(y)) +(q-1)*lag(diff(xn))
Logs.test <- c(rep(TRUE, N.Indepvar.test))  # use logs for all y, x1, ..., xn

# Initialize variables
N.exp.growth <- 0
N.stable <- 0
N.exp.decay <- 0
LL.ratio.all <- vector(mode = "numeric")  # Log likelihood ratio of all test iterations
estimated.ind.coef <- list()  # lists for estimated coefficients
estimated.ind.coef.errors <- list()  # list for errors of estimated coefficients
if(plot.y) 
  plot.all.estimated.y <- vector(mode = "list")
# anov.all <- vector(mode = "list")

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

  # unrestricted model
    coef.ur.test <- data.frame(matrix(NA, ncol = .I, nrow = N.regression.terms))
    for(i in 1:.I){
      coef.ur.test[,i]<- 
        coef.scale * rnorm(N.regression.terms, sd = coef.sd, mean = coef.mean)
    }
    colnames(coef.ur.test) <- paste("i.", 1:.I, sep = "")
    rownames(coef.ur.test) <- c(
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
      coef.ur.test["lag(log(y))",] <- coef.y.scale * coef.ur.test["lag(log(y))",]
      # alternate way of sampling coeffs for lag(log(y)) term
      #coef.ur.test["lag(log(y))",] <- sample(seq(- 0.001, 0.001, by = 0.0001), .I)
  
  # restricted model
    coef.r.test <- coef.ur.test
    if(add.y.lin.trend){ # if add.y.lin.trend = TRUE (to test for obvious case)
      coef.r.test[c(1,3),] <- 0 # set only x1 to 0
    } else {
      coef.r.test[c(1,2+(1:N.Indepvar.test)),] <- 0   # set all lag(x1...xn) = 0
    }
    chisqdf <- sum(coef.r.test == 0)/.I # set equal to number of 0 parameters

# SIMULATE INPUT/OUTPUT OF TestcoeffHomogen ------------------------------------
  for(iteration in 1:N.test){
    # y.calc <- numeric() # storage for calculated y series from x1...xn
    y.calc <- vector(mode = "list", length = N.test)
    # create .P number of initial values for y in levels, 100 + random
 
    tmp.y <- vector(mode = 'list', length = .I)
    for(i in 1:.I){
      tmp.y[[i]] <- c(100+cumsum(rnorm(.P)),rep(NA,.T-.P))
    }
    DF$y <- unlist(tmp.y)
# old implementetation: with dynamic growth (memory intense)
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

    if(add.y.lin.trend) # debug: add linear trend to x1 (and later y) if desired
      DF[factors.test[1]] <- 100+ cumsum(rnorm(.T)+1)

    # old implementetation: with dynamic growth (memory intense)
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
      colnames(model)[ncol(model)] <- rownames(coef.ur.test)[2+i]
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
      y.calc[[j]] <- c(SS.y, SS.y.last)
      
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
    
    y.lin <- 0
    if(add.y.lin.trend){
      y.lin <- rep(1:.T, times = .I)
    }
    # assign final to DF
    DF$y <- y.calc + y.stoch.scale * y.stoch + y.lin  # assign reconstructed
    
    # save current iteration y for later plotting
    plot.all.estimated.y[[iteration]] <- DF$y
    
    # if enabled, plot y,x1,...,xn for this test iteration
    if(plot.iterations){
      ggDF <- ggplot(melt(DF, id.vars = c("i", "t")))
      plot(ggDF +
             geom_line(aes(x = t, y = value, color = variable)) + 
             facet_wrap(facets = "i"))
    }
    
    input$reg.in <- DF
    fitted.residuals <- rnorm(nrow(DF))
    input$reg.out <- list(
          details = list(
            gm.pmg = list(
              fitted.values = DF$y + fitted.residuals , 
                # fitted = DF$y + fitted.residuals
              residuals = fitted.residuals,
              ind.coef = coef.ur.test,
              coefficients = rowSums(coef.ur.test)/ncol(coef.ur.test))
            ))  
    input.r <- input
    input.r$reg.out$details$gm.pmg$ind.coef <- coef.r.test
    input.r$reg.out$details$gm.pmg$coefficients <- rowSums(coef.r.test)/ncol(coef.ur.test)
    
    LL.ur <- pLogL(input, testing.environment = TRUE)
    LL.r <- pLogL(input.r, testing.environment = TRUE)
    LL.ratio <-  -2*LL.r +2*LL.ur
    LL.ratio.all[[iteration]] <- LL.ratio
    
    # NOW TO COMPARE _generated_ vs _estiamted_ coeffs:
    # estiamte coeffs with implemented TestCoeffHomogen(),
     suppressWarnings( # supress likely warning that "my.formula" already in use
       tmp.estimation <- TestCoeffHomogen(DF, p = .P, q = .Q, logs = Logs.test))
     tmp.estimation.indcoef <- tmp.estimation$details$gm.pmg$indcoef
     estimated.ind.coef[[iteration]] <- tmp.estimation.indcoef  # save estimated
     estimated.ind.coef.errors[[iteration]] <- tmp.estimation.indcoef - coef.r.test
      # save errors for later
 
    cat(iteration, " of ", N.test, " done | LL.ratio = ", LL.ratio, "\n", sep = "")
  }
#### end test simulation loop ###
  # unlist all necessary lists:
    estimated.ind.coef <- as.data.frame(estimated.ind.coef)
    estimated.ind.coef.errors <- as.data.frame(estimated.ind.coef.errors)
      #adjust colnames
      tmp.col.names <- paste(rep(paste("N",1:N.test,sep =""), each = .I),
                             rep(paste("I",1:.I, sep=""), times = N.test), 
                             sep = ".")
      colnames(estimated.ind.coef) <- tmp.col.names
      colnames(estimated.ind.coef.errors) <- tmp.col.names
      rm(tmp.col.names)
    plot.all.estimated.y <- data.frame(matrix(unlist(plot.all.estimated.y), nrow = .T*.I))
      colnames(plot.all.estimated.y) <- paste("N",1:N.test, sep = "")
  t.end <- Sys.time()
  # end profiling
  Rprof(NULL)

# Summary statistics -----------------------------------------------------------
t.tot <- t.end-t.start # approximate time used
cat("##### TESTING DONE\n")
if(add.y.lin.trend)
  cat(" # Debug: Added linear trend to x and y")
cat(" Iterations = ", N.test, ", Duration = ", t.tot, "\n",
    " # Total no. of generated Y series = ", N.test*.I, "\n",
    " # exp. growth Y series = ", N.exp.growth, "\n",
    " # stable Y series= ", N.stable,  "\n",
    " # exponential decrease Y series = ", N.exp.decay , "\n",
    "####\n")
if( N.exp.growth != 0 )
  cat("WARNING: plotting of generated Y likely to fail because\n",
      "some have exponential growth to infinity\n",
      " -> TRY: adjusting parameter \"coef.scale\" and \"coef.y.scale\"\n")
rm(t1, t2, t.tot)
# PLOT RESULTS -----------------------------------------------------------------
cat("Plotting to file...\n")
FP <- paste("test_fun_pLogL_",format(Sys.time(),"%Y%m%d_%H%M%S"),".pdf",
                  sep = "") 
pdf(file = FP)

# Plot example of one iteration data
ggDF <- ggplot(melt(DF, id.vars = c("i", "t")))
plot(ggDF +
       geom_line(aes(x = t, y = value, color = variable)) + 
       facet_wrap(facets = "i") +
       ggtitle(paste("Example of Data generated during one iteration cycle\n\n",
                     "Plotting: variables = all, regions = all,",
                     "Iteration = ", iteration,"th iteration (last)\n",
                      sep=""))
     )


# Plot all generated y
if(plot.y){
  plot.ys <- melt(plot.all.estimated.y)
  plot.ys <- data.frame(iteration = plot.ys$variable,
                        i = rep(1:.I,each = .T, times = N.test),
                        t = 1:.T,
                        value = plot.ys$value)
  plot(ggplot(plot.ys) +
         geom_point(aes(x = t , y = value, color = iteration), alpha = 1/3) +
         facet_wrap(facets = "i") +
         ggtitle(paste("Plot for generated variable = Y,\n",
                       "regions = all, iteration = all \n", sep=""))
      )
}

# qqplot of LogLikelihood ratio
qqplot(qchisq(ppoints(1000), df = chisqdf), LL.ratio.all,
       main = expression("Q-Q plot for" ~~ {chi^2}[nu == chisqdf]))
qqline(LL.ratio.all, distribution = function(p) qchisq(p, df = chisqdf),
       prob = c(0.25, 0.75), col = 2)
mtext("qqline(*, dist = qchisq(., df=5), prob = c(0.25, 0.75))")

# histogram of LogLikelihood ratio vs chi squared distribution
hist.gg <- data.frame(LL.ratio.all = as.numeric(LL.ratio.all), 
                        rchisq = rchisq(10000, df = 5))
plot(ggplot(hist.gg) + 
       geom_histogram(aes(LL.ratio.all, color = "LL.ratio.all"), binwidth = 1,
                      fill="red", alpha = 0.2) + 
       geom_histogram(aes(rchisq, color = "qchisq"), binwidth = 1,
                      fill = "blue", alpha = 0.2) + 
       labs(title = paste("Histogram of LL.ratio.all vs chi-square distrib.",
            "random sample\n mu = 5"),
            y = "count",x = "value")
     )

# Visualize estimated coefficients errors
#   coefficients are stored in: estimated.ind.coef
#   errors stored in: estimated.ind.coef.errors
  plot.coef.error.df <- melt(t(estimated.ind.coef.errors))
  colnames(plot.coef.error.df) <- c("iter.region", "regterm", "value")
  tmp.indx <- colsplit(string = as.character(plot.coef.error.df$iter.region), 
                  pattern = ".I", 
                  names = c("iteration", "region"))
  plot.coef.error.df <- cbind(tmp.indx, plot.coef.error.df)
  rm(tmp.indx)
  hist.width <- 0.1
  plot.coef.error.gg <- ggplot(plot.coef.error.df)
  plot(plot.coef.error.gg +
         geom_boxplot(aes(x = regterm, y = value)) + 
         geom_hline(aes(yintercept = 0), color = "red") +
         geom_point(aes(x = regterm, y = value, color = iteration), alpha=0.2) +
         facet_wrap(facet = "region") + 
         labs(title = paste("Boxplot of coefficient estimation errors\n", 
                            "for each regression term\n",
                            "colored by iteration")) + 
         theme(axis.text.x = element_text(angle = 90, hjust = 1), 
                 plot.margin = unit(c(0,0,0,0),"mm"))
      )
  #plot without estimation errors for "intercept"
  plot.coef.error2.gg <- ggplot(plot.coef.error.df[plot.coef.error.df$regterm != "intercept",])
  plot(plot.coef.error2.gg +
         geom_boxplot(aes(x = regterm, y = value)) + 
         geom_point(aes(x = regterm, y = value, color = iteration), alpha=0.2) +
         facet_wrap(facet = "region") + 
         labs(title = paste("Boxplot of coefficient estimation errors\n", 
                            "for each regression term\n",
                            "colored by iteration\n[no intercept]")) + 
         theme(axis.text.x = element_text(angle = 90, hjust = 1), 
               plot.margin = unit(c(0,0,0,0),"mm"))
      )
  plot(plot.coef.error2.gg + 
         geom_histogram(aes(value), binwidth = hist.width, alpha = 0.75) +
         geom_vline(aes(xintercept = 0), color = "red") +
         facet_wrap(facets = "regterm") + 
         ggtitle(paste("Coefficient estimation error histograms\n",
                       "by regression term\n Width = ", hist.width))
       )
  # plot actual coefficients used to generate data
  plot.coef.r <- ggplot(melt(t(coef.r.test)))
  plot(plot.coef.r + 
         geom_point(aes(x = Var2, y = value), color = "red") +
         facet_wrap(facets = "Var1") +
         labs(title = paste("Actual coefficients used for test")) + 
         theme(axis.text.x = element_text(angle = 90, hjust = 1), 
               plot.margin = unit(c(0,0,0,0),"mm"))
       )  
dev.off()
# END PLOTTING
cat("Done plotting.\n Output in", file.path(getwd(),FP), "\n")
rm(FP)


cat("To view code profiling use \"summaryRprof(\"profile_pLogL_test.out\")\"")