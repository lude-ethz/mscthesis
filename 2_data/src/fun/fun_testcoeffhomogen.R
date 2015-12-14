# test for coefficient homogeneity ---------------------------------------------
# compare coefficients estimated using following methodologies:
# 1/ DFE - dynamic fixed effects estimator
# 2/ MG - mean group estimator
# 3/ <PENDING> PMG - pooled mean group estimator

# dependent on: #
# Gen_formula()...formula to generate model specific formula "fun_genformula.R"

# input: #
# df...data frame with model in panel data format

# output: #
# result...list with results

#############################################
# TODO: make compatible with melted data!
#############################################
# OPTIONAL TODO: check why pvcm() doesn't seem to work
#   err.msg.: names attribute [15] must b the same length as the vector [13]
#############################################


# load dependencies ------------------------------------------------------------
  require(plm)
  require(dynlm)


TestCoeffHomogen <- function (df, p, q, logs){

  if(missing(p)){
    cat("# TestCoeffHomogen():\n#  arg q missing/misspecified, defaulting to 2\n")
    p <- 2
  } else if(p%%1 != 0){
    warning("# TestCoeffHomogen():\n#  arg \"p\" not integer,
            dropping decimals \n")
    p <- p%%1
  }
  
  if(missing(q)){
    cat("# TestCoeffHomogen():\n#  arg q missing/misspecified, 
        defaulting to 2\n")
    q <- 2
  }else if(q%%1 != 0){
    warning("# TestCoeffHomogen():\n#  arg \"q\" not integer, 
            dropping decimals \n")
    q <- q%%1
  }
  
  if(missing(logs)||!is.logical(logs)||length(logs)!= (ncol(df) - 2)){
    warning("# TestCoeffHomogen():\n#  arg \"logs\" missing/misspecified, 
        defaulting to FALSE for all variables\n")
    logs <- rep(FALSE, ncol(df) - 2)
  }
  
  N.row <- nrow(df) # no. observations
  N.col <- ncol(df) # no. variables, independent + depenedent 

# extract list of variables used -------------------------------------------------------
  vars.orig <- as.list(colnames(df)[3:ncol(df)])
  N.indepvars <- N.col - 3
  colnames(df) <- c("i", "t", "y", paste(rep("x", N.indepvars), 1:N.indepvars, sep = ""))
  vars <- as.list(colnames(df)[3:ncol(df)])
# Generate forumla:
  # need my.formula to be defined outside of this Functions scope in order for
  # pmg to correctly generate and display a result.
  # check that my.formula is not already existend in the .GlobalEnv, if so back 
  # it up.
  if(exists("my.formula", envir = .GlobalEnv)){
    warning("# TestCoeffHomogen():\n",
            "# variable name \"my.formula\" already in use.\n",
            "# Storing original value in my.formula.old\n")
    my.formula.old <<- my.formula
  }
  my.formula <<- GenFormula(vars, p = p, q = q, logs = logs , form = "plm")

  # This formula specifies coefficients on lagged (log-)level dependent as 
  # >>alpha*beta<<, with alpha & beta defined as in Anundsen 2014, equation (6).
  # ATTENTION: to obtain coefficients as in anundsen, below code backs out
  # coefficients by simply dividing alpha*beta by alpha. How does this affect
  # significance and t-statistics of said estimations?
  
## test form of anundsen 2014 (6) ----------------------------------------------
  # 1/ DFE estimate ---------------------------------------------------------------
    dfe.result <- plm(formula(my.formula), 
                      data = df, 
                      index = c("i","t"), 
                      model = "within")
    dfe.coeff.raw <-  c(NA, dfe.result$coefficients)
    # the c(NA, ... ) is necessary bc plm does not estimate overall intercept
    # back out anundsen 2014 (6) coefficients: 
    dfe.result$coefficients.anundsen <- dfe.coeff.raw
    dfe.result$coefficients.anundsen[3:(2+N.indepvars)] <-
      dfe.coeff.raw[3:(2+N.indepvars)] / dfe.coeff.raw[2]
      # TODO: this backing out of coefficients: possibly requires to divide by (-
      # alpha) instead of just by alpha!!! double check formula in anundsen, it
      # has a (-beta) in the parentheses of (6)
      
    # <obsolete>: test alternative estimators for plm (pooled, random effects)
      # dfe_result_pool <- plm(formula(my.formula), data = df, index = c("i","t"), model = "pooling")
      # dfe_coeffs_pool <- data.frame(coeffs.DFE.pooled = dfe_result_pool$coefficients)
      # dfe_result_re <- plm(formula(my.formula), data = df, index = c("i","t"), model = "random")
      # dfe_coeffs_re <- data.frame(coeffs.DFE2 = c(NA, dfe_result_pool$coefficients))  
    
    
  # Group Mean (GM) Estimate  --------------------------------------------------
    # can use 1/ pvcm(), 2/ pmg(), 3/ custom GM_estimate()

# OPTIONAL TODO: check why pvcm() gives length error
    # 1/ pvcm() #
    # "panel variable coefficient model", this estimates a model per region
      gm.pvcm <- pvcm(formula(my.formula), data = df, model = "within")
    
      # back out alpha, beta representation _from_each_region_ according to
      # anundsen 2014 equation (6)
        gm.pvcm$coefficients.anundsen <- gm.pvcm$coefficients
        gm.pvcm$coefficients.anundsen[,3:(2+N.indepvars)] <-   # beta'_j = 
          gm.pvcm$coefficients[,3:(2+N.indepvars)]/   # beta_estimated/
          gm.pvcm$coefficients[,2]                    # alpha
        
      # calculate group mean for alpha & beta
        gm.pvcm$coeff.gme <- colMeans(gm.pvcm$coefficients.anundsen)
        gm.coeff.pvcm <- gm.pvcm$coeff.gme
      
     
    # 2/ pmg( ... ,model = "mg", ... ) /// HERE pmg = panel mean group, DO NOT
    # CONFUSE WITH POOLED MEAN GROUP ESTIMATOR under next point 3/ 
    # remember we need to _first_ back out alpha, beta according to anundsen 2014
    # eq (6) _for.each.region.individually_, and then calculate the group mean.
    # The regular result of pmg has coefficients for (log-)level independent
    # variables of the form alpha*beta and then meaned (not equal beta meaned,
    # which is what we want)
    my.formula <- GenFormula(vars, p = p, q = q, logs = logs , form = "plm")
      # calculate result, (we disregard gm.pmg$coefficients):
        gm.pmg <- pmg(formula(my.formula), data = df, model = "mg")
      # back out correct betas for each region individually from $indcoef:
        gm.pmg$indcoef.anundsen <- t(gm.pmg$indcoef)
        gm.pmg$indcoef.anundsen[,3:(2+N.indepvars)] <-   # beta'_j = 
          t(gm.pmg$indcoef)[,3:(2+N.indepvars)]/   # beta_estimated /
          t(gm.pmg$indcoef)[,2]# alpha
      # finally calculate mean
        gm.pmg$coefficients.anundsen <- colMeans(gm.pmg$indcoef.anundsen) 
      
    # 3/ <obsolete - Replaced by pvcm()> use custome GM_estimate() 
      # gm_gm_estimate <- GM_estimate(df, p = p, q = q, debug = TRUE)
      # gm_coeffs <- gm_gm_estimate$coeff.gme
 
       
  # Pooled Group Mean (PGM) Estimator ------------------------------------------
    # TODO: implement pgm estimator 
    # description: PGM estimator uses maximum likelihood estimation to find coefficients in
    # Anndsen2014 (6).
    # Originally based on Pesaran 1999. Originally, assumptions:
    #  Assumtion 1 (A1): error terms are iid, ~N(0, sigma^2)
  
    # from A1: we can run mle (use mle2()) on the error term. (?)
        pgm.coeff <- {}

    # test for poolability using pooltest()
    # (pooltest internally is a F test, comparison vs Anundsen Likelihood test?)
     #plm.pooltest <- pooltest(dfe.result, gm.pvcm)
     # TODO: proper interpretation of pooltest?
    
    # assemble result
     result <- list()
     result$coeff.legend <- matrix(data = vars.orig, 
                                   dimnames = list(as.character(vars),"vars"))
     result$formula <- my.formula
     result$input.regions<- unique(df$region)
     result$input.time <- unique(df$time)
     result$coeff.overview <- data.frame(gm.coeff = gm.pmg$coefficients.anundsen,
                                         dfe.coeff = dfe.result$coefficients.anundsen)
     result$details$dfe.result <- dfe.result
     result$details$gm.pmg <- gm.pmg
     result$details$gm.pvcm <- gm.pvcm
     
  return(result)
}
# EOF ---