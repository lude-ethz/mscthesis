# test for Dynamic Fixed Effects (DFE) modeling using plm()

WD <- "D:/GoogleDrive/ETH/2015_FS/MSc_Thesis/2_data"

# load dependencies ------------------------------------------------------------
  setwd(WD)
  source("./src/fun/myfunctions.R")
  require(plm)
  require(dynlm)


# >>> TO BE ADAPTED - MODEL SPECIFIC DATA =======================================
  # load data
  data("Produc")
  data("Cigar")
  
  mydata <- Produc
  #mydata <- Cigar
  
  # model specifications
  N_row <- nrow(mydata) # no. observations
  N_col <- 5 # no. variables, independent + depenedent 
  p <- 1 # no. of lags for dependent variable
  q <- 1 # no. of lags for independent variable
  logs <- TRUE # use log variables? yes/no = true/false

# Create model dataset
#   store model variables as columns in dataframe df
#   y - dependent variable
#   x1-xk - independent variables
#   cy - change in variable y
#   y_1 - 1 period lag in variable y
#   cy_1 - 1 period lag in change in variable y
  
#   # for Production data
  df <- data.frame(matrix(NA, nrow = N_row, ncol = 0))
  df$i <- Produc$state
  df$t <- Produc$year
  df$y <- Produc$gsp
  df$x1 <- Produc$pcap 
  df$x2 <- Produc$pc
  df$x3 <- Produc$emp 
  df$x4 <- Produc$unemp 

  
  # for Cigar data
#   df <- data.frame(matrix(NA, nrow = N_row, ncol = 0))
#   df$i <- Cigar$state
#   df$t <- Cigar$year
#   df$y <- Cigar$price
#   df$x1 <- Cigar$pop 
#   df$x2 <- Cigar$pop16
#   df$x3 <- Cigar$ndi
#   df$x4 <- Cigar$sales

# >>> END OF MODEL SPECIFIC DATA

  
# List of variables used -------------------------------------------------------
  vars <- as.list(colnames(df)[3:ncol(df)])
  N_indepvars <- N_col - 1
# Generate forumla:
  anundsen_formula <- Gen_formula(vars, p = p, q = q, logs = logs , form = "plm")
  # This formula specifies coefficients on lagged (log-)level dependent as 
  # >>alpha*beta<<, with alpha & beta defined as in Anundsen 2014, equation (6).
  # ATTENTION: still need to include code to back out beta's as specified in 
  # Anundsen2014 eq (16)! 
  
## test form of anundsen 2014 (6) ----------------------------------------------
  
  # DFE estimate ---------------------------------------------------------------
    dfe_result <- plm(formula(anundsen_formula), 
                      data = df, 
                      index = c("i","t"), 
                      model = "within")
    dfe_coeff.raw <-  c(NA, dfe_result$coefficients)
    # the c(NA, ... ) is necessary bc plm does not estimate overall intercept
    # back out anundsen 2014 (6) coefficients: 
    dfe_coeff <- dfe_coeff.raw
    dfe_coeff[3:(2+N_indepvars)] <-
      dfe_coeff.raw[3:(2+N_indepvars)] / dfe_coeff.raw[2]                   
    
    # <obsolete>: test alternative estimators for plm (pooled, random effects)
      # dfe_result_pool <- plm(formula(anundsen_formula), data = df, index = c("i","t"), model = "pooling")
      # dfe_coeffs_pool <- data.frame(coeffs.DFE.pooled = dfe_result_pool$coefficients)
      # dfe_result_re <- plm(formula(anundsen_formula), data = df, index = c("i","t"), model = "random")
      # dfe_coeffs_re <- data.frame(coeffs.DFE2 = c(NA, dfe_result_pool$coefficients))  
    
    
  # Group Mean (GM) Estimate  --------------------------------------------------
    # can use 1/ pvcm(), 2/ pmg(), 3/ custom GM_estimate()


    # 1/ pvcm() #
    # "panel variable coefficient model", this estimates a model per region
      gm_pvcm <- pvcm(anundsen_formula, data = df, model = "within")
    
      # back out alpha, beta representation _from_each_region_ according to
      # anundsen 2014 equation (6)
        gm_pvcm$coefficients.anundsen <- gm_pvcm$coefficients
        gm_pvcm$coefficients.anundsen[,3:(2+N_indepvars)] <-   # beta'_j = 
          gm_pvcm$coefficients[,3:(2+N_indepvars)]/   # beta_estimated/
          gm_pvcm$coefficients[,2]                    # alpha
        
      # calculate group mean for alpha & beta
        gm_pvcm$coeff.gme <- colMeans(gm_pvcm$coefficients.anundsen)
        gm_coeff_pvcm <- gm_pvcm$coeff.gme
      
     
    # 2/ pmg( ... ,model = "mg", ... )
    # with 2/, the result group mean estimator is _incorrect_ if we specify the
    # formula explicitly, as is done in Gen_formula: we need to first back out
    # alpha, beta accoridng to anundsen 2014 eq (6) for each region
    # individually, and then calculate the group mean. The regular result of pmg
    # has coefficients for (log-)level independent variables of the form
    # alpha*beta and then meaned (not equal beta meaned, which is what we want)
      # calculate result, (we disregard gm_pmg$coefficients):
        gm_pmg <- pmg(anundsen_formula, data = df, model = "mg")
      # back out correct betas for each region individually from $indcoef:
        gm_pmg$indcoef.anundsen <- t(gm_pmg$indcoef) 
          # transpose for easier handling thanks to element wise column divison 
        gm_pmg$indcoef.anundsen[,3:(2+N_indepvars)] <-   # beta'_j = 
          t(gm_pmg$indcoef)[,3:(2+N_indepvars)]/   # beta_estimated /
          t(gm_pmg$indcoef)[,2]# alpha
      # finally calculate mean
        gm_pmg$coeff.gme <- colMeans(gm_pmg$indcoef.anundsen) 
        
      gm_coeff_pmg <- gm_pmg$coeff.gme
      
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
        pgm_coeff <- {}

  # Compare estimated coeffs side by side
    disp <- data.frame(dfe_coeff = dfe_coeff
                      ,gm_coeff_pvcm = gm_coeff_pvcm
                      ,gm_coeff_pmg = gm_coeff_pmg
                      #,pgm_coeff
                      ) 
    print(disp)  

    # test for poolability using pooltest()
    # (internally a F test, comparison vs Anundsen Likelihood test?)
     plm_pooltest <- pooltest(dfe_result, gm_pvcm)
     
    # TODO: proper interpretation of pooltest?
     
     
     
     
     
     
     
     
     
     
     
# Outdated ---------------------------------------------------------------------
# create model formula according to:
# cy_it  = mu_i + alpha_i*( y_i(t-1) - beta_i*x_i(t-1) ) 
#           + sum_(s=1)^(p-1){ gammay_i,s * cy_i(t-s)}
#           + sum_(s=0)^(q-1){ gammax_i,s * cx_i(t-s)}
#           + phi_i * D_t
#           + e_it

## dynformula to recreate example from ?plm
# plm_formula <- dynformula(formula = y ~ x1 + x2 + x3 + x4, 
#                           lag.form = FALSE,
#                           diff.form = FALSE,
#                           log.form = list(x4 = FALSE, TRUE))

# adjust dynformula to test whether diff(diff(x),1) when used in plm is same as "Dx_(t-1)"
# plm_formula <- dynformula(formula = y ~ x1 + x2 + x3 + x4, 
#                           lag.form = list(x1=TRUE, FALSE),
#                           diff.form = list(x1=TRUE, FALSE),
#                           log.form = list(x4 = FALSE, TRUE))

# estimate DFE model
# dfe_test <- plm( formula(plm_formula), data = df, index = c("i","t"))


# compare to example
# zz <- plm(log(gsp) ~ diff(log(pcap)) + lag(diff(log(pcap)), 1) + log(pc) + log(emp) + unemp,
#           data = Produc, index = c("state","year"))
# print(summary(zz))
# print(summary(dfe_test))


# check for diff/lag for panel data:
# with the below, can manually confirm that diff/lag has been rewritten in plm,
# function as desired: applies diff and lag for each region when used within plm(),
# zz <- plm(gsp ~ pcap + diff(pcap,1) + lag(pcap,1) + diff(diff(pcap),1) + lag(diff(pcap),1),
#           data = Produc, index = c("state","year"))
# zz$model #print panel data used
# comp <- subset(Produc, Produc$state == "ALABAMA" | Produc$state == "ARIZONA", 1:3)
# head(zz$model, 35)
# head(comp,35)


# manual formula specification
# plm_formula2 <- diff(y) ~ 1 +
#   (lag(y) - lag(x1, 1) - lag(x2, 1) - lag(x3, 1) - lag(x4, 1)) +
#   diff(x1) + diff(x1, 1) + diff(x1, 2) + diff(x1, 3) + diff(x1, 4) +
#   diff(x2) + diff(x2, 1) + diff(x2, 2) + diff(x2, 3) + diff(x2, 4) + 
#   diff(x3) + diff(x3, 1) + diff(x3, 2) + diff(x3, 3) + diff(x3, 4) + 
#   diff(x4) + diff(x4, 1) + diff(x4, 2) + diff(x4, 3) + diff(x4, 4)

# EOF ---