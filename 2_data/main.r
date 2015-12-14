### WORK IN PROGRESS

# main.R------------------------------------------------------------------------
# MSc. Thesis
# Delong Lu, 
# ETH Zurich, D-MTEC, Chair of Entrepreneurial Risk
# HS 2015
# ------------------------------------------------------------------------------

# Main script: -----------------------------------------------------------------
# inteded to be run together to produce all resutls of my thesis:
# read descriptionfor folder setup

# Content: ---------------------------------------------------------------------
# - TODO
# - TODO

cat("###### main.R ######\n")

# Variable Definitions ---------------------------------------------------------
cat("------ clearing workspace...")
rm(list=ls())
cat("done\n")

WD <- "D:/GoogleDrive/ETH/2015_FS/MSc_Thesis/2_data"

# prep workspace ---------------------------------------------------------------
  cat("------ setting working directory to\n  ",WD,"\n")
  setwd(WD)

  # load packages
    cat("------ loading packages\n")
    require(xts) # for handling timeseries using xts()
    require(reshape2) # mainly for reshaping using cast()/melt()
    require(tseries) # maily for ADF unit root test adf()
    require(urca) # for ADF unit root test ur.df()
    require(plm) # for panel data estimation
    require(Formula) # for alt. formula generation, ("plm" has dynformula)
  # import some functions
    cat("------ myfunctions.R...running\n")
    source("./src/fun/myfunctions.R")
    cat("------ myfunctions.R...done\n")
  
  # create working environment from data
    cat("------ create_dataset.R...running\n")
    source("./src/create_dataset.R")
    cat("------ create_dataset.R...done\n")
    # alternatively to save time, if create_dataset.R has been exectued before
    # and output daata.rData has not been tampered with:
#     cat("------ loading data.RData...")
#     load("data.RData")
#     cat("done\n")

# run national model  ----------------------------------------------------------
  cat("------ national_model.R...running\n")
  source("./src/model_national.R")
  cat("------ national_model.R...done\n")
  
  
  
cat("###### main.R...done #######\n")