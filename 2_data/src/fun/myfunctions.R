## @knitr myfunctions

# D.L: custom functions for MSc thesis
# list of custom functions

# 
# require(xts)
# require(plm)
# require(dynlm)
# require(reshape2)

source("src/fun/fun_adjustxtswindow.R")
source("src/fun/fun_analyseXts.R")
source("src/fun/fun_egtest.R")
source("src/fun/fun_genformula.R")
source("src/fun/fun_plotpanelpdf.R")
source("src/fun/fun_plotxts.R")
# source("src/fun/fun_plotxts_20150718.R")
source("src/fun/fun_testcoeffhomogen.R")
source("src/fun/fun_xtsdates.R")




# # display start & end of (xts) object. -----------------------------------------
# xtsdates <- function(x){
#   if(is.xts(x)){
#     s <- start(x)
#     e <- end(x)
#     (s <- c(s,e))
#   } else {
#     stop("argument x not a xts object")
#   }
# }

# # adjust xts series to desired time window STARt-to-END -------------------------
# adjw <- function(x,START,END){
# 
#   if(is.xts(x)){
#     
#     if(START<start(x)||END>end(x)) 
#       warning("START/END date out of range of available time series data, kept 
#               max available time window for")
#     WNDW <-  paste(as.Date(START),"/",as.Date(END),sep="")
#     (x <- x[WNDW])
#     
#   } else if(is.environment(x)){
#         n <- ls(x); #list of elements in environment
#         for (i in 1:length(n)){
#           if(is.environment(x[[n[i]]]) || is.xts(x[[n[i]]]))
#             x[[n[i]]] <- adjw(x[[n[i]]],START,END)
#           else {
#             cat("skipped",as.character(ls(x)[i]),": not a xts or environment\n")
#           }
#         }
#         
#   } else {
#     stop("x has to be of type environment/xts")
#     }
# }




# GM_estimate() <REPLACED, by pvcm()> ------------------------------------------
# GM_estimate <- function(df, p = 5, q = 5, debug = FALSE){
#   # Group Mean Estimation Function using plm() & fixed effect/within transform
#   # input:
#   #   df...data frame with panel data in specific format:
#   #        colums are: region index|time index|dependent var|independent vars
#   # output:
#   #   result...list with two elements: coef, mean
#   #     result$coef...list of data frames, with estimated coefs from each region
#   #     result$mean...data frame with calculated means
#   # CAUTION:
#   #   STRONGLY DEPENDENT ON Gen_formula() formatting for backing out coefficients
#   #     for lagged level independent variables in Anundsen2014(6), "beta'"
#   
#   result <- list()
#   result$panels <- unique(df[,1]) # region identifier values
#   result$variables <- colnames(df)[3:ncol(df)]
#   result$formula <- as.formula(Gen_formula(as.list(result$variables), 
#                                            p = p,
#                                            q = q,
#                                            form = "dynlm"))
#   N_panels <- length(result$panels) # number of panels
#   N_indepvars <- length(result$variables)-1 # number of independent variables
#   
#   # TEMP FIX pt.1/2 : CREATING VARIABLE IN .GLOBALENV
#   # "impossiblylong..."...same as result$formula but is created in .GlobalEnv
#   #                  for dynlm() call before this function (GM_estimate) is called
#   # reason: dynlm() does not find result$formula for it's formula argument.
#   #         instead seems to look in the .GlobalEnv for its argument.
#   # TODO: find proper solution without modifying environment outside of function
#     assign(x = "formula_with_impossibly_long_name_to_avoid_confusion",
#            value = result$formula,
#            envir = .GlobalEnv)
#   # END TEMP FIX pt. 1/2 - pt. 2/2 after next for loop
#     
#   # run region specific linear regressions and save to result$regional$<name>
#   for(j in result$panels){
#     if(debug == TRUE){
#       cat("individual regression for panel j = " , j,"\n")
#     }
#     assign(x = "df_subset_with_impossibly_long_name_to_avoid_confusion",
#            value = subset(df, df[,1] == j),
#            envir = .GlobalEnv)
#     
#     result$regional[[j]] <- dynlm(formula = 
#         formula_with_impossibly_long_name_to_avoid_confusion, 
#                                   data = as.ts(df_subset_with_impossibly_long_name_to_avoid_confusion))
#     # back out beta coefficients according to the definition of Anundsen2014(6)
#     result$regional[[j]]$coefficients[3:(2+N_indepvars)] <-   # beta'_j = 
#       result$regional[[j]]$coefficients[3:(2+N_indepvars)]/   # beta_estimated /
#       result$regional[[j]]$coefficients[2]                    # alpha
#   }
#     
#   ## TEMP FIX pt.2/2   
#     rm(formula_with_impossibly_long_name_to_avoid_confusion,
#      envir = .GlobalEnv)
#     rm(df_subset_with_impossibly_long_name_to_avoid_confusion,
#        envir = .GlobalEnv)
#   ## END TEMP FIX pt2/2
#   
#   # aggregate all coefficients in one matrix
#     # TODO check why result is 0x0 matrix
#   for(j in result$panels){
#     if(debug == TRUE){
#       cat("coeff.all loop: j = ", j, "\n")
#     }
#     if (j == result$panels[1]){
#       result$coeff.all <- t(as.data.frame(gm_result$regional[[j]]$coefficients))
#       next
#     }
#     result$coeff.all <- rbind(result$coeff.all, 
#                                  t(as.data.frame(gm_result$regional[[j]]$coefficients)))
#   }
#     rownames(result$coeff.all) <- result$panels
#   
#   # For mean coefficients, sum all individual coefficients
#   for (j in result$panels){
#     if (j == result$panels[1]){
#       coeffsum <- as.data.frame(result$regional[[j]]$coefficients)
#       next
#     }
#     coeffsum <- coeffsum + as.data.frame(result$regional[[j]]$coefficients)
#   }
#   # for simple coeffs, divide by N_panels
#   result$coeff.gme <- coeffsum/N_panels
#   colnames(result$coeff.gme) <- "coeffs.gme"
#   
#   result
# }

source("src/fun/fun_egtest.R")

# config workspace -------------------------------------------------------------
# thesiswd <- "D:/GoogleDrive/ETH/2015_FS/MSc_Thesis/2_data"
# myconfig <- function(wd){
#   if(missing(wd)||!is.character(wd)) {
#     cat(paste("Argument wd not specified or not of type character,will continue with current work directory: ", getwd(),sep=""))
#     wd <- getwd();
#   }
#   if(file.exists(wd)) try(setwd(wd))
#   else {dir.create(wd); setwd(wd); cat(paste("created new directory",wd,sep=""))}
#   
#   if(file.exists(file.path(wd,"rsetup","pkg.csv"))) pkg  <-  read.csv(file.path(wd,"rsetup","pkg.csv"),header=T)
#   else {
#     cat("Did not find required packages, will configure for packages\n ggplot2\n xts")
#     file.create(file.path(wd,"rsetup","pkg.csv"))
#     write.csv(c("ggplot2","xts"),file=file.path(wd,"rsetup","pkg.csv"),row.names=F)
#   }
#   print(pkg);
#   scan();
#   
#   if (length(setd(pkg[,1],rownames(installed.packages())))) {
#     install.packages(setd(pkg[,1],rownames(installed.packages())))
#     cat(paste("installed packages:",setd(pkg[,1],rownames(installed.packages())),sep=""))
#   }
#   lapply(pkg,require)
# }


