# fun_national_model2.R

# preconfig --------------------------------------------------------------------
WD <- "D:/GoogleDrive/Work/MSc_Thesis/2_data"
setwd(WD)

# load required ----------------------------------------------------------------
require(plm)
require(dynlm)
require(xts)
require(reshape2)
require(ggplot2)
require(scales)

cat("#### Loading dependent functions ####\n")
source(file.path("src", "fun", "fun_plotpanelpdf.R"))
source(file.path("src", "fun", "fun_genformula.R"))
source(file.path("src", "fun", "fun_panel_urtest.R"))
source(file.path("src", "fun", "fun_testcoeffhomogen.R"))
source(file.path("src", "fun", "fun_pLogL.R"))
source(file.path("src", "fun", "fun_reconstruct.R"))
source(file.path("src", "fun", "fun_plot_fitted.R"))
cat("#### Loading dependent functions -- DONE ####\n")

# function ALL FACTORS ---------------------------------------------------------
National.Model <- function(P = 5, Q = 5, indep.vars = c(TRUE,TRUE,TRUE,TRUE),
                           Logs = c(TRUE,TRUE,TRUE,TRUE,FALSE), 
                           Ur.confidence.interval = 10, 
                           load.data = TRUE, plot.input = FALSE, 
                           plot.output = TRUE, csv.output = FALSE,
                           print.ur.test = FALSE){
  #### Description 
  # run model with Panel data
  # house price = f(population, income, housing stock) given CPI
  #   y = house price (log)
  #   x1 = population (log)
  #   x2 = income (level)
  #   x3 = housing stock (log)
  ####
  
  #### MODEL PARAMETERS - according to anundsen 2014 paper: 
  # P... element of [1,infty]; P = 1+ number of lags of dep.var
  # Q... element of [1,infty]; P = 1+ number of lags of indep.var
  # Logs... use log forms for variables x1 to xN
  # Ur.confidence.interval... confidence interval for Augmented Dickey Fuller
  #       (ADF) unit root testing,{1%, 5%, 10%}
  ####
  
  if(!(load.data == FALSE)){
    cat("#### Loading data ####\n")
    source(file.path("src", "create_price_panel_hedonic.R")) #apt & house price
    source(file.path("src", "create_cpi_panel.R"))  # cpi 
    source(file.path("src", "create_income_panel.R"))  # income, needs cpi
    source(file.path("src", "create_pop_panel.R"))  # population BFS (ESPOP, STATPOP)
      # TODO: Population in years 1993-1998 are off
    source(file.path("src", "create_pop_zhr.R"))  # population ZHR statistikamt
    source(file.path("src", "create_hstock_panel.R"))  # hstock
    source(file.path("src", "create_rates_panel.R"))  # interest rates
      
      # Temp: select population data: take national from BFS, regional only from zurich
      assign(x = "pop.panel", 
             value = rbind(pop.panel.bfs.q[pop.panel.bfs.q$region == "Schweiz",], 
                     pop.panel.zhr.q),
             envir = .GlobalEnv)
      
    cat("#### Loading data -- DONE #####\n")
  }

  ### Create Dataset 
    DF.all <- rbind(price.hedo[price.hedo$variable == "price_hedo_avg",],
                    pop.panel,
                    inc.panel[inc.panel$variable %in% c("inc_mean_of_median", "inc_mean"),],
                    hstock.panel,
                    rates.panel)
      
    ## TEMP HOTFIXES
      # TODO: adjust root cause of below temp
        # filter wanted timeframe and regions: currently national and zurich distr.
        timeframe.to.keep <- seq(2001,2013, 0.25)
        regions.to.keep <- unique(inc.panel$region) 
        DF.all <- DF.all[DF.all$region %in% regions.to.keep ,]
        DF.all <- DF.all[DF.all$time %in% timeframe.to.keep,]
        
        # for national level income, rename mean of median as inc_mean!
        DF.all$variable[DF.all$variable == "inc_mean_of_median"] <- "inc_mean"
        
        # remove Andelfingen because hedonic index for apt from Ioannis does not
        # seem to have Bezirk Andelfingen, though index for houses deos seem to
        # have! -> check with Ioannis.
        DF.all <- DF.all[(DF.all$region != "Bezirk Andelfingen"),]  
        
        # TODO: HEDONIC PRICE REQUIRES MAJOR OVERHAUL. Ioannis data has different 
        # regions for houses and apartments -> currently not corrected for, probably
        # only using regions which both have. DOUBLE CHECK IN create_price_panel.R
    ##
        
    # create dataset with only desired independent variables
    #   region | time| y | desired indep.vars
      DF <- dcast(DF.all, region + time ~ ...)
      model.factors <- colnames(DF)[-(1:3)]
      DF <- DF[ ,c(TRUE,TRUE,TRUE, indep.vars)] 
      DF <- melt(DF, id.vars = c("region", "time"))
    # extract info on dataset DF:
      
      N.indepvars <- length(model.factors)
  ###

  ### Test data properties 
    has.unit.root <- panel.urtest(DF = DF, conf.ival = Ur.confidence.interval)
    # TODO :need KPSS test for stationairty?
    if(print.ur.test == TRUE){
      cat("has.unit.root")
      print(has.unit.root)  
    }
  ###
 
  ### Run coeff homogeneity test 
    DF <- dcast(DF, region + time ~ ... )
    my.coef <- TestCoeffHomogen(DF, p = P, q = Q, logs = Logs)
    # Save reg output to file if wanted:
    if(csv.output == TRUE){
      write.csv(my.coef$details$dfe.result$model, 
                file = file.path("output", "result_dfe_input.csv"))
      write.csv(my.coef$details$gm.pmg$model, 
                file = file.path("output", "result_gm_pmg_input.csv"))
      write.csv(my.coef$coeff.legend,
                file = file.path("output", "result_input_variable_legend.csv"))
    }
    
    if(plot.input == TRUE){  # Plot reg input if wanted:
      # base plot: one plot for each variable in each region
      # PlotPanelPDF(melt(DF, id.vars = c("region", "time")), mfrow = c(2,3),
                   # f = paste(format(Sys.time(), "%Y%m%d_%H%M%S"),"_PlotRegInput_baseplot",
                             # "_p", P, "_q", Q, "_idvar", N.indepvars,
                             # ".pdf", sep =""))
      # ggplot: one plot for each variable, all regions together
      gg.all <- ggplot(melt(DF, id.vars = c("region", "time")), aes(time, value, color = region))
      pdf(file = file.path("output", "plots",
                           paste(format(Sys.time(), "%Y%m%d_%H%M%S"),
                                 "_PlotRegInput_ggplot", "_p", P, "_q", Q, 
                                 "_idvar", N.indepvars, ".pdf", sep ="")))
        for(i in 1:length(unique(gg.all$data$variable))){
          plot(
            gg.all
            + geom_line(data=subset(gg.all$data,variable==unique(variable)[i]),
                        alpha = 0.5)  # alpha=transparency in [0,1]
            + labs(x = "Quarters" )
            + ggtitle(paste("Factor", i-1, unique(gg.all$data$variable)[i])))
        }
      dev.off()
    }

    if(!(plot.output == FALSE)){  # plot output if wanted:
      Plot.Fitted(reg.in = DF, reg.out = my.coef, P, Q)
    }
    
  ###

#   ### Make comparison between different coefficient models:
#     # extract results for long run coefficients from each model
#     # aka: extract, VAR = [ sigma | phi | theta_i ]
#       # DFE: sigma_i = sigma, phi_i = phi, theta_i = theta
#         var.dfe <- data.frame(region = unique(DF$region),
#                               sigma = var(my.coef$details$dfe.result$residuals),
#                               phi = my.coef$details$dfe.result$coefficients.anundsen[2],
#                               theta = matrix(data = my.coef$details$dfe.result$coefficients.anundsen[1:pvars + 2],
#                                             byrow = TRUE,
#                                             ncol = pvars))
#         # TODO: is sigma = var(all residuals of dfe) correct?
#         # TODO: is var here sigma or already sigma^2
#         
#       # MG: sigma_i, phi_i, theta_i
#         var.mg <- data.frame(region = unique(DF$region),
#                              sigma = NA,
#                              pih = NA,
#                              theta = NA)
#     # TODO:
#       # add log likelihood test
#     # TODO:
#       # add log likelihood ratio statistic
#       # fun_pLogL.R
#       # ll.dfe <- pLogL(VAR = var.dfe, DF = df)
#       # ll.mg <-
#   ###

  ### end / cleanup
    rm(timeframe.to.keep, regions.to.keep)  
    cat("#### Done estimating model #### \n")
    result <- list(reg.in = DF, 
                   reg.out = my.coef, 
                   args = list(P = P, 
                               Q = Q, 
                               model.factors = model.factors, 
                               Logs=Logs))
    return(result)
  ###
}                                                                                                                                                                                                                          