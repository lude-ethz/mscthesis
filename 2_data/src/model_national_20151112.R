# national_model2.R
# rm(list=ls())

# Description ------------------------------------------------------------------
# run model with Panel data
# house price = f(population, income, housing stock) given CPI
#   y = house price (log)
#   x1 = population (log)
#   x2 = income (level)
#   x3 = housing stock (log)

# MODEL PARAMETERS - according to anundsen 2014 paper: -------------------------
P <- 5  # element of [1,infty]; P = 1+ number of lags of dep.var
Q <- 5  # element of [1,infty]; P = 1+ number of lags of indep.var
Logs <- c(TRUE, TRUE, TRUE, TRUE, FALSE)  # use log forms for variables x1 to xN
Ur.confidence.interval <- 10
  # confidence interval for Augmented Dickey Fuller (ADF) unit root testing,{1%,
  # 5%, 10%}
####
  
# Config workspace -------------------------------------------------------------
WD <- "D:/GoogleDrive/Work/MSc_Thesis/2_data"
setwd(WD)

# load required
  require(plm)
  require(dynlm)
  require(xts)
  require(reshape2)

cat("#### Loading functions ####\n")
  source(file.path("src", "fun", "fun_plotpanelpdf.R"))
  source(file.path("src", "fun", "fun_genformula.R"))
  source(file.path("src", "fun", "fun_panel_urtest.R"))
  source(file.path("src", "fun", "fun_testcoeffhomogen.R"))
  source(file.path("src", "fun", "fun_pLogL.R"))
  source(file.path("src", "fun", "fun_reconstruct.R"))
  source(file.path("src", "fun", "fun_plot_fitted.R"))
  cat("#### Loading functions -- DONE ####\n")


  cat("#### Loading data ####\n")
  source(file.path("src", "create_price_panel_hedonic.R")) #apt & house price
  source(file.path("src", "create_cpi_panel.R"))  # cpi 
  source(file.path("src", "create_income_panel.R"))  # income, needs cpi
  source(file.path("src", "create_pop_panel.R"))  # population BFS (ESPOP, STATPOP)
    # TODO: Population in years 1993-1998 are off
  source(file.path("src", "create_pop_zhr.R"))  # population ZHR statistikamt
  source(file.path("src", "create_hstock_panel.R"))  # hstock
  source(file.path("src", "create_rates_panel.R"))  # interest rates
  cat("#### Loading data -- DONE #####\n")


# Temp: select population data: take national from BFS, regional only from zurich
  temp.pop <- rbind(pop.panel.bfs.q[pop.panel.bfs.q$region == "Schweiz",], 
                    pop.panel.zhr.q)
  
# Create dataset ---------------------------------------------------------------
    df <- rbind(price.hedo[price.hedo$variable == "price_hedo_avg",],
                #pop.panel.bfs.q, # some espop data corrupted 1993-1998
                temp.pop,
                inc.panel[inc.panel$variable %in% c("inc_mean_of_median", "inc_mean"),],
                hstock.panel,
                rates.panel
                )

#### TEMP HOTFIXES
  # TODO: adjust root cause of below temp
    # filter wanted timeframe and regions: currently national and zurich distr.
    timeframe.to.keep <- seq(2001,2013, 0.25)
    regions.to.keep <- unique(inc.panel$region) 
    df <- df[df$region %in% regions.to.keep ,]
    df <- df[df$time %in% timeframe.to.keep,]
    
    # for national level income, rename mean of median as inc_mean!
    df$variable[df$variable == "inc_mean_of_median"] <- "inc_mean"
    
    # remove Andelfingen because hedonic index for apt from Ioannis does not
    # seem to have Bezirk Andelfingen, though index for houses deos seem to
    # have! -> check with Ioannis.
    df <- df[(df$region != "Bezirk Andelfingen"),]  
    
    # TODO: HEDONIC PRICE REQUIRES MAJOR OVERHAUL. Ioannis data has different 
    # regions for houses and apartments -> currently not corrected for, probably
    # only using regions which both have. DOUBLE CHECK IN create_price_panel.R
####

# extract some dimensions of the resulting dataset df:
  N.indepvars <- length(unique(df$variable))-1

# Test data properties
    has.unit.root <- panel.urtest(DF = df, conf.ival = Ur.confidence.interval)
    cat("has.unit.root")
    print(has.unit.root)
    
    # some handling procedure for handling UR in data:
    
    # need KPSS test for stationairty?
    
# Run coeff homogeneity test ---------------------------------------------------
    df <- dcast(df, region + time ~ ... )
    my.coef <- TestCoeffHomogen(df, p = P, q = Q, logs = Logs)
      # q=p=2 <-> 1 lag, q=p=5 <-> anundsen from, 4 lags
    
    # save to file for manual exam:
    write.csv(my.coef$details$dfe.result$model, 
              file = file.path("output", "result_dfe_input.csv"))
    write.csv(my.coef$details$gm.pmg$model, 
              file = file.path("output", "result_gm_pmg_input.csv"))
    write.csv(my.coef$coeff.legend,
              file = file.path("output", "result_input_variable_legend.csv"))
    # for ref: use pdim <- check panel dimension 
    
    # Plot data used:
    PlotPanelPDF(melt(df, id.vars = c("region", "time")),
                 f = "nationalmodel2_data_used.pdf",
                 mfrow = c(2,3))
    
# extract results for long run coefficients from each model
# aka: extract, VAR = [ sigma | phi | theta_i ]
    # DFE: sigma_i = sigma, phi_i = phi, theta_i = theta
    var.dfe <- data.frame(region = unique(df$region),
                          sigma = var(my.coef$details$dfe.result$residuals),
                          phi = my.coef$details$dfe.result$coefficients.anundsen[2],
                          theta = matrix(data = my.coef$details$dfe.result$coefficients.anundsen[1:N.indepvars + 2],
                                        byrow = TRUE,
                                        ncol = N.indepvars))
      # TODO: is sigma = var(all residuals of dfe) correct?
      # TODO: is var here sigma or already sigma^2
    
    # MG: sigma_i, phi_i, theta_i
    var.mg <- data.frame(region = unique(df$region),
                         sigma = NA,
                         pih = NA,
                         theta = NA)
# TODO:
    # add log likelihood test
# TODO:
    # add log likelihood ratio statistic
    # fun_pLogL.R
#     ll.dfe <- pLogL(VAR = var.dfe, DF = df)
#     ll.mg <-

    
# visualizations
    Plot.Fitted(reg.in = df, reg.out = my.coef, P, Q)
    
rm(timeframe.to.keep, regions.to.keep)  
cat("#### Done estimating model #### \n")

# cat("\n>> Display result summary? [y/n]")
#   scan.in <- scan(what = character(), nmax = 1)
#   if(scan.in == "y")
#     print(my.coef[1:3])
#   rm(scan.in)
