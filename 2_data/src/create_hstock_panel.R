# create_hstock_panel.R --------------------------------------------------------
# variable definition:
# <variable>.<type>.<region>.<frequency>
# variable: hstock
# type: raw/panel 
# region: national/canton
# frequency: annual/quarterly/monthly {a/q/m}

# Output: ----------------------------------------------------------------------
# hstock.panel ... dataframe
# output/plots/hstock_panel_interpol.pdf ... PDF plot

# NATIONAL ---------------------------------------------------------------------
  # read raw file, annual data with years 1990,2000,2009-2013
    hstock.r.national <- read.csv(file = file.path("raw","stock",
                                            "hstock_cantonal_1990_2013.csv"),
                                  header = TRUE,
                                  skip = 2)
    n.obs <- ncol(hstock.r.national) - 1
  
  # extract only national level data
    temp.panel <- data.frame(time = c(1990,2000, seq(2009,2013)),
                             value = as.vector(t(hstock.r.national
                                               [1, 2:ncol(hstock.r.national)])))

  # extrapolate quarterly data
    t.start <- min(temp.panel$time) + 1
    t.end <- max(temp.panel$time) + 1
    temp.time <- seq(t.start, t.end, 0.25)
    n.obs <- length(temp.time)
    hstock.r.national.spln <- spline(x = temp.panel$time + 1,
                                    # +1 to adapt to decimal date format of xout
                                    # 2000.00 = 1999q4
                                     y = temp.panel$value,
                                     xout = temp.time,
                                     method = "fmm")

  # add interpolated values to hstock.panel
    hstock.panel <- data.frame(region = rep("Schweiz", n.obs),
                               time = temp.time,
                               variable = rep("hstock", n.obs),
                               value = hstock.r.national.spln$y)

  # plot interpolation differences
    f <- file.path("output","plots","hstock_panel_interpol.pdf")
    pdf(f)
    parold <- par(mfrow=c(2,1))
    plot(x = hstock.r.national.spln$x,
         y = hstock.r.national.spln$y, 
         type ="o",
         main = "Interpolation of hstock in region \"Schweiz\"")
    lines(temp.panel$time + 1, temp.panel$value, type = "p" , col = "red")
    lines(temp.panel$time + 1, temp.panel$value, type = "h" , col = "red")
    # keep pdf() device open, continue plotting later
    
# ZURICH -----------------------------------------------------------------------
  # read raw file, annual data with years 1990 - 2013
    hstock.r.zhr <- read.csv(file.path("raw","stock","hstock_zhr_1990_2014.csv"))
    temp.panel <- data.frame(region = hstock.r.zhr$GEBIET_NAME,
                             time = hstock.r.zhr$INDIKATOR_JAHR,
                             variable = rep("hstock", nrow(hstock.r.zhr)),
                             value = hstock.r.zhr$INDIKATOR_VALUE)
  
  #interpolate quarterly values
    # seperate all variables (currently only one)
    temp.panel <- dcast(data = temp.panel, 
                            formula = region + time ~ ...) 
    # generate new quarterly dates
    t.start <- min(temp.panel$time) + 1
    t.end <- max(temp.panel$time) + 1
    temp.time <- sort(seq(t.start, t.end, 0.25))
    
    # interpolate for each region and each variable
    temp.panel.q <- data.frame()
    for(i in unique(temp.panel$region)){
      temp.subset <- subset(temp.panel, region == i)
      temp.current <- data.frame(region = rep(i, length(temp.time)),
                                 time = temp.time)
  
      temp.current$hstock <- spline(x = temp.subset$time + 1, 
                                    # +1 to match  output year end date format
                                      y = temp.subset$hstock,
                                      xout = temp.time,
                                      method="fmm")$y
      temp.panel.q <- rbind(temp.panel.q, temp.current)
    }
    temp.panel.q <- melt(data = temp.panel.q,
                             id.vars = c("region", "time"))
    # add Zhr data to main panel
    hstock.panel <- rbind(hstock.panel, temp.panel.q)
    
    # plot interpolation differences for zhr data
    # continuing to plot in previous pdf() device.
    for(i in unique(temp.panel$region)){
      panela <- subset(temp.panel, region == i)
      panela$time <- panela$time + 1
      panelq <- subset(temp.panel.q, region == i)
      
      plot(panelq$time, panelq$value, type = "o", 
           main = paste("Interpolation of hstock in region \"",i, "\""))
      lines(panela$time, panela$hstock,type = "p", col = "red")
      lines(panela$time, panela$hstock,type = "h", col = "red")
    }
    par(parold)
    dev.off()
    cat("Plotted to file: \n", file.path("<WD>",f),"\n")
    

# save data --------------------------------------------------------------------
  save(hstock.panel, file="raw/hstock_panel.RData")
  

# Plot hstock panel ------------------------------------------------------------
  PlotPanelPDF(hstock.panel, "hstock_panel.pdf", mfrow = c(2,1))

# end --------------------------------------------------------------------------
  rm(hstock.r.national, hstock.r.zhr, hstock.r.national.spln)
  rm(i, f, n.obs, panela, panelq, parold, t.end, t.start, temp.panel, temp.panel.q,
     temp.subset, temp.time, temp.current)
  