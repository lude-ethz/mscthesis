require(xts)
require(reshape2)
require(ggplot2)
require(scales)

# Create income panel data -----------------------------------------------------
# variable definition:
# <variable>.<type>.<region>.<frequency>
# variable: income
# type: raw/panel 
# region: national/canton
# frequency: annual/quarterly/monthly {a/q/m}

# Input: -----------------------------------------------------------------------
  source(file.path("src", "create_cpi_panel.R")) 
    # CPI data for deflating to real income
  source(file.path("src", "fun", "fun_plotpanelpdf.R"))
    # function to plot panel dataset

# Output: ----------------------------------------------------------------------
# inc.panel

# Variable definitions
timeframe.to.keep <- seq(2000, 2013, 0.25)
  # timeframe for which we have income data for all Zurich bezirke and National

# NATIONAL aggregated annual median pretax income by type of employment --------
  # in yearly numbers
  inc.r.nat.a <- read.csv("raw/inc/inc.csv")
  inc.r.nat.a$Date <- as.Date(inc.r.nat.a$Date,  "%d/%m/%Y")
  inc.r.nat.a <- as.xts(inc.r.nat.a[, 2:ncol(inc.r.nat.a)], 
                        order.by = inc.r.nat.a$Date) 
  
  # interpolate quarterly data
    temp.qrtrs <- seq(as.Date("1992-01-01"), as.Date("2015-01-01"), "quarters")-1
    inc.r.nat.q <- xts(matrix(nrow = length(temp.qrtrs), 
                              ncol = ncol(inc.r.nat.a)), 
                       order.by = temp.qrtrs)
    colnames(inc.r.nat.q) <- colnames(inc.r.nat.a);
  
    #interpolate quarterly data for each type of employment
    for (i in 1:ncol(inc.r.nat.a)){
      inc.r.nat.q[, i] <- data.frame(spline(x = index(inc.r.nat.a),
                                            y = inc.r.nat.a[, i], 
                                            xout=temp.qrtrs))[, 2]
    }
    
  inc.r.nat.q$mean_of_median <- rowSums(inc.r.nat.q[,1:4])/4
  
  
  # create income panel data - format: region|time|variable|value
  # income panel: national data
  # >>> ORIGINAL DATA IS STARTS AND ENDS WITH YEAR END <<<
  # convert to quarterly representation in quarterly steps:
  # 2000.00 = 1999 q4 data / 2000.25 = 2000 q1 data / 2000.5 = 2000 q2 data etc.
    t.start <- as.numeric(format(min(index(inc.r.nat.q)),"%Y")) + 1
    t.end <- as.numeric(format(max(index(inc.r.nat.q)),"%Y")) + 1
    temp.time <- seq(t.start, t.end, 0.25)
    
    inc.panel <- data.frame(region = rep("Schweiz", nrow(inc.r.nat.q)),
                            time = temp.time,
                            variable = rep("inc_mean_of_median", 
                                           nrow(inc.r.nat.q)),
                            value = inc.r.nat.q$mean_of_median)
    colnames(inc.panel) <- c("region", "time", "variable", "value")
    rownames(inc.panel) <- 1:nrow(inc.panel)
    
    (inc.panel)

# ZURICH REGIONAL data: 1999-2012, annual, mean and median, ZHR districts ------
  temp.inc <- read.csv("raw/inc/inc_zhr_1999_2012.csv", header = TRUE)  
  temp.inc.panel <- data.frame(region = temp.inc$GEBIET_NAME,
                               time = temp.inc$INDIKATOR_JAHR,
                               variable = temp.inc$INDIKATOR_ID,
                               value = temp.inc$INDIKATOR_VALUE)
  # change variable identifiers to "median"/"mean"
  temp.inc.panel$variable[temp.inc.panel$variable == 316] <- "median"
  temp.inc.panel$variable[temp.inc.panel$variable == 317] <- "mean"
  
  # interpolate quarterly:
    # seperate mean and median ts columns
    temp.inc.panel <- dcast(data = temp.inc.panel, 
                            formula = region + time ~ ...) 
    # generate new quarterly dates
    t.start <- min(temp.inc.panel$time) + 1
    t.end <- max(temp.inc.panel$time) + 1
    temp.time <- seq(t.start, t.end, 0.25)
  # TODO: CAN ADAPT CODE TO USE TEMP.TIME AS INTERPOLATION AXIS
  # interpolate for each region and each variable
    temp.inc.panel.q <- data.frame()
    for(i in unique(temp.inc.panel$region)){
      temp.subset <- subset(temp.inc.panel, region == i)
      
      temp.current <- data.frame(region = rep(i, length(temp.subset$time)*4-3),
                            time = temp.time) 
      # here: *4-3 => *4 extend quarters each year, -3 first year start in q4
      
      temp.current$inc_mean <- spline(x = seq(1,length(temp.subset$time),1),
                             y = temp.subset$mean,
                             xout = seq(1,length(temp.subset$time), 0.25),
                             method="fmm")$y
      temp.current$inc_median <- spline(x = seq(1,length(temp.subset$time),1),
                               y = temp.subset$median,
                               xout = seq(1,length(temp.subset$time), 0.25),
                               method="fmm")$y
      temp.inc.panel.q <- rbind(temp.inc.panel.q, temp.current)
    }
    temp.inc.panel.q <- melt(data = temp.inc.panel.q,
                             id.vars = c("region", "time"))

# Merge ------------------------------------------------------------------------
  inc.panel <- rbind(inc.panel, temp.inc.panel.q)
  inc.panel <- inc.panel[inc.panel$time %in% timeframe.to.keep, ]
# Optional Plot inc.panel
  PlotPanelPDF(inc.panel, "inc_panel.pdf", mfrow = c(2,1))
 
# create real income
  incr.panel <- inc.panel
  cpi.panel <- cpi.panel[cpi.panel$date %in% timeframe.to.keep, ]
  incr.panel$value <- incr.panel$value / cpi.panel$value

# EOF ------------------------------------------------------------------------
  # remove temp values
    rm(i, temp.qrtrs, temp.time, t.start, t.end,
     temp.inc.panel, temp.inc.panel.q,
     temp.subset, temp.current, temp.inc)
  # remove unused datasets
  rm(inc.r.nat.q, inc.r.nat.a, cpi.panel)
  save(inc.panel, file="raw/inc_panel.RData")
