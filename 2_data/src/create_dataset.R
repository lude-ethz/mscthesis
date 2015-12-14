## @knitr data
require(reshape2)
# create_dataset.R--------------------------------------------------------------
# Purpose: Script to create datasets/working environments from raw data


# TODO -------------------------------------------------------------------------
# - bundle loading of necessary files at top for easy adaptation
# - proper save of each variables data
# - 


# Content ----------------------------------------------------------------------
# - prep lines
# - import: population from espop
# - import: population from statpop
# - import: interest rates
# - import: cpi
# - import: rent price index
# - import: unemployment
# - import: home price index
# - import: home prices
# - import: icnome
# - import: housing stock
# - import: mortgages
# - EOF confirm

# prep lines -------------------------------------------------------------------
str <- NULL #temp string for confirmation later
# set WD, unnecessary if called through main.R
#setwd("D:/GoogleDrive/ETH/2015_FS/MSc_Thesis/2_data")

# Population from BFS ESPOP,  1981-2009 ----------------------------------------
  # TODO: insert script to import espop population data?
  cat("------ create_pop_panel.R\n")
  source(file.path("src","create_pop_panel.R"))
  cat("------ create_pop_panel.R...done\n")
  
  
  
# <POSSIBLY OUTDATED> Population from BFS STATPOP 2010-2013 ----------------------------------------
  ## desc: this data is annual,  2010-2013end
  #load population data
#   pop <- new.env();
#   pop$all <- as.data.frame(read.csv("raw/pop/pop.csv"))
#   pop$desc <- as.data.frame(read.csv("raw/pop/pop_desc.csv"));
#   pop$cantons <- as.data.frame(read.csv("raw/pop/pop_cantons.csv"));
#   pop$type <- as.data.frame(read.csv("raw/pop/pop_type.csv"));
# 
#   #create pop subsets for each canton
#   for (i in pop$cantons[, 1]){
#     assign(i, subset(pop$all, pop$all$cantonname==i), envir=pop)
#     row.names(pop[[i]]) <- 1:8;
#     #cat("created pop subset for:", i, "\n")
#   }
#   rm(i)
#   #save as environment pop as pop.RData
#   save(pop, file="raw/pop.RData")
#   str <- c(str, "/raw/pop.RData\n")

# Pop Zurich/from cantonal buerau of statistics --------------------------------
# OBSOLETE,  now that we have district level data form BFS inquiry
# load data,  convert to xts,  remove column "Date" as index of xts object is the
# same
#   pop.zhr <- new.env()
#   pop.zhr$raw <- data.frame(read.csv("raw/pop/pop_zhr_district_annual_1960_2014.csv"));
#   
#   pop.zhr$a <- as.xts(pop.zhr$raw, order.by = as.Date(pop.zhr$raw$Date, origin="1899-12-30"));
#   pop.zhr$a$Date <- NULL;
#   
#   #add missing year 1961,  implicitly convert to numeric values
#   suppressWarnings(
#     pop.zhr$a <- c(pop.zhr$a, 
#                    xts(matrix(data=as.numeric(NA), 
#                               ncol=dim(pop.zhr$a)[2]),
#                        as.Date("1961-12-31")))
#   )
#   # interpolate quarterly series for pop.zhr in popq.zhr
#   # ...by using splines
#     # generate dates for new quarterly datapoints
#     qrtrs <- seq(as.Date("1961/1/1"),  as.Date("2015/1/1"),  "quarters")-1;
#     # initialize qrtly series
#     pop.zhr$q <- xts(matrix(nrow=length(qrtrs), ncol=dim(pop.zhr$a)[2]), order.by=qrtrs);
#     colnames(pop.zhr$q) <- colnames(pop.zhr$a);
#     
#     # apply interpolation over all districts of pop.zhr (all columns)
#     for(j in 1:(ncol(pop.zhr$a))){
#       pop.zhr$q[, j] <- xts(data.frame(spline(index(pop.zhr$a), pop.zhr$a[, j], xout=qrtrs))[2], order.by = qrtrs)
#     }
#     rm(j)
#   
#   #save
#   save(pop.zhr, file="raw/popzhr.RData");
#   str <- c(str, "/raw/popzhr.RData\n")
  

# Interest rates -------------------------------------------------------
  rates <- new.env();
  rates$desc <- as.data.frame(read.csv("raw/rates/rates_desc.csv"));
  rates$all <- as.data.frame(read.csv("raw/rates/rates.csv"));
  rates$all <- as.xts(
        rates$all[, 2:dim(rates$all)[2]], 
        order.by = as.Date(rates$all$Date, origin="1899-12-30"))
  
  # create in panel data format
  rates$all2 <- as.data.frame(read.csv("raw/rates/rates.csv", 
                                       header = TRUE))
  
  # remove missing values ("M") by converting to numeric & NA
  rates$all2 <- suppressWarnings(apply(rates$all2, c(1,2), as.numeric))
  
  # TODO: convert date column: original format: "yyyy mm"
  {}
  
  #TODO: add region identifier (necessary?)
  rates$all2 <- data.frame( region = rep("Schweiz", nrow(rates$all2)),
                            rates$all2)
  
  save(rates, file="raw/rates.RData");
  str <- c(str, "/raw/rates.RData\n");

# CPI -------------------------------------------------------
  cpi <- new.env();
  cpi$desc <- as.data.frame(read.csv("raw/cpi/cpi_desc.csv"));
  
  #create annual series in xts obj
  cpi$a <- as.data.frame(read.csv("raw/cpi/cpia.csv"));
  cpiyears <- seq(as.Date(paste(first(cpi$a$date), "-12-31", sep="")), as.Date(paste(last(cpi$a$date), "-12-31", sep="")), "years")
  cpi$a <- data.frame(region = rep("schweiz", nrow(cpi$a)),
                      cpi$a)
  #cpi$a <- as.xts(cpi$a[2:dim(cpi$a)[2]], order.by = cpiyears)
  
  #create monthly series in xts obj
  cpi$m <- as.data.frame(read.csv("raw/cpi/cpim.csv"))
  
  
  cpi$m <- as.xts(cpi$m, order.by=as.Date(cpi$m$date, origin="1899-12-30")-1);
  #monthly data in original file is dated for the 1st of each month,  
  #meaning data published on 01-jan is data of december (VERIFY!)
  #adjust dates to 31st of each previous month,  so data from december are values form december
  
  # add region column for panel data format
  cpi$m <- data.frame(region = rep("schweiz", nrow(cpi$m)),
                      cpi$m)
  #TODO: if not in xts style, then
  # cpi$m$date <- currently in excel style integer rep. reformat to yyyymm
  
  #create quarterly set
  # TODO: create quartlery set in panel data format
  cpi$q <- cpi$m[endpoints(cpi$m, "quarters"),]
  colnames(cpi$q) <- c("region", "date", "cpiq191406", "cpiq193908", "cpiq196609", "cpiq197709", "cpiq198212", "cpiq199305", "cpiq200005", "cpiq200512", "cpiq201012", "ccpiq")
  
  save(cpi, file="raw/cpi.RData", precheck=T);
  str <- c(str, "/raw/cpi.RData\n")
  rm(cpiyears)
  

# Rent Price Index BFS -------------------------------------------------------

  #original annual series 
  ## load rent price index,  columns= rent price index with different base  values in yyyymm, 
  ## rows=monthly value,  values are published at beginning of month,   
  ## for convenience,  will date each months value to the last day of the previous
  ## adjust dates to 31st of each previous month,  so data from december are values form december
    mpim.raw <- read.csv("raw/rent/mpim.csv")
    start <- as.Date(paste(min(mpim.raw$year), "-01-01"), "%Y -%m-%d")
    end <- as.Date(paste(max(mpim.raw$year), "-12-01"), "%Y -%m-%d")
    dates <- seq(from=start,  to=end,  by="1 months")-1;
  
  #create new env and save monthly series as an xts object under 'mpi$m',  remove
  #year and month column
    mpi <- new.env();
    mpi$m <-  xts(mpim.raw[, 3:ncol(mpim.raw)],  order.by=dates)
  #create quarterly series,  where datapoints = value at end of each quarter.
    mpi$q <- mpi$m[endpoints(mpi$m, "quarters")]
  #save  
    save(mpi, file="raw/mpi.RData")
    str <- c(str, "/raw/mpi.RData\n")
    rm(mpim.raw, start, end, dates)
    
    

# unempl -------------------------------------------------------
    # load original unempl series,  1991q2-2015q1
    # years 1991-2009 are q2 anually only,  2010q1-2015q1 are quarterly
    empl <- new.env()
    empl$unempl.raw <- read.csv("raw/empl/unempl.csv")
    empl$unempl.raw <- as.xts(empl$unempl.raw, order.by=as.Date(empl$unempl.raw$Date,  origin="1899-12-30"))
    empl$unempl.raw$Date <- NULL

    #interpolate quarterly values for all years where 
    qrtrs <- seq(from=as.Date("1991-07-01"),  to=as.Date("2015-04-01"), by="months")-1
    
    spln <- data.frame(spline(index(empl$unempl.raw), empl$unempl.raw, xout=qrtrs),  method="fmm")  
    spln$x <- as.Date(spln$x, offset="1899-12-30")
    spln <- as.xts(spln$y, spln$x)
    spln.q <- spln[endpoints(spln, "quarters")]
    empl$unempl.q <- spln.q
    colnames(empl$unempl.q) <- colnames(empl$unempl.raw)
    
    # visualize interpolated data (optional)
    #plot(spln,  main="Spline interpolation for unemployment numbers \n (default method Forsythe,  Malcolm and Moler)")
    #lines(spln.q, col="blue")
    #lines(empl$unempl.raw, type="o", col="red")
    
    rm(spln, spln.q)
    rm(qrtrs)
    
    
    #save 
    save(empl, file="raw/empl.RData")
    str <- c(str, "/raw/empl.RData\n")

# Real Estate price index SNB/W&P ----------------------------------------------
# comment from publication re methodology of index:
# "The annual indices are calculated using a weighted arithmetic mean of the 
# quarterly indices. The calculation is based on the quarters for the current 
# year available at the time in question. The weight accorded to each quarter
# varies according to supply quantities."
    
  pindx <- new.env()
  pindx$q <- as.data.frame(read.csv("raw/price/price_index_SNB.csv"))
  pindx$q$Date <- as.Date(pindx$q$Date,  origin="1899-12-30")
  pindx$q <- as.xts(pindx$q[, 2:ncol(pindx$q)], order.by=pindx$q$Date)
  
  # create custom total measure for all types
  # methodology: equal weighted of Apt_rent_tot, Apt_owned, SingleFamHome 
  pindx$q$tot <- rowSums(cbind(pindx$q$Apt_rent_tot,
                               pindx$q$Apt_owned,
                               pindx$q$SingleFamHome))/3

# home prices -------------------------------------------------------
  
  price <- new.env()
  #import data,  as.data.frame to let R convert into proper data types. file has 
  #strings and numers,  if only use read.csv,  integer numbers will be stored as 
  #character with potentially problematic behaviour,  e.g. 101 stored as " 101"
  #with a space
  price$raw <- as.data.frame(read.csv("raw/price/mean_median.csv"))

  price_small <- data.frame(region = price$raw$BFS_BEZIRK_NAME,
                            time = price$raw$QUARTAL_EIGENTUM,
                            type = price$raw$OBJEKT_ART_TEXT,
                            median = price$raw$MEDIAN_KAUFPREIS,
                            mean = price$raw$MEAN_KAUFPREIS)
  
  m_price_small <- melt(data = price_small,
                        id.vars = c("region","time","type"))
  #create a reference table for bezirk IDs/names
  price$cantons <-  as.matrix(unique(data.frame(price$raw$BFS_KANTON_NAME, price$raw$BFS_BEZIRK_ID, price$raw$BFS_BEZIRK_NAME)))
  write.csv(price$cantons, file="raw/price/desc_cantons_districts.csv")
  
  
  # create proper dates for observations for future xts conversion
  # date in raw data given in 5 digit numbers,  format=yyyyq,  q={1, 2, 3, 4}
  # some districts have 2015q2 ,  some do not....therefore checking every single entry....
  # quite time intensive,  any alternatives?)
    price$raw$y <- round(price$raw$QUARTAL_EIGENTUM/10, 0)
    price$raw$q <- price$raw$QUARTAL_EIGENTUM%%price$raw$y
    price$raw$m <- price$raw$q*3;
    #s <- Sys.time()
    for(i in seq(along=1:nrow(price$raw))){
      t <- price$raw$m[i]
      # if(is.integer(t))stop("price$raw$m are not integer,  pls check")
      if(t<=7) price$raw$d[i] <- as.character(30+t%%2)
      else if(t<=12) price$raw$d[i] <- as.character(30+(t-7)%%2)
      else stop("price$raw$m seems to have values >12")
    }
    #e <- Sys.time()
    #print(e-s)
    rm(i, t)
    price$raw$date <- as.Date(paste(as.character(price$raw$y), as.character(price$raw$m), as.character(price$raw$d), sep="-"))
  #convert to xts  
    price$raw <- as.xts(price$raw, order.by = price$raw$date)
  
  #create subsets for apartments and hosues;
  #subsetting an xts object with multiple observations at the same date/time
  #will give index wawrnings when subsetting,  using supressWarnings to supress.
    suppressWarnings(price$house <- subset(price$raw, OBJEKT_ART_CODE==1))
    suppressWarnings(price$apt <- subset(price$raw, OBJEKT_ART_CODE==2))
  
  ### WORK IN PROGRESS ###
  # create datasets in XTS format
  ### ### ### ### ### ###  

#   n_bezirk <- length(unique(price$raw$BFS_BEZIRK_ID))
#   price$sfh <- new.env()
#   
#   price$sfh$mean <- as.xts(data.frame(matrix(NA,  nrow = length(unique(price$raw$date)),  ncol = n_bezirk)), order.by=unique(index(price$raw)))
#   colnames(price$sfh$mean) <- paste(rep("b", n_bezirk), as.integer(unique(price$raw$BFS_BEZIRK_ID)), sep="")
#   t <- 1
#   for (i in unique(price$raw$BFS_BEZIRK_ID)){
#     print(i)
#     price$sfh[, t] <- subset(price$raw, BFS_BEZIRK_ID==i & OBJEKT_ART_CODE==1, MEAN_KAUFPREIS)
#     price$sfh[, n_bezirk+t] <- subset(price$raw, BFS_BEZIRK_ID==i & OBJEKT_ART_CODE==1, MEDIAN_KAUFPREIS)
#     t <- t+1
#   }
#     
    
  #save prices
    save(price, file="raw/price.RData")
    str <- c(str, "/raw/price.RData\n")

# # Now in seperate file! ------------------------------------------------------
# # income ---------------------------------------------------------------------
#   inc <- new.env()
# # income bfs: national aggregated, 1998-2012 monthly nominal
#   inc$inc1$a <- read.csv("raw/inc/inc_national_brutto_1998_2012.csv",
#                          skip = 4,
#                          header = TRUE)
#   colnames(inc$inc1$a) <- c("date", "inc")
#   yrs <- inc$inc1$a$date
#   qrtrs <- first(yrs):last(yrs)
#   
#   yrs <- as.Date(paste(inc$inc1$a$date,
#                        rep("-12-31", length(yrs)),
#                        sep=""))
#   
#   qrtrs <- as.Date(paste(sort(rep(qrtrs,4)), 
#                          rep(c("-03-31","-06-30","-09-30","-12-31"),length(qrtrs)),
#                          sep = ""))
#   # TODO : interpolation methodology:
#   # - using cubic spline, make sense even? (year end data only, possibly linear?)
#   # - cubic spline seems to be wrapping data at endpoints? (see beginning 3 points which is strictly speaking extrapolation)
#   spln <- spline(x = yrs,
#                  y = inc$inc1$a$inc,
#                  xout = qrtrs,
#                  method="fmm")
#   
#   inc$inc1$q <- as.xts(spln$y,
#                        order.by = as.Date(spln$x))
#   colnames(inc$inc1$q) <- "inc_q"
#   
#   inc$inc1$a <- as.xts(inc$inc1$a$inc,
#                        order.by = yrs)
#   
#   
#   # optionally visualize inc series
#   plot(x = inc$inc1$q,
#        type = "o",
#        main = "National median monthly income \n interpolated series")
#   lines(inc$inc1$a, type = "h", col = "red")
#   
# # income bfs: national aggregated by type of employment, annual median pretax income
#   
#   inc$inc_median$a <- read.csv("raw/inc/inc.csv")
#   inc$inc_median$a$Date <- as.Date(inc$inc_median$a$Date,  "%d/%m/%Y")
#   inc$inc_median$a <- as.xts(inc$inc_median$a[, 2:ncol(inc$inc_median$a)], order.by = inc$inc_median$a$Date) #by dropping date column,  new xts object will be of type integer as deisred
#   
#   qrtrs <- seq(as.Date("1992-01-01"), as.Date("2015-01-01"), "quarters")-1
#   inc$inc_median$q <- xts(matrix(nrow=length(qrtrs), ncol=ncol(inc$inc_median$a)), order.by = qrtrs)
#   colnames(inc$inc_median$q) <- colnames(inc$inc_median$a);
#   
#   #interpolate quarterly data for all working types
#   for (i in seq(along=(1:ncol(inc$inc_median$a)))){
#     inc$inc_median$q[, i] <- data.frame(spline(index(inc$inc_median$a), inc$inc_median$a[, i], xout=qrtrs))[, 2]
#   }
#   
#   inc$inc_median$q$mean_of_median <- rowSums(inc$inc_median$q[,1:4])/4
#   
#   
#   # create income panel - format: region|time|variable|value
#     # income panel: national data
#     # convert date to bfs format yyyyq (1991q4-2014q4)
#       temp_time <- paste(sort(rep(1991:2014,4)),
#                          rep(1:4,length(1991:2014)),
#                          sep = "")
#       temp_time <- temp_time[4:length(temp_time)]
#       
#       inc$inc_panel <- data.frame(region = rep("Schweiz", nrow(inc$inc_median$q)),
#                                   time = temp_time,
#                                   variable = rep("mean_of_median", nrow(inc$inc_median$q)),
#                                   value = inc$inc_median$q$mean_of_median)
#       colnames(inc$inc_panel) <- c("region", "time", "variable", "value")
#       rownames(inc$inc_panel) <- 1:nrow(inc$inc_panel)
#       
#   
#     # income panel zurich data: 1999-2012, annual, mean and median, zurich districts
#       temp_inc <- read.csv("raw/inc/inc_zhr_1999_2012.csv", header = TRUE)  
#       temp_inc_panel <- data.frame(region = temp_inc$GEBIET_NAME,
#                                   time = temp_inc$INDIKATOR_JAHR,
#                                   variable = temp_inc$INDIKATOR_ID,
#                                   value = temp_inc$INDIKATOR_VALUE)
#       temp_inc_panel$variable[temp_inc_panel$variable == 316] <- "median"
#       temp_inc_panel$variable[temp_inc_panel$variable == 317] <- "mean"
#       
#       #interpolate quarterly
#       temp_inc_panel <- dcast(data = temp_inc_panel, 
#                               formula = region + time ~ ...) #bring out mean and median ts
#            
#       t_start <- min(temp_inc_panel$time)
#       t_end <- max(temp_inc_panel$time)
#       temp_time <- as.numeric(sort(seq(t_start+1,t_end+1,0.25)))
#       
#       temp_inc_panel_q <- data.frame()
#       for(i in unique(temp_inc_panel$region)){
#         temp_subset <- subset(temp_inc_panel, region == i)
#         current <- data.frame(region = rep(i, length(temp_subset$time)*4-3),
#                               time = temp_time)
#         current$mean <- spline(x = seq(1,length(temp_subset$time),1),
#                                y = temp_subset$mean,
#                                xout = seq(1,length(temp_subset$time), 0.25),
#                                method="fmm")$y
#         current$median <- spline(x = seq(1,length(temp_subset$time),1),
#                                y = temp_subset$median,
#                                xout = seq(1,length(temp_subset$time), 0.25),
#                                method="fmm")$y
#         temp_inc_panel_q <- rbind(temp_inc_panel_q, current)
#       }
#      temp_inc_panel_q <- melt(data = temp_inc_panel_q,
#                         id.vars = c("region", "time"))
#       inc$inc_panel <- rbind(inc$inc_panel, temp_inc_panel_q)
#   
#       
#       # TODO: national data, still in yearly format, need to interpolate quarterly.
#       rm(i, qrtrs, spln, temp_time)
#   save(inc, file="raw/inc.RData")
#   str <- c(str, "/raw/inc.RData")
  
# housing stock -------------------------------------------------------
  hstock <- new.env()
  hstock$raw <- read.csv("raw/stock/hstock_cantonal_1990_2013.csv",  skip=2, header=T)
  hstock$raw <- melt(hstock$raw, id.vars="X")
  hstock$temp <- dcast(hstock$raw, variable~X, value.var="value")
  hstock$national <- data.frame(year=colsplit(hstock$temp$variable, pattern="X", names=c("tx", "ty"))$ty, hstock=hstock$temp$Schweiz)
  hstock$temp <- NULL

  
# interpolate quarterly hstock
  # prep dates for interpolation
  yrs <- hstock$national$year
  qrtrs <- hstock$national[1, 1]:hstock$national[nrow(hstock$national), 1]
  n_yrs <- length(yrs)
  n_qrtrs <- length(qrtrs)
  yrs <- as.Date(paste(yrs, rep("-12-31", n_yrs), sep=""))
  qrtrs <- as.Date(paste(sort(rep(qrtrs, 4)), rep(c("-03-31", "-06-30", "-09-30", "-12-31"), n_qrtrs), sep=""))
  hstock$national <- as.xts(hstock$national$hstock, order.by = yrs)
  
  # spline interpolate
  spln <- data.frame(spline(index(hstock$national), hstock$national, xout=qrtrs,  method="hyman"))
  spln <- as.xts(spln$y, order.by = as.Date(spln$x))

  #merge into hstock$national
  hstock$national <- merge(hstock$national, spln)["2000-12-31/"]
  colnames(hstock$national) <- c("raw", "q")
  
  #visualize plot (optional)
  plot(hstock$national$q, type="o", main = "National housing stock \n interpolated quarterly from biennial")
  lines(hstock$national$raw, type="h", col="red")
  
  rm(qrtrs, n_qrtrs, yrs, n_yrs, spln)

  

# MORTGAGES ---------------------------------------
  mortg <- new.env()

# mortgage rates 
  mortg$rts <- read.csv("./raw/mortgage/mortgage_rates_1937_2007_national.csv", 
                        skip=10, 
                        header=F)
  
  # additional description at beginnin - cut first 8 rows,
  # take only 1st and 3rd column for dates and residential rates
  end <- nrow(mortg$rts)
  mortg$rts <- data.frame(mortg$rts[9:end, 1], mortg$rts[9:end, 3])
  colnames(mortg$rts) <- c("date",  "r_m")
  
  a <- colsplit(mortg$rts[, 1], patter=" ", c("y", "m"))
  d <- rep(rep("1", 12), last(a$y)-first(a$y)+1)
  a$d <- d[1:nrow(a)]
  a$date <- as.Date(paste(a$y, a$m, a$d, sep="-"))
  
  mortg$rts <- as.xts(
                as.numeric(as.character(mortg$rts$r_m)), #double convert to proper handle characters/format
                order.by=a$date)
  
  rm(a, end)

# mortgage volume
  

# confirm -----------------------------------------
  cat("###\nCreated datasets:\n", str, "  in", getwd(), "\n###\n")
  rm(str)
  save.image(file="data.RData")
  cat("###\nCreated summary dataset \n data.RData in\n",  getwd(), "\n###\n")
