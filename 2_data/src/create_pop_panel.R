# create_pop_panel_bfs.R - DESCRIPTION --------------------------------
  # script to import Population and Imigration PURPOSE: data from two sources
  # (BFS ESPOP statistics and BFS STATPOP statistics), bring into panel data
  # structure, adjust for district changes around 2009, merge into one DF

# TODO: ------------------------------------------------------------------------
# 1/ replace current district filtering (based on alphanumeric, no "anton" ) 
# with proper list of districts to be included. (currently excluding Geneve, 
# Uri, ...) 2/ check pop data anomaly: as seen in plots, some regions show 
# impossible population jumps in timeframe 1992-2998 (almost drop to 0, jump
# back to previous levels), possibly a decimal point error?
 

# Output: ----------------------------------------------------------------------
  # Data frames in Environment
    # espop - espop data
    # statpop - statpop data
  # CSV files in Working directory
    # espop_regions.csv - all unique regions of espop
    # statpop_regions.csv - all unique regions of statpop

# requires ---------------------------------------------------------------------
  # directory structure: csv data in folder: <WD>/raw/pop/bfsespop/
  # WD: working directory / main data folder
  require(reshape2)

# Create folder structure for output -------------------------------------------
if(!dir.exists(file.path(getwd(),"output"))){
  if(dir.create(file.path(getwd(), "output"))){
    dir.create(file.path(getwd(),"output","plots"))
  }else{
    warning("Cannot create folder & subfolders", file.path(getwd(),"output"))
  }
}

# Create ESPOP -----------------------------------------------------------------
  # import from ESPOP 1981-2009
  
  #timeframe of data available in the folder
  t <- 2009:1981
  #file name syntax: "T41_YYYYT_6.csv" where YYYY is the year of the data
  f1 <-  file.path("raw", "pop", "bfsespop", "T41_")
  f2 <- "T_6.csv"
  
  #initialize empty data frame for results
  #format: columns = observation years,  rows = districts
  espop <- data.frame(matrix(NA, ncol=4))
  colnames(espop) <- c("region","time","pop","imig")
  
  #for all years in t
  for (i in 1:length(t)){
    #define the filename for that year
    f <- paste(f1, t[i], f2, sep="")
#     cat(f)
#     scan();
    if (file.exists(f)){
      # read file for year t[i]
      a <- read.csv(f, skip=12, header=TRUE) 
      
      # format into panel data format only with columns
      # region|time|population|imig, take only name of region and year end
      # population, population as number and with spaces removed
      a <- data.frame(region = a[, 1], 
                      time = rep(t[i], nrow(a)),
                      pop = as.numeric(gsub(pattern=" ",  replacement="", x=a[, 11])), 
                      imig = as.numeric(gsub(pattern=" ",  replacement="", x=a[, 9]))) 
      
      # a$region contains regions switzerland, canton, district, municipality. 
        # Filter out municip.. Municip. names are alphanumeric,  rest only letters,
        # so filter by only letters
        a <- a[grepl("^[[:alpha:]]", a$region), ]
        
        # can now convert to character strings
        a$region <- as.character(a$region)
        
        #Filter out cantonal level data,  filter by string for cantons (written as
        #Kanton,  Canton,  Cantone)
        # a <- a[!(grepl("anton", a$region)), ]
        
        # save into espop
        if (i==1){
          espop <- a
        } else { #add data to result table,  check that district names have not changed.
          espop <- rbind(espop, a)
        }
    }
    #if file for year t[i] was not found,  show warning
    else cat("###\n## WARNING: for Population of year ", t[i], 
             "\n##   did not find file ", f, "\n###\n", sep="")
  }
espop$region <- as.character(espop$region)

# Create STATPOP ----------------------------------------------------------------------
  # import from STATPOP 2010-2013
  #timeframe of data available in the folder
    t <- 2010:2013
  
  #read file containing numerical identifiers for each bezirk
    f <- file.path("raw", "pop", "bfsstatpop", "bfsstatpop_bezirk_id.csv")
    id <- read.csv(f, header=T)

  #read file containing STATPOP data
    f <- file.path("raw", "pop", "bfsstatpop", "bfsstatpop.csv")
    a <- read.csv(f, header=FALSE, skip=2)
    colnames(a) <- c("time", "region", "variable", "value") #give colnames as file
    a$region <- gsub(pattern=">> ", replacement="", a$region) #remove">> "from names
  
  # add a numerical region identificator as defined in id$BFS_BEZIRK_ID
    # sort a alphabetically by region names
      a <- dcast(a, region ~ time, sum)  # will convert year numbers into levels
    # sort id alphabetically by id$BFS_BEZIRK_NAME
      id <- id[order(id$BFS_BEZIRK_NAME),]
    
    # add the numerical region identifier from id$BFS_BEZIRK_ID
      if(all(a$region == id$BFS_BEZIRK_NAME)){ # make sure we have same bezirke
        a$region_id <- (id$BFS_BEZIRK_ID)
      } else { 
        stop("bfsstatpop.csv does not have same districts as 
             bfsstatpop_bezirk_id.csv")
      }
  
  # bring a into panel data format: time | region | region_id | pop
    a <- melt( data = a, 
               id.vars = c("region", "region_id"), 
               variable.name = "time", 
               value.name = "pop")
    a$time <- as.numeric(as.character(a$time)) # make a$time numeric again
  # add immigration data - currently missing!
    # TODO: extract STATPOP immigration data
    imig <- rep(NA, nrow(a))
    a <- cbind(a, imig)
    rm(imig)

  # finalize
    statpop <- a
    rm(a);
    

# Create espop_regions.csv, statpop_regions.csv --------------------------------
  if(dir.exists(file.path(getwd(),"output"))){
    write.csv(x = unique(espop$region), 
              file = file.path("output", "regions_espop.csv"),
              row.names = FALSE)
    write.csv(x = unique(statpop$region), 
              file = file.path("output", "regions_statpop.csv"),
              row.names = FALSE)
  } else {
    warning("Cannot find folder ", file.path(getwd(),"output"),"\n",
         "- Skipped creating \"output/regions_espop.csv\"\n",
         "- Skipped creating \"output/regions_statpop.csv\"\n"
    )
  }

# Filter Regions which stayed constant in 2009/2010 district change: -----------
  # GENERAL IDEA: 2009/2010 district naming changed. Keep districts which are
  # 1./the same, 2./geographyically mostly the same and underwent namechange.
    
  # After manual Matching in excel: take only districts which have not changed 
  # during 2009/2010 reorganization of Swiss districts OR which have only had a 
  # change in Name.
  # 2 alternative approaches: 1./ keep only ones that are the same name
  # (easiest, loose 29 regions from new statpop) 2/ compare manually with csv
  # output and include namechanges (loose 19 regions from statpop)
  
  # most stringent way: only keep same names
    # length(unique(statpop$region)) # 149
    # length(unique(espop$region)) # 159
    # length(to.keep) #120
    # to.keep <- intersect(espop$region, statpop$region)
    # espop <- espop[espop$region %in% to.keep,]
    # statpop <- statpop[statpop$region %in% to.keep,]
    # rm(to.keep)
  # manual comparison
    to.change <- as.data.frame(read.csv("raw/pop/pop_region_names_to_change.csv",
                                     header = TRUE)) # espop 10

   for(i in 1:nrow(to.change)){
     espop$region[espop$region == as.character(to.change$espop[i])] <- 
       as.character(to.change$statpop[i])
   }
  rm(to.change)

  # TESTING NEW TO KEEP method:
  
    to.keep.districts <- as.data.frame(read.csv(file.path("raw", "Districts.csv"), header = TRUE))
    to.keep.districts <- apply(to.keep.districts, 1, as.character)
    espop <- espop[espop$region %in% to.keep.districts,]
    statpop <- statpop[statpop$region %in% espop$region,]
    
  # OBSOLETE: section to delete certain unwanted regions,
  #           replaced by exhaustive list of wanted districts
#   to.delete <- as.data.frame(read.csv("raw/pop/pop_region_names_to_delete.csv",
#                                       header = TRUE)) # espop 29/ statpop 20
#   
#   # delete unwanted in espop
#   for(i in 1:length(unique(to.delete$espop.delete))){
#     espop <- espop[!(espop$region == as.character(to.delete$espop.delete[i])),] 
#   }
#   # delete unwanted in statpop
#   for(i in 1:length(unique(to.delete$statpop.delete))){
#     statpop <- statpop[!(statpop$region == as.character(to.delete$statpop.delete[i])),] 
#   }
#   rm(to.delete)
  

# merge ------------------------------------------------------------------------
  #add misisng Bezirk ID column
    # espop <- data.frame(region = espop$region, 
    #                     region_id = rep(NA,nrow(espop)),
    #                     time = espop$time, 
    #                     pop = espop$pop,
    #                     imig = espop$imig)
  
  # merge, remove region_id from statpop (currently not used)
  pop.panel.bfs <- rbind(espop,statpop[,-2])
 
  # bring into panel data format, sorted by region and time
  pop.panel.bfs <- melt(pop.panel.bfs, id.vars = c("region","time"))
  # Canton of Geneve is duped (counts as canton and district), filter out
    pop.panel.bfs <- pop.panel.bfs[!duplicated(pop.panel.bfs[,1:3]),]
  
    

# Interpolate quarterly values -------------------------------------------------
  pop.panel.bfs <- dcast(unique(pop.panel.bfs), formula = region + time ~ ...)
  t.start <- as.numeric(min(pop.panel.bfs$time)) + 1
  t.end <- as.numeric(max(pop.panel.bfs$time)) + 1
  temp.time <- seq(t.start, t.end, 0.25)
  temp.pop.panel.q <- data.frame()
  for(i in unique(pop.panel.bfs$region)){
    temp.subset <- subset(pop.panel.bfs, region == i)
    
    temp.current <- data.frame(region = rep(i, length(temp.time)),
                               time = temp.time) 

    temp.current$pop <- spline(x = temp.subset$time + 1,
                                    y = temp.subset$pop,
                                    xout = temp.time,
                                    method="fmm")$y
      # no value in interpolating imigration numbers, not a stock
#     temp.current$imig <- spline(x = temp.subset$time + 1,
#                                y = temp.subset$imig,
#                                xout = temp.time,
#                                method="fmm")$y
    temp.pop.panel.q <- rbind(temp.pop.panel.q, temp.current)
  }
  pop.panel.bfs.q <- temp.pop.panel.q
  
  

# Format to panel data ---------------------------------------------------------
  pop.panel.bfs <- melt(pop.panel.bfs, id.vars = c("region", "time"))
  pop.panel.bfs.q <- melt(data = pop.panel.bfs.q,
                          id.vars = c("region", "time"))
  espop <- melt(espop, id.vars = c("region", "time"))
  statpop <- melt(statpop, id.vars = c("region", "time", "region_id"))
  statpop <- statpop[,-3]
  

# Optional plot of espop, statpop and pop.panel.bfs --------------------------------
  PlotPanelPDF(pop.panel.bfs, "pop_panel_bfs.pdf", mfrow = c(2,1))
  PlotPanelPDF(pop.panel.bfs.q, "pop_panel_bfs.pdf", mfrow = c(2,1))
  PlotPanelPDF(espop, "espop_panel.pdf", mfrow = c(2,1))
  PlotPanelPDF(statpop, "statpop_panel.pdf", mfrow = c(2,1))
  

# end --------------------------------------------------------------------------
  # cleanup
  rm(espop, statpop)
  rm(temp.pop.panel.q, t.start, t.end, temp.current, temp.subset, temp.time)
  rm(to.keep.districts, f, f1, f2, i, id, t)
  save(pop.panel.bfs, file="raw/pop_panel.RData")
  save(pop.panel.bfs.q, file="raw/pop_panel_q.RData")
  # possibly to keep? (removing for easy sourcing in scripts)
    rm(pop.panel.bfs) # annual values with imig (most imig = NA)
  