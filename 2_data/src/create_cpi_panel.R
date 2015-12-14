require(xts)
require(reshape2)

# load monthly data
  cpim <- as.data.frame(read.csv("raw/cpi/cpim.csv"))
  cpim$date <- as.Date(cpim$date, origin = "1899-12-30")-1
    #monthly data in original file is dated for the 1st of each month,  
    #meaning data published on 01-jan is data of december (VERIFY!)
    #adjust dates to 31st of each previous month,  so data from december are values form december

# extract quarterly data: CURRENTLY END OF QUARTER VALLUES ONLY, no averaging
  cpi.panel <- as.xts(cpim[-1], order.by=cpim$date)
  cpi.panel <- cpi.panel[endpoints(cpi.panel, "quarters"),]
  cpi.panel <- data.frame(region = "Schweiz", date = index(cpi.panel) , cpi.panel, 
                     row.names = NULL)
  colnames(cpi.panel) <- c("region", 
                      "date",
                      "cpi.panel191406", 
                      "cpi.panel193908", 
                      "cpi.panel196609", 
                      "cpi.panel197709", 
                      "cpi.panel198212", 
                      "cpi.panel199305", 
                      "cpi.panel200005", 
                      "cpi.panel200512", 
                      "cpi.panel201012", 
                      "ccpi.panel")

# convert dates to decimal, x.25 = xq1 , x.5 = xq2 , x.75 = xq3, x+1.00 = xq4
  dec.date <- colsplit(cpi.panel$date, "-", c("y","m","d"))
  dec.date$offset <- unlist(lapply(dec.date[,2], function(x){
    if(x == 12) return(1)
    if(x == 9) return(0.75)
    if(x == 6) return(0.5)
    if(x == 3) return(0.25)
    return(NA)  # remeber to remove dates later which are not a quarter.
                # endpoint() keeps most recent observation even if it does not
                # fall on specified dates
  }))
  dec.date$result <- dec.date$y + dec.date$offset
  cpi.panel$date <- dec.date$result
  cpi.panel <- cpi.panel[!is.na(cpi.panel$date),]  # remove dates which were NA in dec.date

# bring into panel data format
  cpi.panel <- melt(cpi.panel, id.vars =  c("region", "date"))
  
rm(cpim, dec.date)  
  
# SELECT ONE CPI BASE YEAR
  cpi.panel <- cpi.panel[cpi.panel$variable == "cpi.panel200005",]  

