#read test.csv / try to filter pop data....

#requires directory structure:
# wd: main data folder
# csv data in folder: <WD>/raw/pop/bfsespop/

require(reshape2)

#### import from ESPOP 1981-2009 ####

#timeframe of data available in the folder
t <- 2009:1981
#file name syntax: "T41_YYYYT_6.csv" where YYYY is the year of the data
f1 <-  "./raw/pop/bfsespop/T41_"
f2 <- "T_6.csv"

#initialize empty data frame for results
#format: columns = observation years,  rows = districts
pop.bezirk <- data.frame(matrix(NA, ncol=2*length(t)+1, nrow=159))
colnames(pop.bezirk) <- c("Bezirk", paste(rep(c("pop_", "imi_"), length(t)), sort(c(t, t), decreasing=TRUE), sep=""))

#for all years in t
for (i in 1:length(t)){
  #define the filename for that year
  f <- paste(f1, t[i], f2, sep="")
  
  if (file.exists(f)){
    #read file for year t[i]
    a <- read.csv(f, skip=12, header=TRUE) 
    
    #take only name of region and year end population,  population as number and with spaces removed
    a <- data.frame(x1=a[, 1], 
                    x2=as.numeric(gsub(pattern=" ",  replacement="", x=a[, 11])), 
                    x3=as.numeric(gsub(pattern=" ",  replacement="", x=a[, 9]))) 
    
    #Filter out Municipalities: regions are switzerland, canton, district, municipality. 
    # Municipalities names are alphanumeric,  rest only letters,  so filter by only letters
    a <- a[grepl("^[[:alpha:]]", a$x1), ]
    #Filter out cantonal level data,  filter by string for cantons (written as
    #Kanton,  Canton,  Cantone)
    a <- unique(a[!(grepl("anton", a$x1)), ])

    #add yearly datapoints to result data frame
      # if first year,  add the district names
      if (i==1) 
        pop.bezirk[, 1] <- as.character(a$x1)
      #add data to result table,  check that district names have not changed.
      if (all(pop.bezirk[, 1] == as.character(a$x1))) 
        pop.bezirk[, (2*i):(2*i+1)] <- data.frame(a$x2, a$x3)
      #if district names are not the same as previous - throw error,  abort.
      else stop(cat("Period", t, "district names are not the same as previous years!", sep=""))
  }
  #if file for year t[i] was not found,  show warning
  else cat("###\n## WARNING: Did not find file ", f, " for year ", t[i], "\n###\n", sep="")
}


#### reshape espop data and plot for overview ####
a <- melt(pop.bezirk, id.vars="Bezirk")
splt <- colsplit(a$variable, "_", c("variable", "year"))
a <- data.frame(bezirk=a$Bezirk, year=splt$year, variable=splt$variable, value=a$value)
a <- dcast(a, year+variable~bezirk)

espop.pop <- subset(a, variable=="pop")
yrs <- espop.pop$year
yrs <- as.Date(paste(yrs,  rep("-12-31",  length(yrs)),  sep = ""))
espop.pop <- as.xts(espop.pop,  order.by = yrs)
espop.pop$year <- NULL

espop.imi <- subset(a, variable=="imi")
yrs <- espop.imi$year
yrs <- as.Date(paste(yrs,  rep("-12-31",  length(yrs)),  sep = ""))
espop.imi <- as.xts(espop.imi,  order.by = yrs)
espop.imi$year <- NULL


pdf(file="plots/espop_pop.pdf")
par(mfrow=c(2, 2))
for(i in 3:ncol(espop.pop)){
  plot(index(espop.pop), espop.pop[, i], type="o",  main=paste("pop", colnames(espop.pop)[i], sep=" "))
}
dev.off()

pdf(file="plots/espop_imi.pdf")
par(mfrow=c(2, 2))
for(i in 3:ncol(espop.imi)){
  plot(index(espop.imi), espop.imi[, i], type="o",  main=paste("imi", colnames(espop.imi)[i], sep=" "))
}
dev.off()


# temp: interpolate espop data ----------------------------------------------

out <- as.Date(paste(sort(rep(1981:2009,4)),rep(c("-03-31","-06-30","-09-30","-12-31"),length(1981:2009)),sep = ""))
df <- data.frame(schweiz = as.numeric(espop.pop$'SCHWEIZ / SUISSE'))
df$schweiz[3] <- 0

spln <- spline(as.numeric(yrs), df$schweiz, xout = as.numeric(out))

rm(a, i, t, f, f1, f2)
# TODO: ---------------------------------------------------------------------
## 1. adjust for changing districts
## 2. add BFS bezirk ID in second col
## 3. add quarterly interpolated data

####
####### details of district changes:
####

### to remove/ceased to exist (2009 end)
# Bezirk Bischofszell
# Bezirk Diessenhofen
# Bezirk Steckborn
# District de Courtelary
# District de la Neuveville
# District de Moutier
# Amt Fraubrunnen
# Amt Biel
# Amt Burgdorf
# Amt Niedersimmental
# Amt Interlaken
# Amt Aarwangen
# Amt Obersimmental
# Amt Aarberg
# Amt Thun
# Amt Konolfingen
# Amt Nidau (part)
# Amt Signau
# Amt Frutigen
# Amt Oberhasli
# Amt Wangen
# Amt Saanen
# Amt Büren
# Amt Laupen
# Amt Trachselwald
# Amt Erlach
# Amt Schwarzenburg
# 
# 
### to add/newly formed
# Verwaltungskreis Bern-Mittelland
# Verwaltungskreis Biel/Bienne
# Verwaltungskreis Emmental
# Verwaltungskreis Frutigen-Niedersimmental
# Verwaltungskreis Interlaken-Oberhasli
# Verwaltungskreis Oberaargau
# Verwaltungskreis Obersimmental-Saanen
# Verwaltungskreis Seeland
# Verwaltungskreis Thun
# 
### reinclude	
# uri
# obwalden
# nidwalden
# glarus
# basel-stadt
# genf
# appenzell-IR
# Zug
#
#
### to rename	direct Transitions
# Bezirk Küssnacht
# Bezirk Maloja
# Bezirk See/District du Lac
# Amt Sursee
# Amt Entlebuch
# Amt Hochdorf
# Amt Willisau
# Amt Luzern


#### import from STATPOP 2010-NOW ####

#timeframe of data available in the folder
t <- 2010:2013
#read in ID numbers
f <- "./raw/pop/bfsstatpop/bfsstatpop_bezirk_id.csv"
id <- read.csv(f, header=T)

#file name syntax: "T41_YYYYT_6.csv" where YYYY is the year of the data
f <-  "./raw/pop/bfsstatpop/bfsstatpop.csv"

a <- read.csv(f, header=FALSE, skip=2)
colnames(a) <- c("year", "region", "variable", "value")
a$region <- gsub(pattern=">>", replacement="", a$region)
a <- dcast(a, year+region~., sum)

#add bezirk id REQUIRES: bezirk names to be sorted in alphabetical order with
#national total last! Fortunately,  dcast can take care of alphabetical sorting.
#+bezirk names have a space in front,  causes nationial total to skip to end
a$bezirk_id <- paste(rep("b", length(id$BFS_BEZIRK_ID)), id$BFS_BEZIRK_ID, sep="")
colnames(a) <- c("year", "bezirk_name", "pop", "bezirk_id")
a <- dcast(a, bezirk_id+bezirk_name~year,  value.var="pop")
colnames(a) <- c("bezirk_id", "bezirk_name", paste(rep("pop_", length(t)), t, sep=""))

pop.bezirk.statpop <- a

#temp df for plotting
a <- melt(pop.bezirk.statpop, id.vars=c("bezirk_id", "bezirk_name"))
splt <- colsplit(a$variable, "_", c("variable", "year"))
a <- data.frame(bezirk_id=a$bezirk_id, bezirk_name=a$bezirk_name, variable=splt$variable, year=splt$year, value=a$value)
a <- dcast(a, year+variable~bezirk_name)
pdf(file="plots/statpop_pop.pdf")
par(mfrow=c(2, 2))
for(i in 3:ncol(a)) plot(a$year, a[, i], type="o",  main=paste("pop", colnames(a)[i], sep=" "))
dev.off() 


# end

rm(splt)
