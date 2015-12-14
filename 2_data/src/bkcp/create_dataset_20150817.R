## @knitr data

####
# Script to create datasets/working environments from raw data
####

rm(list=ls())
require(xts)
require(reshape2)
setwd("D:/GoogleDrive/ETH/2015_FS/MSc_Thesis/2_data");
str<-NULL #temp string for confirmation later


### Population from BFS ESTPOP, 1981-2009 ###

####################################################################
# INSERT 
####################################################################


#### Population from BFS STATPOP 2010-2013 ####
  ## desc: this data is annual, 2010-2013end
  #load population data
  pop<-new.env();
  pop$all<-as.data.frame(read.csv("raw/pop/pop.csv"))
  pop$desc<-as.data.frame(read.csv("raw/pop/pop_desc.csv"));
  pop$cantons<-as.data.frame(read.csv("raw/pop/pop_cantons.csv"));
  pop$type<-as.data.frame(read.csv("raw/pop/pop_type.csv"));

  #create pop subsets for each canton
  for (i in pop$cantons[,1]){
    assign(i,subset(pop$all,pop$all$cantonname==i),envir=pop)
    row.names(pop[[i]])<-1:8;
    #cat("created pop subset for:",i,"\n")
  }
  rm(i)
  #save as environment pop as pop.RData
  save(pop,file="raw/pop.RData")
  str<-c(str,"/raw/pop.Rdata\n")

# #### Population of Zurich/from cantonal buerau of statistics ####
  ## OBSOLETE, now that we have district level data form BFS inquiry
#   #load data, convert to xts, remove column "Date" as index of xts object is the
#   #same
#   pop.zhr<-new.env()
#   pop.zhr$raw<-data.frame(read.csv("raw/pop/pop_zhr_district_annual_1960_2014.csv"));
#   
#   pop.zhr$a<-as.xts(pop.zhr$raw,order.by = as.Date(pop.zhr$raw$Date,origin="1899-12-30"));
#   pop.zhr$a$Date<-NULL;
#   
#   #add missing year 1961, implicitly convert to numeric values
#   suppressWarnings(pop.zhr$a<-c(pop.zhr$a,xts(matrix(data=as.numeric(NA),ncol=dim(pop.zhr$a)[2]),as.Date("1961-12-31"))))
#   # interpolate quarterly series for pop.zhr in popq.zhr
#   # ...by using splines
#     # generate dates for new quarterly datapoints
#     qrtrs<-seq(as.Date("1961/1/1"), as.Date("2015/1/1"), "quarters")-1;
#     # initialize qrtly series
#     pop.zhr$q<-xts(matrix(nrow=length(qrtrs),ncol=dim(pop.zhr$a)[2]),order.by=qrtrs);
#     colnames(pop.zhr$q)<-colnames(pop.zhr$a);
#     
#     # apply interpolation over all districts of pop.zhr (all columns)
#     for(j in 1:(ncol(pop.zhr$a))){
#       pop.zhr$q[,j]<-xts(data.frame(spline(index(pop.zhr$a),pop.zhr$a[,j],xout=qrtrs))[2],order.by = qrtrs)
#     }
#     rm(j)
#   
#   #save
#   save(pop.zhr,file="raw/popzhr.RData");
#   str<-c(str,"/raw/pop.Rdata\n")
  

#### Rates ####
  rates<-new.env();
  rates$desc<-as.data.frame(read.csv("raw/rates/rates_desc.csv"));
  rates$all<-as.data.frame(read.csv("raw/rates/rates.csv"));
  rates$all<-as.xts(rates$all[,2:dim(rates$all)[2]],order.by = as.Date(rates$all$Date,origin="1899-12-30"))
  #load data into data frame, convert to xts (dates in original file are stored
  #as integers using excel offset, first load data frame for ease of converting)

  save(rates,file="raw/rates.RData");
  str<-c(str,"/raw/rates.RData\n");

#### CPI ####
  cpi<-new.env();
  cpi$desc<-as.data.frame(read.csv("raw/cpi/cpi_desc.csv"));
  
  #create annual series in xts obj
  cpi$a<-as.data.frame(read.csv("raw/cpi/cpia.csv"));
  cpiyears<-seq(as.Date(paste(first(cpi$a$date),"-12-31",sep="")),as.Date(paste(last(cpi$a$date),"-12-31",sep="")),"years")
  cpi$a<-as.xts(cpi$a[2:dim(cpi$a)[2]],order.by = cpiyears)
  
  #create monthly series in xts obj
  cpi$m<-as.data.frame(read.csv("raw/cpi/cpim.csv"));
  cpi$m<-as.xts(cpi$m[2:dim(cpi$m)[2]],order.by=as.Date(cpi$m$date,origin="1899-12-30")-1);
  #monthly data in original file is dated for the 1st of each month, 
  #meaning data published on 01-jan is data of december (VERIFY!)
  #adjust dates to 31st of each previous month, so data from december are values form december
  
  #create quarterly set
  cpi$q<-cpi$m[endpoints(cpi$m,"quarters")]
  colnames(cpi$q)<-c("cpiq191406","cpiq193908","cpiq196609","cpiq197709","cpiq198212","cpiq199305","cpiq200005","cpiq200512","cpiq201012","ccpiq")
  
  save(cpi,file="raw/cpi.RData",precheck=T);
  str<-c(str,"/raw/cpi.RData\n")
  rm(cpiyears)
  

#### Rent Price Index ####

  #original annual series 
  ## load rent price index, columns= rent price index with different base  values in yyyymm,
  ## rows=monthly value, values are published at beginning of month,  
  ## for convenience, will date each months value to the last day of the previous
  ## adjust dates to 31st of each previous month, so data from december are values form december
    mpim.raw<-read.csv("raw/rent/mpim.csv")
    start<-as.Date(paste(min(mpim.raw$year),"-01-01"),"%Y -%m-%d")
    end<-as.Date(paste(max(mpim.raw$year),"-12-01"),"%Y -%m-%d")
    dates<-seq(from=start, to=end, by="1 months")-1;
  
  #create new env and save monthly series as an xts object under 'mpi$m', remove
  #year and month column
    mpi<-new.env();
    mpi$m<- xts(mpim.raw[,3:ncol(mpim.raw)], order.by=dates)
  #create quarterly series, where datapoints = value at end of each quarter.
    mpi$q<-mpi$m[endpoints(mpi$m,"quarters")]
  #save  
    save(mpi,file="raw/mpi.RData")
    str<-c(str,"/raw/mpi.RData\n")
    rm(mpim.raw,start,end,dates)
    
    

#### unempl ####
    # load original unempl series, 1991q2-2015q1
    # years 1991-2009 are q2 anually only, 2010q1-2015q1 are quarterly
    empl<-new.env()
    empl$unempl.raw<-read.csv("raw/empl/unempl.csv")
    empl$unempl.raw<-as.xts(empl$unempl.raw,order.by=as.Date(empl$unempl.raw$Date, origin="1899-12-30"))
    empl$unempl.raw$Date<-NULL
    
    
    #interpolate quarterly values for all years where 
    qrtrs<-seq(from=as.Date("1991-07-01"), to=as.Date("2015-04-01"),by="months")-1
    
    spln<-data.frame(spline(index(empl$unempl.raw),empl$unempl.raw,xout=qrtrs), method="fmm")  
    spln$x<-as.Date(spln$x,offset="1899-12-30")
    spln<-as.xts(spln$y,spln$x)
    spln.q<-spln[endpoints(spln,"quarters")]
    empl$unempl.q<-spln.q
    colnames(empl$unempl.q)<-colnames(empl$unempl.raw)
    
    # visualize interpolated data (optional)
    #plot(spln, main="Spline interpolation for unemployment numbers \n (default method Forsythe, Malcolm and Moler)")
    #lines(spln.q,col="blue")
    #lines(empl$unempl.raw,type="o",col="red")
    
    rm(spln,spln.q)
    
#   # compare default spline method (FMM) vs hyman method, numeric example suggest 0 difference at interpolated dates.    
#     spln2<-data.frame(spline(index(empl$unempl.raw),empl$unempl.raw,xout=qrtrs),method="hyman")  
#     spln2$x<-as.Date(spln2$x,offset="1899-12-30")
#     spln2<-as.xts(spln2$y,spln2$x)
#     spln2.q<-spln2[endpoints(spln2,"quarters")]
#     
#     spln_difference_fmm_hyman<-spln-spln2
#     View(spln_difference_fmm_hyman)
#     scan();
#     rm(spln2,spln2.q,qrtrs)
    rm(qrtrs)
    
    
    #save 
    save(empl,file="raw/empl.RData")
    str<-c(str,"/raw/empl.RData\n")

#### home price index ####
  priceindex<-new.env()
  priceindex$q<-as.data.frame(read.csv("raw/price/price_index_SNB.csv"))
  priceindex$q$Date<-as.Date(priceindex$q$Date, origin="1899-12-30")
  priceindex$q<-as.xts(priceindex$q[,2:ncol(priceindex$q)],order.by=priceindex$q$Date)
  #priceindex$q<-as.numeric(priceindex$q)

  
#### home prices ####
  
  price<-new.env()
  #import data, as.data.frame to let R convert into proper data types. file has 
  #strings and numers, if only use read.csv, integer numbers will be stored as 
  #character with potentially problematic behaviour, e.g. 101 stored as " 101"
  #with a space
  price$raw<-as.data.frame(read.csv("raw/price/mean_median.csv"))

  #create a reference table for bezirk IDs/names
  price$cantons<- as.matrix(unique(data.frame(price$raw$BFS_KANTON_NAME,price$raw$BFS_BEZIRK_ID,price$raw$BFS_BEZIRK_NAME)))
  write.csv(price$cantons,file="raw/price/desc_cantons_districts.csv")
  
  
  # create proper dates for observations for future xts conversion
  # date in raw data given in 5 digit numbers, format=yyyyq, q={1,2,3,4}
  # some districts have 2015q2 , some do not....therefore checking every single entry....
  # quite time intensive, any alternatives?)
    price$raw$y<-round(price$raw$QUARTAL_EIGENTUM/10,0)
    price$raw$q<-price$raw$QUARTAL_EIGENTUM%%price$raw$y
    price$raw$m<-price$raw$q*3;
    #s<-Sys.time()
    for(i in 1:nrow(price$raw)){
      t<-price$raw$m[i]
      # if(is.integer(t))stop("price$raw$m are not integer, pls check")
      if(t<=7) price$raw$d[i]<-as.character(30+t%%2)
      else if(t<=12) price$raw$d[i]<-as.character(30+(t-7)%%2)
      else stop("price$raw$m seems to have values >12")
    }
    #e<-Sys.time()
    #print(e-s)
    rm(i,t)
    price$raw$date<-as.Date(paste(as.character(price$raw$y),as.character(price$raw$m),as.character(price$raw$d),sep="-"))
  #convert to xts  
    price$raw<-as.xts(price$raw,order.by = price$raw$date)
  
  #create subsets for apartments and hosues;
  #subsetting an xts object with multiple observations at the same date/time
  #will give index wawrnings when subsetting, using supressWarnings to supress.
    suppressWarnings(price$house<-subset(price$raw,OBJEKT_ART_CODE==1))
    suppressWarnings(price$apt<-subset(price$raw,OBJEKT_ART_CODE==2))
  
  ### WORK IN PROGRESS ###
  # create datasets in XTS format
  ########################  
#   n_bezirk<-length(unique(price$raw$BFS_BEZIRK_ID))
#   price$sfh<-new.env()
#   
#   price$sfh$mean<-as.xts(data.frame(matrix(NA, nrow = length(unique(price$raw$date)), ncol = n_bezirk)),order.by=unique(index(price$raw)))
#   colnames(price$sfh$mean)<-paste(rep("b",n_bezirk),as.integer(unique(price$raw$BFS_BEZIRK_ID)),sep="")
#   t<-1
#   for (i in unique(price$raw$BFS_BEZIRK_ID)){
#     print(i)
#     price$sfh[,t]<-subset(price$raw,BFS_BEZIRK_ID==i & OBJEKT_ART_CODE==1,MEAN_KAUFPREIS)
#     price$sfh[,n_bezirk+t]<-subset(price$raw,BFS_BEZIRK_ID==i & OBJEKT_ART_CODE==1,MEDIAN_KAUFPREIS)
#     t<-t+1
#   }
#     
    
  #save prices
    save(price,file="raw/price.RData")
    str<-c(str,"/raw/price.RData\n")

#### income ####
  inc<-new.env()
  inc$a<-read.csv("raw/inc/inc.csv")
  inc$a$Date<-as.Date(inc$a$Date, "%d/%m/%Y")
  inc$a<-as.xts(inc$a[,2:ncol(inc$a)],order.by = inc$a$Date) #by dropping date column, new xts object will be of type integer as deisred
  
  qrtrs<-seq(as.Date("1992-01-01"),as.Date("2015-01-01"),"quarters")-1
  inc$q<-xts(matrix(nrow=length(qrtrs),ncol=ncol(inc$a)),order.by = qrtrs)
  colnames(inc$q)<-colnames(inc$a);
  
  #interpolate quarterly data for all working types
  for (i in 1:ncol(inc$a))
  {
    inc$q[,i]<-data.frame(spline(index(inc$a),inc$a[,i],xout=qrtrs))[,2]
  }
  rm(i,qrtrs)
  save(inc,file="raw/inc.RData")
  str<-c(str,"/raw/inc.RData")
  
#### housing stock ####
  hstock<-new.env()
  hstock$raw<-read.csv("raw/stock/hstock.csv", skip=2,header=T)
  hstock$raw<-melt(hstock$raw,id.vars="X")
  hstock$temp<-dcast(hstock$raw,variable~X,value.var="value")
  hstock$national<-data.frame(year=colsplit(hstock$temp$variable,pattern="X",names=c("tx","ty"))$ty,hstock=hstock$temp$Schweiz)
  hstock$temp<-NULL
  
  #quarterly interpolation:
  ##########################
  ##########################
  ##########################
  ##########################
  ##########################
  ##########################
  
  
#### confirm ####
  cat("###\nCreated datasets:\n",str,"  in",getwd(),"\n###\n")
  rm(str)
  save.image(file="data.RData")
  cat("###\nCreated summary dataset \n data.RData in\n", getwd(),"\n###\n")
