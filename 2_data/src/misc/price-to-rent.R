## @knitr acid

## acid tests


#### 1 Simple Price to rent ####
# "OECD rent to price without fundamentals"
# from Girouard.N2006_Recent House PRice Developments: The Role of Fundamentals
# nominal house price index / rent from cpi

timeframe<-"1970-03-31/2014-12-31"
plotwindow<-"1987-03-31/2014-12-31"                              
dates<-index(priceindex$q[timeframe])
n_obs<-(2014-1970+1)*4
t<-1:n_obs
parold<-par(mfrow=c(2,1))


# get CPI , simple conversion to 1969q4 as base =100
  cpi_rent<-mpi$q$X196609[timeframe]/as.numeric(mpi$q$X196609["1969-12-31"])*100


# Single Family Houses (SFH)
  price_index_sfh<-as.numeric(priceindex$q$SingleFamHome[timeframe])
  # OECD "simple" price to rent ratio for SFH
  oecd_pr_sfh<-as.xts(as.numeric(price_index_sfh)/as.numeric(cpi_rent)*100,order.by = dates)
  oecd_pr_sfh_avg<-as.xts(rep(mean(oecd_pr_sfh),n_obs),order.by=index(oecd_pr_sfh))
  oecd_pr_sfh_dev<-oecd_pr_sfh-oecd_pr_sfh_avg
  oecd_pr_sfh_dev_pc<-oecd_pr_sfh_dev/oecd_pr_sfh_avg*100
    # plot
    plot(oecd_pr_sfh)
    lines(oecd_pr_sfh_avg,col="red")
    plot(oecd_pr_sfh_dev_pc[plotwindow])
    rm(oecd_pr_sfh_avg)  
    
    cat("Price to Rent values Sfh")
    cat("start:",oecd_pr_sfh_dev_pc["1987-03-31"])
    cat("end:",oecd_pr_sfh_dev_pc["2014-12-31"])
    cat("min(oecd_pr_sfh_dev_pc)=",min(oecd_pr_sfh_dev_pc))
    cat("max(oecd_pr_sfh_dev_pc)=",max(oecd_pr_sfh_dev_pc))
    cat("median(oecd_pr_sfh_dev_pc)=",median(oecd_pr_sfh_dev_pc))
    cat("mean(oecd_pr_sfh)=",mean(oecd_pr_sfh))
    
  # some descriptive data for price index SFH
  # lm_price_sfh<-lm(as.numeric(price_index_sfh)-100~0+t)
  # price_trend_sfh<-as.xts(lm_price_sfh$fitted.values+100,order.by=index(price_index_sfh))
  # price_dev_sfh<-as.xts(as.numeric(price_index_sfh)-as.numeric(price_trend_sfh),order.by=index(price_index_sfh))
  # price_dev_sfh_pc<-price_dev_sfh/price_trend_sfh*100
    # plot this descriptive data
    # plot(price_index_sfh)
    # lines(price_trend_sfh,col="red")
    # plot(price_dev_sfh_pc)
  

# Owner Occupied Apartments (APT)
  price_index_apt<-as.numeric(priceindex$q$Apt_owned[timeframe])
  
  # OECD "simple" price to rent ratio for APT
  oecd_pr_apt<-as.xts(as.numeric(price_index_apt)/as.numeric(cpi_rent)*100,order.by = dates)
  oecd_pr_apt_avg<-as.xts(rep(mean(oecd_pr_apt),n_obs),order.by=index(oecd_pr_apt))
  oecd_pr_apt_dev<-oecd_pr_apt-oecd_pr_apt_avg
  oecd_pr_apt_dev_pc<-oecd_pr_apt_dev/oecd_pr_apt_avg*100
    # plot
    plot(oecd_pr_apt)
    lines(oecd_pr_apt_avg,col="red")
    plot(oecd_pr_apt_dev_pc[plotwindow])
    rm(oecd_pr_apt_avg)  
    
    cat("Price to Rent values Apt\n");
    cat("start:",oecd_pr_apt_dev_pc["1987-03-31"]);
    cat("end:",oecd_pr_apt_dev_pc["2014-12-31"]);
    cat("min(oecd_pr_apt_dev_pc)=",min(oecd_pr_apt_dev_pc));
    cat("max(oecd_pr_apt_dev_pc)=",max(oecd_pr_apt_dev_pc));
    cat("median(oecd_pr_apt_dev_pc)=",median(oecd_pr_apt_dev_pc));
    cat("mean(oecd_pr_apt)=",mean(oecd_pr_apt));
  # some descriptive data for price index APT
  # lm_price_apt<-lm(as.numeric(price_index_apt)-100~0+t)
  # price_trend_apt<-as.xts(lm_price_apt$fitted.values+100,order.by=index(price_index_apt))
  # price_dev_apt<-as.xts(as.numeric(price_index_apt)-as.numeric(price_trend_apt),order.by=index(price_index_apt))
  # price_dev_apt_pc<-price_dev_apt/price_trend_apt*100
    # plot
    # plot(price_index_apt)
    # lines(price_trend_apt,col="red")
    # plot(price_dev_apt_pc)
  
  
#one general plot with cpi, index sfh, index apt
par(mfrow=c(3,1))
plot(price_index_sfh)
plot(price_index_apt)
plot(cpi_rent)

par(parold)

#overview plot of two price-to-rent series
plot(oecd_pr_sfh_dev_pc[plotwindow], main="Simple Price-to-Rent ratios in Switzerland\n  Single family homes [black]\n Owner occupied Apartments [blue]")
lines(oecd_pr_apt_dev_pc[plotwindow], col="blue")


rm(dates,parold,timeframe,n_obs,t,plotwindow)

#rm(lm_prices_apt,lm_prices_sfh,lm_cpi)

