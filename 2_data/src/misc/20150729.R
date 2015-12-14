rm(list=ls());
# Meeting MSc Thesis
# for: 2015.07.27 Wed. 2pm

##variables
timeframe<-"2000-01-01/2015-04-01"
##

# try setting up a VECM
# similar to anundsen paper:

if(require(xts)==FALSE){install.packages("xts"); require(xts)}
if(require(urca)==FALSE){install.packages("urca"); require(urca)}

setwd("D:/GoogleDrive/ETH/2015_FS/MSc_Thesis/2_data");

load("data.RData")


# we use the function ca.jo() from package urca to estimate VECM specifically we
# use ca.jo(..., type="transitory") to estimate model with PHI*y_(t-1) (see 
# Anundsen paper eq.9)

#ca.jo requires all data series to be in one data.frame/matrix where volumns are
#the variables y_i, rows are observations t.
#we create such a data frame "vecm.zhr" from data loaded in data.RData
BEZIRK_ID<-unique(price$apt$BFS_BEZIRK_ID)

affoltern<-cpi$q$cpiq200005[timeframe]
affoltern$unempl<-empl$unempl.q[timeframe]
affoltern$mpi<-mpi$q$X200005[timeframe]
affoltern$pop<-pop.zhr$q$Bezirk.Affoltern[timeframe]
affoltern$price<-subset(price$apt,BFS_BEZIRK_ID==101)
View(affoltern)

vecm.affoltern<-ca.jo(affoltern)
summary(vecm.affoltern)
