## Model 1
## Friday 10th July 2015


setwd("D:/GoogleDrive/ETH/2015_FS/MSc_Thesis/2_data");
load("data.RData")
#create df
df<-data.frame(rep(0,101))

#add year and cpi
df$year<-cpi$cpia$date;
df$cpi<-cpi$cpia$cpia201012;
df[,1]<-NULL;

#add pop data
temp<-NULL
for(i in 1:4){
temp[i]<-pop$Schweiz$sum2[i*2]
}
df$pop<-c(rep(0,101-length(temp)),temp)
rm(temp,i)

#shorten to last 4 years where pop is available
df<-df[98:101,]

#add 30y bond rates / currently by hand calculated in excel
# temp<- as.data.frame(rates$all$Date)
# temp[2]<-rates$all$X30ySNB
# temp<-temp[312:522,]
# x30ySNB<-ts(temp[2],frequency=13,start=c(1997,12))
# x30ySNBa<-aggregate(x30ySNB,nfrequency=1,FUN=mean)
# df$rate<-x30ySNBa
# rm(x30ySNB,temp)
df$rate<-c(1.914,1.164,1.572,1.400)

#add mpi / currently by hand, from table
df$mpi<-c(100.8,101.4,101.8,103.0)


#simple regression
lm.mpi<-lm(df$mpi~df$cpi+df$pop+df$rate)

