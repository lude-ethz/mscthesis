## formatting mpi data
load("rent/mpim_dates.RData")
load("rent/t.RData")

mpim<-date;

temp<-NULL;
for (i in 1:dim(t)[1]){
  temp<-c(temp,as.double(t[i,2:length(t)]))
}
mpim[,2]<-temp
rm(temp)
colnames(mpim)<-c("date","mpim201012")
save(mpim,file="mpim.RData")
