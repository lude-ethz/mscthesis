# 2015-07-14
# try to regress mpi on pop using quarterly data

rm(list=ls())
setwd("D:/GoogleDrive/ETH/2015_FS/MSc_Thesis/2_data");
load("data.RData")
load(file="raw/popzhr.RData")
require(xts)



#mpi is available in monthly/quarterly
#pop.zhr is annually, popq.zhr is quarterly spline interpolation.
# -> run regression

# growth in house prices vs inflation
# growth in house prices vs population stock/growth/influx
# growth in house prices vs rates

#1
mpizhr<-mpi$q$X199305["1995-01-01/2015-01-01"] #extract mpi with base 1993 for last 10 yrs

#2
popzhr<-popq.zhr["1995-01-01/2015-01-01"]

#3
for (i in 1:ncol(popzhr)){
  
  print(summary(a<-lm(mpizhr~popzhr[,i])))
  print(i)
  #plot(a$residuals)
  acf(a$residuals)
  scan()
}


