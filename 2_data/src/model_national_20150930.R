# create national model:

# content --------------------------------------------------
# - global definitions/prep
# - get necessary data
# - some descriptive stats
# - run model
# -  ...

# global def & prep -------------------------------------
  
# requires create_dataset.R, if run from main.R, next line uncessecary
# source('D:/GoogleDrive/ETH/2015_FS/MSc_Thesis/2_data/src/create_dataset.R')

tf <- "2001-12-31/2012-12-31" #timeframe for observation
n_row <- 45 #number of observations, TIED TO tf!

# get data --------------------------------------------------------------------
# variables needed are:
# - cpi
# - house prices
# - housing stock
# - population
# - income
# - construction
# - immigration
# - rates
# - mortgage rates
# - mortgage volume

n_col <- 10 #number of variables
national <- data.frame(matrix(data = NA,
                              nrow = n_row,
                              ncol = n_col))
colnames(national) <- c("cpi",
                        "hp",
                        "hstock",
                        "pop",
                        "inc",
                        "constr",
                        "imi",
                        "r_r",
                        "r_m",
                        "vol_m")

national$cpi <- cpi$q$cpiq200005[tf]
national$hp <- pindx$q$tot[tf]
national$hstock <- hstock$national$q[tf]
national$inc <- inc$inc1$q[tf]
national$pop <- NULL # TODO: piece together espop/statpop
national$constr <- NULL
national$imi <- NULL
national$r_r <- NULL
national$r_m <- NULL
national$vol_m <- NULL

# temp espop/statpop merge: ----------------------------------------------------
# as long as espop and statpop are not properly integrated
# TODO: integrate espop/statpop,
# TODO: replace following temp merge section
  a <- espop.pop$'SCHWEIZ / SUISSE'[tf]
  
  b <- subset(pop.bezirk.statpop, bezirk_id == "b0")
  b <- xts(x = t(b[1,3:ncol(b)]), 
           order.by = as.Date(paste(2010:2013,rep("-12-31",length(2010:2013)),sep = ""))
           )
  c <- merge(a,b)
  c[10:13,1]<-c[10:13,2]
  colnames(c)<-c("schweiz","schweiz1")
  c<-c[,1]
  out <- seq(as.Date("2002-01-01"), as.Date("2014-01-01"), "quarters")-1
  
  spln<-data.frame(spline(as.numeric(index(c)),c$schweiz,xout = as.numeric(out)))
  d <- as.xts(spln$y, order.by = as.Date(spln$x))[tf]
  plot(d, type="o", main = "National Population - interpolated"); 
  lines(c, type="h", col="red")
  national$pop <- d
  
  rm(a,b,c,d, out, spln)
# end temp merge



# convert nominal to real values, where applicable -----------------------------
  #income:
  national$inc_r <- national$inc / national$cpi

# test for unit roots
  summary(ur.df(national$cpi, type = "trend")) # cannot reject unit root
  summary(ur.df(national$hp, type = "trend")) # cannot reject unit root
  summary(ur.df(national$hstock, type = "drift"))
  summary(ur.df(national$pop, type = "trend")) # cannot reject unit root
  summary(ur.df(national$inc_r, type = "trend")) # cannot reject unit root
  
  
  
# calc log variables
  log_national <- as.data.frame(mapply(log, national))
  
# decide which dataset to use
#   dataset <- log_national
#   cat("------ using LOG variables --------------------------------------------------")
  dataset <- national
  cat("------ using LEVEL variables --------------------------------------------------")
    
# some descriptive stats ------------------------------------------------------
summary(dataset)
df <- data.frame(hp = dataset$hp,
                 hstock = dataset$hstock,
                 inc = dataset$inc,
                 cpi = dataset$cpi,
                 pop = dataset$pop)
dataset_acf <- acf(df, plot = F)

# check for unit roots ---------------------------------------------------------
for (i in 1:ncol(dataset)){
  cat("----------------\nTesting for unit root in:\n",colnames(dataset[i]),
      "\n------------------\n")
  i_df <- ur.df(as.vector(dataset[,i]), type="trend", selectlags = "AIC")
  print(summary(i_df))
}
# result: currently mostly no evidence against unit root -> danger of spurious
# regressions if just using standard linear model


# create model in changes using diff(x,1,1)
cdataset <- data.frame(matrix(NA, ncol= ncol(dataset), nrow = nrow(dataset)))
colnames(cdataset) <- paste(rep("c",ncol(dataset)),colnames(dataset),sep="")
for (i in 1:ncol(dataset)){
  cdataset[i] <- diff(dataset[,i],1,1)
}
cdataset <- cdataset[2:nrow(cdataset),]

lm_cdataset <- lm(chp ~ chstock + cpop + cinc_r, data = cdataset)
print(summary(lm_cdataset));

# check cointegration ----------------------------------------------------------
for (i in 3:ncol(dataset)){
  cat("----------------\nTesting Cointegration for:\n",colnames(dataset[,c(2,i)]),
      "\n------------------\n")
  EGtest(dataset$hp,dataset[,i], trend = "trend") # custom EngleGranger test fun
}

# ECM model
# TODO: add VECM model if cointegration exists
# lm_dataset <- lm(hp ~ hstock + pop + inc_r, data = dataset)
# print(summary(lm_dataset))

# Panel Data Mode:
# Todo: add panel data model once known,
# potential starting point: fixed effect model, random effects model
# further points: Anundsen paper model.
# 

#EOF
rm(n_col, n_row, tf)
