# 
# is.NullOb <- function(x) is.null(x) | all(sapply(x, is.null))
# 
# rmNullObs <- function(x) {
#   x <- Filter(Negate(is.NullOb), x)
#   lapply(x, function(x) if (is.list(x)) rmNullObs(x) else x)
# }
# 
# df2 <- pdata.frame(df)
# df3 <- df2[df2$region %in% unique(df2$region)[1:2],]
# df3 <- df3[,1:4]
# df4 <-data.frame(rmNullObs(split(df$price_hedo_avg, df$region)))
# 
# panel.ur.test.llc <- purtest(object = df4,
#                              data = df4,
#                              test = "levinlin",
#                              exo = "intercept",  # exo variable to introduce  in DF regressions, [none, intercept, trend])
#                              lags = "AIC",  # criterion for optimal length selection 
#                              pmax = 8  # max lag periods
#                              # q = 
#                              # dfcor = 
#                              # fixedT
#                              )
# 
# panel.ur.test.mw <- purtest(object = df4,
#                              data = df4,
#                              test = "madwu",
#                              exo = "intercept",  # exo variable to introduce  in DF regressions, [none, intercept, trend])
#                              lags = "AIC",  # criterion for optimal length selection 
#                              pmax = 8  # max lag periods
#                              # q = 
#                              # dfcor = 
#                              # fixedT
#                              )
# 
# panel.ur.test.ips <- purtest(object = df4,
#                             data = df4,
#                             test = "ips",
#                             exo = "intercept",  # exo variable to introduce  in DF regressions, [none, intercept, trend])
#                             lags = "AIC",  # criterion for optimal length selection 
#                             pmax = 8  # max lag periods
#                             # q = 
#                             # dfcor = 
#                             # fixedT
#                             )
# 
# panel.ur.test.hadri <- purtest(object = df4,
#                              data = df4,
#                              test = "hadri",
#                              exo = "intercept",  # exo variable to introduce  in DF regressions, [none, intercept, trend])
#                              lags = "AIC",  # criterion for optimal length selection 
#                              pmax = 8  # max lag periods
#                              # q = 
#                              # dfcor = 
#                              # fixedT
# )
# 
# # save.image(file = "alldata.RData")
# # rm(list=ls())
# # load("alldata.RData")
# 
# 
# testpanel <- dcast(price.hedo, region + time ~ ...)
# testpanel <- pdata.frame(testpanel, c("region", "time"))
# 
# lagged <- lag(testpanel[,3], 1)
# dif <- diff(testpanel[,3], 1)
# testpanel$l1 <- lagged
# 
# 
# # test matrix element wise multiplication vs standard matrix multiplication
# # A <- matrix(1:21, nrow = 3)
# # B <- matrix(11:20, nrow = 2)
# # A%*%t(B)
# # A*B
# 
# 
# plot(fitted.values(my.coef$details$gm.pmg), type = "l")
# lines(my.coef$details$gm.pmg$model$`diff(log(y))`, col = "red")
# fitted.y <- df$price_hedo_avg[1] + cumsum(my.coef$details$gm.pmg$fitted.values)
# plot(fitted.y, type ="l")
# plot(df$price_hedo_avg, col = "red", type = "l")
# lines(my.coef$details$gm.pmg$fitted.values*10000, col = "blue")
# 
# 
# # check coefficient significance between: nominal, real, real-normed
# 
# ## 20151113
# x <- seq(-10,10, 0.1)
# 
# par(mfrow = c(2,1))
# 
# plot(x,dnorm(x), type = "l", ylim = c(0,1))
# lines(x,dnorm(x, mean = 1), type ="l", col ="red")
# #lines(x,dunif(x), type ="l", col ="blue")
# 
# qqnorm(rlnorm(x))
# qqline(rlnorm(x), col = "black")
# qqline(rnorm(x), col = "red")


## 20151115
  rm(list=ls())
  WD <- "D:/GoogleDrive/Work/MSc_Thesis/2_data"
  setwd(WD)
  source(file.path("src", "fun_model_national.R"))
  source(file.path("src", "fun","fun_PlotCoefComparison.R"))
  
  # all factors
  r.5 <- National.Model(P = 5 , Q = 5, load.data = T, plot.input = TRUE)
  r.4 <- National.Model(P = 4 , Q = 4, load.data = F)
  r.3 <- National.Model(P = 3 , Q = 3, load.data = F)
  r.2 <- National.Model(P = 2 , Q = 2, load.data = F)
  Plot.Coef.Comparison(r.5, highlight = c(1,2,3,6,7,8,26,27))
  Plot.Coef.Comparison(r.4, highlight = c(1,2,3,6,7,8,10,11,12,13,23))
  Plot.Coef.Comparison(r.3, highlight = c(1,2,3,6,7,20,21))
  Plot.Coef.Comparison(r.2, highlight = c(1,2,3,6,7,11,14))
  
  r.p4 <- National.Model(P = 4, Q = 5, load.data = F)
  r.p3 <- National.Model(P = 3, Q = 5, load.data = F)
  r.p2 <- National.Model(P = 2, Q = 5, load.data = F)
  r.p1 <- National.Model(P = 1, Q = 5, load.data = F)
  Plot.Coef.Comparison(r.p4, highlight = c(1,2,6,7,8,18,26))
  Plot.Coef.Comparison(r.p3, highlight = c(1,2,6,7,8,17))
  Plot.Coef.Comparison(r.p2, highlight = c(1,2,6,7,14,17,19))
  Plot.Coef.Comparison(r.p1, highlight = c(1,2,6,7,8,18,26))
  
  r.q4 <- National.Model(P = 5, Q = 4, load.data = F)
  r.q3 <- National.Model(P = 5, Q = 3, load.data = F)
  r.q2 <- National.Model(P = 5, Q = 2, load.data = F)
  r.q1 <- National.Model(P = 5, Q = 1, load.data = F)
  Plot.Coef.Comparison(r.q4, highlight = c(1,2,3,6,7,24))
  Plot.Coef.Comparison(r.q3, highlight = c(1,2,5,6,21,22))
  Plot.Coef.Comparison(r.q2, highlight = c(1,2,5,10,18))
  Plot.Coef.Comparison(r.q1, highlight = c(1,2,5,10))
  
  r.p1.q5 <- r.p1
  r.p1.q4 <- National.Model(P = 1, Q = 4, load.data = F)
  r.p1.q3 <- National.Model(P = 1, Q = 3, load.data = F)
  r.p1.q2 <- National.Model(P = 1, Q = 2, load.data = F)
  r.p1.q1 <- National.Model(P = 1, Q = 1, load.data = F)
  Plot.Coef.Comparison(r.p1.q4, highlight = c(1,2,6,10,20))
  Plot.Coef.Comparison(r.p1.q3, highlight = c(1,2,6,10,11,12,17,18))
  Plot.Coef.Comparison(r.p1.q2, highlight = c(1,2,3,6,14))
  Plot.Coef.Comparison(r.p1.q1, highlight = c(1,2,3,5,6))
  
  #no hstock
  indep.vars <- c(T,T,F,T)
  Logs <- c(T,T,T,F)
  r.ns5 <- National.Model(P = 5 , Q = 5, load.data = F, indep.vars = indep.vars, Logs = Logs)
  r.ns4 <- National.Model(P = 4 , Q = 4, load.data = F, indep.vars = indep.vars, Logs = Logs)
  r.ns3 <- National.Model(P = 3 , Q = 3, load.data = F, indep.vars = indep.vars, Logs = Logs)
  r.ns2 <- National.Model(P = 2 , Q = 2, load.data = F, indep.vars = indep.vars, Logs = Logs)
  Plot.Coef.Comparison(r.ns5, highlight = c(1,2,3,5,10,11,12,18,21,22))
  Plot.Coef.Comparison(r.ns4, highlight = c(1,2,3,5,9,10,11,12,18,19))
  Plot.Coef.Comparison(r.ns3, highlight = c(1,2,3,5,11,12,13,15,16))
  Plot.Coef.Comparison(r.ns2, highlight = c(1,2,3,5,10,12))
  
  #no income
  # indep.vars <- c(T,F,T,T)
  # Logs <- c(T,T,T,F)
  # r.ni5 <- National.Model(P = 5 , Q = 5, load.data = F, indep.vars = indep.vars, Logs = Logs)
  # r.ni4 <- National.Model(P = 4 , Q = 4, load.data = F, indep.vars = indep.vars, Logs = Logs)
  # r.ni3 <- National.Model(P = 3 , Q = 3, load.data = F, indep.vars = indep.vars, Logs = Logs)
  # r.ni2 <- National.Model(P = 2 , Q = 2, load.data = F, indep.vars = indep.vars, Logs = Logs)
  save.image("Report_Data.Rdata")

# 
# # 20151116 - descriptive plotting
# # price data
# require(ggplot2)
# require(scales)
# gg.price <- ggplot(price.hedo, aes(time, value, color = region))
# gg.price + facet_wrap("variable", nrow = 3) +
#  geom_line( alpha = 1/3) + geom_line(data = price.hedo[price.hedo$region == "Schweiz",], colour = "red", size = 1)
# 
# #hstock
# gg.hstock <- ggplot(hstock.panel, aes(time, value, color = region))
# gg.hstock + geom_line(data = subset(hstock.panel,hstock.panel$region != "Schweiz")) + labs(x = "Quarters", y = "No. of dwelling")
# 
# gg.inc <- ggplot(inc.panel, aes(time, value, color = region))
# plot(gg.inc 
#   + facet_wrap(facets = "variable") 
#   + geom_line() + labs(x = "Quarters", y = "Income") 
#   + scale_y_continuous(labels = comma)
#   + ggtitle("Income Panel Dataset")
# )

# 20151118
# backing out dfe fitted values
# y<-(r.5$reg.out$details$dfe.result$model[[1]])
# y.resid <- r.5$reg.out$details$dfe.result$residuals
# yhat <- y - y.resid # these are fitted vals, log-ed and differenced
# yhat.backed <- NA # TODO
  
# 20151120
# trying to reconstruct pmg values for r squared
  r.5.model <- as.data.frame(r.5$reg.out$details$gm.pmg$model)
  r.5.model <- r.5.model[,-1]  # remove Y
  r.5.coef <- r.5$reg.out$details$gm.pmg$coefficients
  r.5.coef[1] <- 0  # variant without intercept
  yhat<-apply(r.5.model, 1, function(x){sum(x * r.5.coef)})
  
  r.5.fitted <- r.5$reg.out$details$gm.pmg$fitted.values1
  r.5.y <- r.5$reg.out$details$gm.pmg$model[,1]
  comp <- data.frame(r.5.y, r.5.fitted, yhat)
  
  View(comp)
  
# 20151123 check resid vs index for any pattern
  df <- data.frame(
    region = sort(rep(1:12, times = 44)),
    idx = rep(1:44, times = 12),
    res = r.5$reg.out$details$gm.pmg$residuals
    )
  gplot<-ggplot(df, aes(x= idx,y= res, color = region))
  gplot + 
    geom_point() + 
    facet_wrap(facet = "region") + 
    geom_hline(aes(yintercept = 0)) + 
    ggtitle("Residuals over time index for each region")
  ggsave(filename = "output/plots/20151123_residual_vs_time/Plot_residual_vs_time_per_region.pdf")
  
  
  #x <- matrix(rnorm(99), ncol = 3, byrow = TRUE)
  x <- matrix(rnorm(66), ncol = 2)
  x <- cbind(sort(rep(1:3,11)),rep(1:11,3),x)
  x <- pdata.frame(data.frame(x))
  colnames(x) <- c("region", "time", "x1", "x2")
  
  Name.Indepvars <- c("x1", "x2")
  Q <- 4
  for (i in Name.Indepvars){
    for(j in 1:Q){
      x[[paste(i,".d",sep="")]] <- diff(x[[i]],1)
      x[[paste(i,".d.l",j, sep="")]] <- lag(x[[paste(i,".d", sep="")]], j)
    }   
  }
  
  x$x <- lag(x[["X3"]], 2)
  x$x <- diff(x[["X3"]], 1)
  x$X3.dl2 <- diff(x[["X3.l2"]],1)

  # 02.Dec.2015 - this part expanded into test_fun_pLogL.R; a test for my implementation of the panel log likelihood test
#   rm(list=ls())
#   WD <- "D:/GoogleDrive/Work/MSc_Thesis/2_data"; setwd(WD)
#   source(file.path("src", "fun_model_national.R"))
#   source(file.path("src", "model_national_graphical_eval.R"))
#   
#   r.5 <- National.Model(P = 5 , Q = 5, load.data = T)  
#   
#   art.in <- list()
#   art.in$reg.in <- matrix(data = c(rep(1:2,each = 10), rep(1:10,2), # region,time
#                             c((1:10)^(1),(1:10)^(1)) + rnorm(20), # y
#                             rep(-4:5, 2) + rnorm(20),  # x1
#                             seq(5,6, length.out = 20)), # x2
#                           nrow = 20,
#                           dimnames = list(1:20, c("i", "t", "y", "x1", "x2")))
#   art.in$reg.in <- as.data.frame(art.in$reg.in)
#   art.in$args <- list(P = 5,
#                      Q = 5,
#                      model.factors = colnames(art.in$reg.in)[-c(1:3)],
#                      Logs = c(F,F))
#   rndm <- rnorm(20)
#   art.in$reg.out <- list(
#     details = list(
#       dfe.result = NA,
#       gm.pmg = list(
#         fitted.values = art.in$reg.in$y + rndm,
#         residuals = rndm,
#         coefficients = 
#         rnorm(1+
#               art.in$args$P+
#               length(art.in$args$model.factors)*art.in$args$Q)),
#       gm.pvcm = NA))