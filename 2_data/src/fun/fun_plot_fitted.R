# requires:
# function: Reconstruct()

Plot.Fitted <- function(reg.in , reg.out, P = 5, Q = 5, 
                        plot.pdf = TRUE, pdf.name = NULL){
  # input
  # reg.in ... panel data frame with regression model input in _levels_
  #     reg.in = region | time | dep.var | indep.var1 | indep.var2 |..
  # reg.out ... output from fun_testcoeffhomogen.R
  # P = number of lags for dependent variable incorporated into reg.out
  # Q = number of lags for independent variable incorporated into reg.out
  
  if( plot.pdf == TRUE){
    if(is.null(pdf.name)){
      pdf.name <- paste(format(Sys.time(), "%Y%m%d_%H%M%S"), "_PlotFitted", 
                        "_p",P , "_q",Q, "_idvar", ncol(reg.in)-3,
                        ".pdf",
                        sep ="")
    }
  
    # check output filepath <WD>\output\plots\
    if(!dir.exists(file.path(getwd(),"output","plots"))){
      tmp<-dir.create(file.path("output","plots"), recursive = T)
      if(tmp){
        pdf.name <- file.path("output", "plots", pdf.name)  
        rm(tmp)
      } else {
        warning("<WD>/output/plots not available - printing directly into <WD> ")
      }
    } else {
      pdf.name <- file.path("output", "plots", pdf.name)
    }
  }
  # Plotting
    t.reg.count <- 0
    if(plot.pdf  == TRUE)
      pdf(file = pdf.name)
    #par(mfrow = c(2,2))  # <or>
    layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
    
    # begin plotloop
    for(i in unique(reg.in[,1])){
      t.reg.count <- t.reg.count + 1
      
      # create subset for region i
        z <- subset(reg.in[,1:3], region == i)
        colnames(z) <- c("i", "t", "y")
        z$logy <- log(z$y)
        z$logy_l <- c(NA, z$logy[2:nrow(z)-1])  # lagged
        z$logy_d <- z$logy - z$logy_l  # difference_actual
        z$logy_d_fitted <- 
          c(rep(0,5),reg.out$details$gm.pmg$fitted.values[(1:44)+44*(t.reg.count-1)])
        z$e_actual <- z$logy_d_fitted - z$logy_d
        z$e_fitted <- 
          c(rep(0, 5), reg.out$details$gm.pmg$residuals[(1:44)+44*(t.reg.count-1)])
        # z$e_check <- !as.logical(round(z$e_actual - z$e_fitted, 15), na.rm = T)
        z$logy_reconstr <- Reconstruct(z$logy[5], z$logy_d_fitted)[-1]
        z$y_reconstr <- exp(z$logy_reconstr)
        z$exp_e_actual <- z$y_reconstr - z$y
        # z$exp_e_fitted <- exp(z$e_fitted)
        # z$exp_e_check <- !as.logical(round(z$exp_e_actual -z$exp_e_fitted,0),na.rm=T
        
      # plot price fitted vs actual and errors
        plot(z$t, z$y, 
             col = "black", type = "l", 
             xlab = "Period", ylab = "Hedonic Price level", 
             ylim = c(50, 150),
             main = paste("Hedonic Price in ", i,"\nFitted Values (red) vs Actual Values (black)",
                          "\n Error(blue) on right axis in index points"))
        lines(z$t, z$y_reconstr,
              col = "red", type ="l")
      # add info about number of lags and variables
        text(x = min(z$t), y = min(50), 
             labels=paste("\nModel Params:\np =",P, ", q =", Q, 
                  "\nfactors =", paste(colnames(reg.in)[-(1:3)], collapse =", ")),
             adj = c(0,0))
      # add residual bars
        par(new = T)
        plot(z$t,z$exp_e_actual, 
             col = "blue", type ="h", 
             ylim = c(-11,+11),
             axes = F, xlab = "", ylab = "")
        axis(4, col = "black",lwd=1)

      # residual scatter plot
        plot(z$logy_d_fitted, z$e_fitted, xlim = c(-0.2, 0.4), main = "Residual of Fitted Values")
        lines(smooth.spline(z$logy_d_fitted, z$e_fitted, spar = 0.85), lty = 1, col = "red")
        abline(h = 0)
        boxplot(z$e_fitted, add = TRUE, at = -0.18, col = "gray", boxwex = 0.05, pch = 4)
        
      # qq-normal plot for residuals
        qqnorm(z$e_fitted, main = "Normal Q-Q for Residuals")
        qqline(z$e_fitted)  #
        # TODO: 
        # update to proper formatting (incl. norm line etc)
        # double check plotted series

    }
    if(plot.pdf == TRUE)
      dev.off()
  # -----
  # end plot loop ----
  rm(t.reg.count)
  
  if(plot.pdf == TRUE)
    cat("Plotted to file: plot.fitted()\n <WD>/", pdf.name, "\n")
}