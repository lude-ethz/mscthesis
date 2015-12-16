require(grid)

Plot.Coef.Comparison <- function(coef.output, pdf.name = NULL, 
                                  highlight = 0){
  # National Model visual evaluation
  #  run a national model and display some visualsfor graphical interpretation
  #  1./ with homogeneous restrictions: check dfe.result
  #       1.1 plot fitted price for each region over the actual (ggplot + facet + line)
  #  2./ with heterogeneous coefficients: 
  #       2.1 overlay coefficients for each region (no homogeneity restriction) 
  #           with group meaned coefficients (with homogeneity restriction)
  #       2.2 plot 

## 0 check args ----------------------------------------------------------------
  P <- coef.output$args$P
  Q <- coef.output$args$Q
  model.factors <- paste(coef.output$args$model.factors, collapse = ", ")
  if(is.null(pdf.name)){
    pdf.name <- paste(format(Sys.time(), "%Y%m%d_%H%M%S"), "_CoefComp", 
                      "_p",P , "_q",Q, "_idvar", ncol(coef.output$reg.in)-3,
                      ".pdf",
                      sep ="")
    if(dir.exists(file.path("output", "plots")))
      pdf.name <- paste(file.path("output", "plots",pdf.name))
  }
    
## 1 no heterogeneity ----------------------------------------------------------

# TODO


## 2 with heterogeneity: --------------------------------------------------------
# group mean estimator (using pmg function of "plm" package)  

  indcoef <- melt(coef.output$reg.out$details$gm.pmg$indcoef)
  meancoef <- melt(coef.output$reg.out$details$gm.pmg$coefficients)
  meancoef <- data.frame(Var1 = rownames(meancoef),
                        Var2 = "GroupMean",
                        value = meancoef)
  allcoef <- rbind(indcoef, meancoef)
  
  gg.allcoef <- ggplot(allcoef , aes(x = Var1, y = value))
  p1 <- (gg.allcoef 
       + geom_boxplot(data = subset(gg.allcoef$data, Var2 != "GroupMean"))
       + geom_vline(xintercept=highlight, alpha = 0.2, size = 3, color = "blue")
       + geom_point(data = subset(gg.allcoef$data, Var2 != "GroupMean"), aes(color = Var2), shape = 8) # plot region specific ones
       + geom_point(data = subset(gg.allcoef$data, Var2 == "GroupMean"), color = "red", size = 2, shape = 19) # plot group mean
       + ggtitle(paste("Coefficient comparison for \nP=",P," Q=", Q,"\nFactors =",model.factors))
       + labs(x= "Factor", y = "Coefficient Amplitude")
       + theme(axis.text.x = element_text(angle = 90, hjust = 1), 
               plot.margin = unit(c(0,0,0,0),"mm"))
       )
  
  p2 <- (gg.allcoef 
       + geom_boxplot(data = subset(gg.allcoef$data, Var2 != "GroupMean")) 
       + geom_vline(xintercept=highlight, alpha = 0.2, size = 3, color = "blue")
       + geom_point(data = subset(gg.allcoef$data, Var2 != "GroupMean"), aes(color = Var2), shape = 8) # plot region specific ones
       + geom_point(data = subset(gg.allcoef$data, Var2 == "GroupMean"), color = "red", size = 2, shape = 19) # plot group mean
       + coord_cartesian(ylim = c(-1000,1000))
       + ggtitle(paste("Coefficient comparison for \nP=",P," Q=", Q,"\nFactors =",model.factors))
       + labs(x= "Factor", y = "Coefficient Amplitude")
       + theme(axis.text.x = element_text(angle = 90, hjust = 1), 
               plot.margin = unit(c(0,0,0,0),"mm"))
       )
  
  p3 <- (gg.allcoef 
         + geom_boxplot(data = subset(gg.allcoef$data, Var2 != "GroupMean")) 
         + geom_vline(xintercept=highlight, alpha = 0.2, size = 3, color = "blue")
         + geom_point(data = subset(gg.allcoef$data, Var2 != "GroupMean"), aes(color = Var2), shape = 8) # plot region specific ones
         + geom_point(data = subset(gg.allcoef$data, Var2 == "GroupMean"), color = "red", size = 2, shape = 19) # plot group mean
         + coord_cartesian(ylim = c(-250,250)) 
         + ggtitle(paste("Coefficient comparison for \nP=",P," Q=", Q,"\nFactors =",model.factors))
         + labs(x= "Factor", y = "Coefficient Amplitude")
         + theme(axis.text.x = element_text(angle = 90, hjust = 1), 
                 plot.margin = unit(c(0,0,0,0),"mm"))
         )
  p4 <- (gg.allcoef 
         + geom_boxplot(data = subset(gg.allcoef$data, Var2 != "GroupMean")) 
         + geom_vline(xintercept=highlight, alpha = 0.2, size = 3, color = "blue") # highlight significant coeffs
         + geom_point(data = subset(gg.allcoef$data, Var2 != "GroupMean"), aes(color = Var2), shape = 8) # plot region specific ones
         + geom_point(data = subset(gg.allcoef$data, Var2 == "GroupMean"), color = "red", size = 2, shape = 19) # plot group mean
         + coord_cartesian(ylim = c(-50,50))
         + ggtitle(paste("Coefficient comparison for \nP=",P," Q=", Q,"\nFactors =",model.factors))
         + labs(x= "Factor", y = "Coefficient Amplitude")
         + theme(axis.text.x = element_text(angle = 90, hjust = 1), 
                 plot.margin = unit(c(0,0,0,0),"mm"))
         )
  
  pdf(file = pdf.name, paper = "a4r")
    plot(p1)
    plot(p2)
    plot(p3)
    plot(p4)
    
  dev.off()
}