# Transaction numbers, ZHR 1990-2014 -------------------------------------------


setwd("D:/GoogleDrive/ETH/2015_FS/MSc_Thesis/2_data")



f <- "./raw/transactions/transactions_zhr_1990_2014.csv"
sales <- read.csv(f) 

sales_desc <- dcast(data = sales, 
                    INDIKATOR_NAME ~ SET_NAME, 
                    value.var = "INDIKATOR_ID", 
                    fun = first)

# transactions by district and property type
sales_type <- dcast(data = sales, 
                    INDIKATOR_JAHR ~ GEBIET_NAME+INDIKATOR_ID,
                    value.var = "INDIKATOR_VALUE")

# transactions by district
sales_total <- dcast(sales, 
                     INDIKATOR_JAHR ~ GEBIET_NAME, 
                     value.var = "INDIKATOR_VALUE", 
                     sum)

#transaction in canton of zhr
sales_total_zhr <- dcast(data = sales, 
                    INDIKATOR_JAHR ~ INDIKATOR_ID,
                    value.var = "INDIKATOR_VALUE",
                    sum)
colnames(sales_total_zhr) <- c("year","EFH", "MFH", "SWE")

sales_total_zhr$total <- rowSums(sales_total_zhr)


# plot/visualize to PDF file
pdf(file = file.path("plots", "transaction_volume_ZHR_1990_2014.pdf"))
  # plot zurich total
  
  plot(x = sales_total_zhr$year,
       y = sales_total_zhr$total,
       type = "o",
       col = "red",
       main = paste("Transaction Volume in canton of ZHR"),
       xlab = "Year of observation",
       ylab = "Number of transactions pa",
       ylim = c(0, max(sales_total_zhr$total*1.1)),
       lwd = 1.5)
  
  lines(x = sales_total_zhr$year,
        y = sales_total_zhr$EFH,
        lty = 2)
  
  lines(x = sales_total_zhr$year,
        y = sales_total_zhr$SWE,
        lty = 3)
  
  lines(x = sales_total_zhr$year,
        y = sales_total_zhr$MFH,
        lty = 4)
  
  legend(x = min(sales_total_zhr$year),
         y = max(sales_total_zhr$total)*1.11,
         legend = c("All bldg","EFH: Einfamilienhaus","SWE: Stockwerkeigentum","MFH: Mehrfamilienhaus"),
         lty = 1:4,
         col = c("red", "black", "black", "black"))

  # plot each district
  for(i in (2:13)){
    
    text(c(0,-10), labels = "test text")
    
    plot(x = sales_total$INDIKATOR_JAHR,
         y = sales_total[, i],
         type = "o",
         col = "red",
         main = paste("Transaction Volume in \n", colnames(sales_total)[i], sep = ""),
         xlab = "Year of observation",
         ylab = "Number of transactions pa",
         ylim = c(0, max(sales_total[, i]*1.1)),
         lwd = 1.5)
    
    lines(x = sales_type$INDIKATOR_JAHR,
          y = sales_type[, (i-1)*3-1],
          lty = 2)
    
    lines(x = sales_type$INDIKATOR_JAHR,
          y = sales_type[,(i-1)*3],
          lty = 4)
    
    lines(x = sales_type$INDIKATOR_JAHR,
          y = sales_type[,(i-1)*3+1],
          lty = 3)
    
    legend(x = min(sales_type$INDIKATOR_JAHR),
           y = max(sales_total[,i]),
           legend = c("All bldg","EFH","SWE","MFH"),
           lty = 1:4,
           col = c("red", "black", "black", "black"))
  }
dev.off()
#par(parold)

#sum zurich total

