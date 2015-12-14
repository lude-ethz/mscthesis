# create_price_panel_hedonic.R
# create hedonic price index panel data

# get district names
csv.colnames <- read.csv(file.path("raw", "price", "hedonic", "colnames.csv"))


# apt prices -------------------------------------------------------------------
price.hedo.apt.r <- read.csv(file.path("raw", "price", "hedonic", "apt.csv"),
                             header = TRUE)
colnames(price.hedo.apt.r) <- csv.colnames[,1]
price.hedo.apt.r <- melt(price.hedo.apt.r, id.vars = "Quarter")
price.hedo.apt.r <- data.frame(region = price.hedo.apt.r[,2],
                               time = price.hedo.apt.r[,1],
                               variable = rep("price_hedo_apt", nrow(price.hedo.apt.r)),
                               value = price.hedo.apt.r[,3])

# house prices -----------------------------------------------------------------
price.hedo.house.r <- read.csv(file.path("raw", "price", "hedonic", "house.csv"),
                               header = TRUE)
colnames(price.hedo.house.r) <- csv.colnames[,1]
price.hedo.house.r <- melt(price.hedo.house.r, id.vars = "Quarter" )
price.hedo.house.r <- data.frame(region = price.hedo.house.r[,2],
                                 time = price.hedo.house.r[,1],
                                 variable = rep("price_hedo_house", nrow(price.hedo.house.r)),
                                 value = price.hedo.house.r[,3])

# merge apt and houses, create average -----------------------------------------
price.hedo <- rbind(price.hedo.house.r, price.hedo.apt.r)
price.hedo <- dcast(price.hedo, formula = region + time ~ ...)
price.hedo$price_hedo_avg <- apply(X = cbind(price.hedo$price_hedo_apt, price.hedo$price_hedo_house),
                                   MARGIN = 1,
                                   FUN = mean)
price.hedo <- melt(price.hedo, id.vars = c("region", "time"))

# optional plot
PlotPanelPDF(df = price.hedo,
             f = "price_hedonic.pdf",
             mfrow = c(3,1))

# end
rm(price.hedo.house.r, price.hedo.apt.r, csv.colnames)
