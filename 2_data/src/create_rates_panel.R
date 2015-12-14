# create rates panel, 
  ###
  # CURRENTLY: keeping only 3mCHFLibor
  ###
rates.r.nat.m <- as.data.frame(read.csv(file.path("raw", "rates", "rates.csv")))
rates.r.nat.m <- as.xts(
  rates.r.nat.m[, 2:dim(rates.r.nat.m)[2]], 
  order.by = as.Date(rates.r.nat.m$Date, origin="1899-12-30"))

# convert to quarterly value
rates.r.nat.q <- rates.r.nat.m[endpoints(rates.r.nat.m, "quarters")]
rates.r.nat.q <- rates.r.nat.q["1989/"]
  # use quarterly average instead of quarter end value?

# create decimal representation of quarters
temp.time <- data.frame(y = as.numeric(format(index(rates.r.nat.q), "%Y")),
                        q = as.vector(quarters(index(rates.r.nat.q))))
t <- apply(temp.time, 1, function(x){ 
                          r <- as.numeric(x[1])
                          q <- as.character(x[[2]]); 
                          if(q == "Q1") r <- r + 0.25
                          if(q == "Q2") r <- r + 0.50
                          if(q == "Q3") r <- r + 0.75
                          if(q == "Q4") r <- r + 1.00
                          r})

# create panel data
rates.panel <- data.frame(region = rep("Schweiz", nrow(rates.r.nat.q)),
                          time = t,
                          rates.r.nat.q,
                          row.names = 1:nrow(rates.r.nat.q))

rates.panel <- melt(rates.panel, id.vars = c("region", "time"))

###
# CURRENTLY: keeping only 3mCHFLibor
rates.panel <- rates.panel[rates.panel$variable == "X3mCHFL",]
###




# create series for each region
districts.to.keep <- as.data.frame(read.csv(file.path("raw", "Districts.csv")))
n.districts <- length(districts.to.keep[,1])
temp.rows.to.repeat <- rep(1:length(unique(rates.panel$time)), n.districts)
temp.newpanel <- rates.panel[temp.rows.to.repeat,]
n.periods <- length(unique(rates.panel$time))
temp.rows.to.repeat <- rep(1:length(districts.to.keep[,1]), each = n.periods)
temp.newpanel$region <- districts.to.keep[temp.rows.to.repeat,1]
rates.panel <- temp.newpanel

rm(temp.rows.to.repeat, temp.newpanel, n.districts, n.periods)


# end
save(rates.panel, file="raw/rates.RData")
rm(rates.r.nat.q, rates.r.nat.m, temp.time, t)

