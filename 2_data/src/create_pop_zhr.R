# create pop_zhr_panel.R
require(reshape2)

# load data & select wanted elements
pop.zhr.r <- read.csv2(file.path("raw", "pop", "zhr", "pop_zhr.csv"))
pop.panel.zhr <- data.frame(region = pop.zhr.r$GEBIET_NAME, 
                            time = pop.zhr.r$INDIKATOR_JAHR, 
                            variable = "pop",
                            value = pop.zhr.r$INDIKATOR_VALUE)

# change region name for Canton of zurich to "Kanton Zuerich" ------------------
pop.panel.zhr$region <- as.character(pop.panel.zhr$region)
pop.panel.zhr$region[pop.panel.zhr$region == "Zürich - ganzer Kanton"] <- 
  "Kanton Zuerich"

# interpolate quarterly values -------------------------------------------------
  #bring out individual variable ts
  pop.panel.zhr <- dcast(pop.panel.zhr, formula = region + time ~ ...) 
  # new quarterly time resolution
  t.start <- as.numeric(min(pop.panel.zhr$time)) + 1
  t.end <- as.numeric(max(pop.panel.zhr$time)) + 1
  temp.time <- seq(t.start, t.end, 0.25)
  # interpolate, for each region, probably could use apply.
  temp.pop.panel.q <- data.frame()
  for(i in unique(pop.panel.zhr$region)){
    temp.subset <- subset(pop.panel.zhr, region == i)
    temp.current <- data.frame(region = rep(i, length(temp.time)),
                               time = temp.time) 
    temp.current$pop <- spline(x = temp.subset$time + 1,  #+1 to match temp.time
                               y = temp.subset$pop,
                               xout = temp.time,
                               method="fmm")$y
    temp.pop.panel.q <- rbind(temp.pop.panel.q, temp.current)
  }
  pop.panel.zhr.q <- melt(temp.pop.panel.q, id.vars = c("region", "time"))
  
# plot panels ------------------------------------------------------------------  
  PlotPanelPDF(pop.panel.zhr.q, f = "pop_panel_zhr_q.pdf", mfrow = c(2,2))
# end / save & clean -----------------------------------------------------------
  save(pop.panel.zhr.q, file = file.path("raw", "pop_panel_zhr_q.RData"))
  rm(t.start, t.end, temp.time, temp.pop.panel.q, i, temp.current, temp.subset)
  rm(pop.zhr.r, pop.panel.zhr)
