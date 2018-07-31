## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

library(CSHShydRology)
HYDAT_list <- HYDAT_list
fig_file <- system.file("extdata", "04JD005_h.PNG", package = "CSHShydRology")


## ---- echo=TRUE----------------------------------------------------------
mfile <- system.file("extdata", "04JD005_Daily_Flow_ts.csv", package = "CSHShydRology")
mdata <- read.csv(mfile)
head(mdata)
str(mdata)
length(mdata$ID)
unique(mdata$ID)

## ---- echo=TRUE----------------------------------------------------------
mdata <- read_ECDE_flows(mfile)
head(mdata)
str(mdata)
length(mdata$ID)
unique(mdata$ID)

## ---- fig.width=6, fig.height=3------------------------------------------

knitr::include_graphics(fig_file)

## ---- echo=TRUE, fig.width=6, fig.height=5-------------------------------
md <- flow_raster(mdata)

## ---- echo=TRUE,fig.width=6, fig.height=5--------------------------------
md <- flow_raster_qa(mdata)

## ---- echo=TRUE,fig.width=6, fig.height=5--------------------------------
title <- get_wscstation("04JD005", stn = HYDAT_list)
title <- title[21]
mp <- regime_plot(mdata$Date, mdata$Flow, title, wyear = 1)
mp <- regime_plot(mdata$Date, mdata$Flow, title, wyear = 10)

fdc <- fdcurve(mdata$Flow, title)

## ---- echo=TRUE, fig.width=4, fig.height=5-------------------------------

range1 <- c(1980, 1989)
range2 <- c(1990, 1999)
mplot <- binned_MannWhitney(mdata, step = 5, range1, range2, ptest = 0.05)
str(mplot)
scol <- c("red", NA, "blue")
ylims <- c(0, max(mplot$series$period1, mplot$series$period2))
plot(mplot$series$period, mplot$series$period1, pch = 21, col = "blue", type = "b", ylim = ylims, main = mplot$StationID)
points(mplot$series$period, mplot$series$period2, pch = 21, col = "green", bg = scol[mplot$series$code], type = "b")

## ---- echo=TRUE,fig.width=10, fig.height=6-------------------------------

mp <- polar_plot(mplot)

## ---- echo=TRUE, fig.width=6, fig.height=6-------------------------------

tr <- flow_raster_trend(mdata$Date, mdata$Flow, step = 5, station = "04JD005")
tr <- flow_raster_trend(mdata$Date, mdata$Flow, step = 11, station = "04JD005")

## ---- echo=TRUE, fig.width=6, fig.height=5-------------------------------

cdata <- cut_block(mdata, "2000/01/01","2010/12/31")
tr <- flow_raster_trend(cdata$Date, cdata$Flow, step = 11, station = "04JD005")

