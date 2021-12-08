## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width=8,fig.height=4
)
library(CSHShydRology)
CAN05AA008 <- CAN05AA008

## -----------------------------------------------------------------------------
daily_flows <- CAN05AA008[, c(3, 4)]
result1 <- ch_hydrograph_plot(flows = daily_flows, winter_shading = FALSE)
result2 <- ch_hydrograph_plot(flows = daily_flows, winter_shading = TRUE)

## -----------------------------------------------------------------------------
myprd <- "2000-01-01/2000-12-31"
result3 <- ch_hydrograph_plot(
  flows = daily_flows, winter_shading = TRUE,
  prd = myprd
)

## -----------------------------------------------------------------------------
precip <- data.frame("Date" = daily_flows$Date, "precip" = abs(rnorm(nrow(daily_flows))) * 10)
result4 <- ch_hydrograph_plot(
  flows = daily_flows, precip = precip, winter_shading = TRUE,
  prd = myprd
)

## -----------------------------------------------------------------------------
result5 <- ch_hydrograph_plot(
  flows = daily_flows, precip = precip, winter_shading = TRUE,
  prd = myprd, range_mult_precip = 2, range_mult_flow = 1.8
)

## -----------------------------------------------------------------------------
ylab <- expression(paste("Discharge [m"^"3", "/s]"))
result6 <- ch_hydrograph_plot(
  flows = daily_flows, precip = precip,
  prd = myprd, ylabel = ylab
)

## -----------------------------------------------------------------------------
ylab_precip <- "Rainfall [mm]"
result7 <- ch_hydrograph_plot(
  flows = daily_flows, precip = precip,
  prd = myprd, precip_label = ylab_precip
)

## -----------------------------------------------------------------------------
result8 <- ch_hydrograph_plot(
  flows = daily_flows, precip = precip,
  prd = myprd, leg_pos = "right"
) # change legend to the right side
result9 <- ch_hydrograph_plot(
  flows = daily_flows, precip = precip,
  prd = myprd, leg_box = TRUE
) # add legend fill and outline
result10 <- ch_hydrograph_plot(
  flows = daily_flows, precip = precip,
  prd = myprd, zero_axis = F
) # default plot outside of function with buffer around axis

