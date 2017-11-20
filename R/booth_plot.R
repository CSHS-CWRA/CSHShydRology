#' Booth Plot
#'
#' A Booth plot is a plot of peaks over threhold flood events with duration on the horizontal and 
#' either magnitude (default) or volume on the vertical axis
#'
#' @param events : a dataframe from get_peaks 
#' @param threshold : the threshold used in get_peaks
#' @param title : uses format of "05BB001 - Bow River at Banff AB*" obtained from get_WSCstation 
#' @param type either 'mag' [magnitude default] or 'vol' [volume]
#'
#' @references Booth, E.G., Mount, J.F., Viers, J.H. 2006. Hydrologic Variability of the Cosumnes River Floodplain. #' San Francisco Estuary & Watershed Science 4:21.
#' @references Whitfield, P.H., and J.W. Pomeroy. 2016. Changes to flood peaks of a mountain river: implications #' for analysis of the 2013 flood in the Upper Bow River, Canada. Hydrological Processes 30:4657-73. doi: #' 10.1002/hyp.10957.

#'
#' @export 
#' 
#'
#' 
#' @examples
#' \dontrun{test <-booth_plot(events, threshold, title, type='mag')}
#' \dontrun{test <-booth_plot(events, threshold, title, type='vol')}
#' 
#
############################################


booth_plot <- function (events, threshold, title, type='mag') {
  
  #set common items
  mname <-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  ocol <- c("black","blue","darkgreen","black","blue","darkgreen", "black","blue","darkgreen","black","blue","darkgreen")
  mcol <- c("gray10","blue","slateblue3","slateblue4","green", "cyan","green4","darkorange","red","darkorange4","gray70","gray40")

  xlabel="Duration (days)"
  xlines<- c(7,14,21,60)
  xlimits <- c(1,350)

vlabel=expression(paste("Event volume km" ^{3}))
vlines <-c(.01, .02, .05,.1,.2,.5,1.,2.,5.,10.,20.,50.,100.,200.,500.,1000.,2000.,5000., 10000.)

ylabel=expression(paste("Mean Daily Discharge m" ^{3}, "/sec"))
ylines <-c(.1,.2,.5,1.,2.,5.,10.,20.,50.,100.,200.,500.,1000.,2000.,5000., 10000.)

month <- as.numeric(format(events$st_date, "%m"))

############################################################################   for volume
if (type=='vol') {
  ylimits <-c(min(events[,4],na.rm=TRUE),round(max(events[,4],na.rm=TRUE),digits=1))
  
  graphics::plot(events[,5],events[,4], xlab=xlabel,col=ocol[month],bg=mcol[month], pch=22, xlim=xlimits, ylim=ylimits, ylab=vlabel, 
       yaxt='n', log="xy",  main=title)
  graphics::abline(h=vlines, lty=3, col="gray50")
  graphics::abline(v=xlines, lty=3, col="gray50")
  graphics::axis(2,las=2)
  graphics::legend("topright",mname,pch=22,col=ocol, pt.bg=mcol, bg="white")
  graphics::mtext(paste("Threshold=",threshold," m3/s"),side=4,line=1)
}

############################################################################ for magnitude
if (type=="mag") {
  ylimits <-c(threshold,round(max(events[,3],na.rm=TRUE),digits=0))
  
  graphics::plot(events[,5],events[,3], xlab=xlabel,col=ocol[month],bg=mcol[month], pch=21, cex=1.1, xlim=xlimits, ylim=ylimits, ylab=ylabel, 
       yaxt='n', log="xy",  main=title)
  graphics::abline(h=ylines, lty=3, col="gray50")
  graphics::abline(v=xlines, lty=3, col="gray50")
  graphics::axis(2,las=2)    
  graphics::legend("topright",mname,pch=21,col=ocol, pt.bg=mcol, bg="white")
  Graphics::mtext(paste("Threshold=",threshold," m3/s"),side=4,line=1)
  
}
############################################################################
}
