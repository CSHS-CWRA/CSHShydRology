#' Raster pot with WSC quality flags
#' 
#' @description {
#' Produces a raster plot: years against day of year, showing all the data flags
#' A (Partial) green
#' B (Below Ice)  blue
#' D (Dry)  yellow
#' E (Estimated) red
#'}
#' @param dframe - a datafile from water survey [read.wsc]


#' @author Paul Whitfield <paul.h.whitfield@gmail.com>


#' Produces a gray scale plot of observations with colour symbol showing where data has a quality flag
#'
#' @export 
#' 
#' 
#' @examples
#' \dontrun{qaplot <-raster_qa("05AA008_Daily_flow_ts")}


###### librarys attached


raster_qa <- function( dframe) {


  ##### Fixed labels and text strings
  DOY <- "Day of Year"
  ylabelq=expression(paste("Discharge m" ^{3}, "/sec"))


  ########################################################  Discharge section

  rastercolours=c("gray90","gray80","gray70","gray60", "gray50", "gray40", "gray30", "gray20", "gray10")
  qcols=grDevices::colorRampPalette(rastercolours)



  station <- as.character(dframe$ID[1])
  sname<- get_wscstation(station)
  title=sname$Station_lname

  date <-as.Date(dframe$Date,"%Y/%m/%d")

  Year <- as.numeric(format(date,"%Y"))
  doy <- as.numeric(timeDate::dayOfYear(timeDate::as.timeDate(date)))

  mYear <-max(Year, na.rm=TRUE)
  nYear <-min(Year, na.rm=TRUE)-1
  Years <-mYear-nYear
 
  qdata <-array(dim=c(Years,366))
  flag <-array(dim=c(Years,366))

  dframe$SYM <-as.character(dframe$SYM)   #############    change flag codes to colours
  dframe$SYM[dframe$SYM=="A"] <-3
  dframe$SYM[dframe$SYM=="B"] <-4
  dframe$SYM[dframe$SYM=="D"] <-7
  dframe$SYM[dframe$SYM=="E"] <-2
  dframe$SYM <-as.numeric(dframe$SYM)   ##### convert to numbers for plotting colour

  inYear <-array(nYear:mYear)

  for (k in 1:length(dframe[,1])) {

    qdata[(Year[k]-nYear),doy[k]] <- dframe$Flow[k]
    flag[Year[k]-nYear,doy[k]] <- dframe$SYM[k]
    # 1 was no flag, 2 was "A", 3 was "B", 4 was "C", 5 was "D", 6 was "E"

  }
  flag[flag==1] <-NA

  qmax <-max(qdata, na.rm=TRUE)
  qmin <-min(qdata, na.rm=TRUE)


  ################################   raster map of daily flows


  qraster <- raster::flip(raster::raster(qdata, xmn=1,xmx=max(doy, na.rm=TRUE), ymn=min(Year,na.rm=TRUE), 
                                 ymx=max(Year,na.rm=TRUE)), direction='y')
  
  Fraster <- raster::flip(raster::raster(flag,  xmn=1,xmx=max(doy, na.rm=TRUE), ymn=min(Year,na.rm=TRUE), 
                                 ymx=max(Year,na.rm=TRUE)), direction='y')
  
  Araster <-raster::rasterToPoints(Fraster, fun=function(x) {x==3})
  Braster <-raster::rasterToPoints(Fraster, fun=function(x) {x==4})
  Draster <-raster::rasterToPoints(Fraster, fun=function(x) {x==7})
  Eraster <-raster::rasterToPoints(Fraster, fun=function(x) {x==2})

  ########################################################### start  plotting section
  graphics::par(oma=c(0,0,3,0))
  graphics::layout(matrix(c(1,1,1,1,2,1,1,1,1,3), 2, 5, byrow = TRUE))
  graphics::par(mar=c(4,4,1,1))


  #################################################################  panel one
  graphics::image(qraster, xlab=DOY, las=1, ylab="", las=1, col=qcols(9))

  graphics::points(Araster, pch=15, cex=.7, col="green")     ### colours conform to ECData Explorer
  graphics::points(Braster, pch=15, cex=.7, col="cyan")
  graphics::points(Draster, pch=15, cex=.7, col="yellow")
  graphics::points(Eraster, pch=15, cex=.7, col="red")

  graphics::box()
  #################################################################  panel two
  graphics::frame()
  graphics::par(mar=c(2,0,1,8))

  zr=c(min(qdata,na.rm=TRUE),max(qdata,na.rm=TRUE))
  ######### scale bar and legend

  fields::image.plot(legend.only=TRUE, zlim= zr, col=qcols(9), legend.width=3.5, legend.shrink=0.8,
             legend.args=list(text=ylabelq, side=2,line=2.5, cex=0.8))

  #################################################################  panel three (element #ten)
  graphics::frame()
  graphics::frame()
  graphics::frame()
  graphics::frame()
  graphics::frame()

  graphics::par(mar=c(0,0,0,0))
  leg.txt <-c(" (A) Partial", " (B) Ice", " (D) Dry"," (E) Estimate")
  lcol <-c("green","blue","yellow", "red")
  graphics::legend("topleft", leg.txt, pch=c(15,15,15,15), col=lcol, cex=1.25, bty='n')

  ##############################################################  Add title
  tscale=1.7
  if(nchar(title)>=45) tscale=1.5
  if(nchar(title)>=50) tscale=1.2
  graphics::mtext(title, side=3, line=0, cex=tscale,outer=TRUE)

  ############################################################### end plotting section

  return()
}

#######################################################################################



