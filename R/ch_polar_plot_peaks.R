#'Polar / circular plots of peak flows
#' @description Polar / circular plots of peak flows.  
#'   Creates a polar plot of flow peaks in one of several different forms.  
#'    Basic plot has shading for nival and pluvial centroids.
#'
#' @param title a title to be added to the plot
#' @param direction  a value or array of mean/median direction, circular mean or 
#' median of points from ch_circ_mean_reg (optional)
#' @param regularity a value or array of regularity from ch_circ_mean_reg (optional).
#' @note points inside the plot
#' @param in_pch a value or an array of symbols to be used for centroids.  To be in color, 
#' must be one of 21 to 25 to get a symbol with border, elsewise a red symbol is plotted.
#' @param in_col an array of colors, either numbers or names to apply to centroid points (optional, 
#' default is "red")
#' @param in_cex an array of symbol sizes
#' @param in_detail an array of indices indicating symbol [1] shape, [2] colour, [3] background, 
#' and [4]size
#' @note in_pch, in_col, and in_cex will normally be of the same length and that would 
#' be the maximum index of in_detail
#' @param days an array of days of year to be plotted on perimeter (optional).
#' @param labels an array of labels to be placed beside points with direction and regularity (optional)
#' @param label_pos an array of positions indicating when label be placed (1, 2, 3, or 4 - below, left, 
#' above, right)(optional - default is below)
#' @param shading  if  \code{TRUE} adds shading and labels for nival and pluvial regimes default = \code{FALSE}
#' @param shade percentage of shading, default is 35.
#' @note points on the outside 
#' @param out_pch symbols for points on outside of circle
#' @param pt_col colour used for points for events. default = "darkblue". If pt_col is an array it is used to colour 
#' the individual points of days
#' @param out_cex point size for symbol
#' @param ... other plot options
#' 
#' @return Creates a circular plot of peak flows.
#'  
#' @references
#'  Pewsey, A., M. Neuhauser, and G. D. Ruxton. 2014. Circular Statistics in R, 
#'  192 pp., Oxford University Press.
#'  
#'  Whitfield, P. H. 2018. Clustering of seasonal events: A simulation study using 
#'  circular methods. Communications in Statistics - Simulation and Computation 47(10): 3008-3030.
#'  
#'  Burn, D. H., and P. H. Whitfield. 2023. Changes in the timing of flood events resulting 
#'  from climate change. Journal of Hydrology.
#' @export
#' @author Paul Whitfield 
#' @examples 
#' # base plot
#' ch_polar_plot_peaks() 
#' 
#' #base plot with area shading
#' ch_polar_plot_peaks(shading = TRUE)
#' 
#' # plot of annual maximum series
#' data(CAN05AA008)
#' am <- ch_sh_get_amax(CAN05AA008)
#' ch_polar_plot_peaks(days = am$doy, title = "05AA008")
#' 
#' #remove partial years
#' am <-am[am$days >= 365,]
#' ch_polar_plot_peaks(days = am$doy, title = "05AA008")
#' 
#' #plot the centroid
#' m_r <- ch_circ_mean_reg(am)
#' ch_polar_plot_peaks(direction = m_r$mean, regularity = m_r$regularity, title = "05AA008")
#' 
#' # plot peaks and centroid
#' ch_polar_plot_peaks(days = am$doy, direction = m_r$mean, regularity = m_r$regularity, 
#' title = "05AA008")

ch_polar_plot_peaks <- function(title = NA, direction = NULL, regularity = NULL, 
                                days = NULL, 
                                shading = FALSE, 
                                shade = 35, 
                                pt_col = "darkblue",
                                in_pch = NULL, 
                                in_cex = NULL, 
                                in_col = NULL, 
                                in_detail = NULL,
                                labels=NULL, 
                                label_pos = NULL,
                                out_pch= 16,  
                                out_cex = 0.8,  ...){
  
  oldw <- getOption("warn")
  options(warn = -1)
  
  opar <- par()
  on.exit(par(opar))
  on.exit(options(warn = oldw))
  
  par(cex.lab = 0.7)
  par(col.lab = "gray")
  
  if (is.null(label_pos)) label_pos <- rep(1,length(direction))
  if (is.null(in_col)) in_col <- rep("red", length(direction))
  if (is.null(in_pch)) in_pch <- rep(19, length(direction))
  if (is.null(in_cex)) in_cex <- rep(0.8, length(direction))
  
  ##### set some details 
  dlabels <- c("Jan","Feb","Mar","Apr", "May", "Jun", "Jul","Aug","Sep",
               "Oct","Nov","Dec", "")
  
  dbreaks <- c(1,32,60,91,121,152,182,213,244,274,305, 335, 366)
  dbreaks <-dbreaks/365*2*pi
  
  
  
  #######  
  if(!shading){
    
    out_a <-seq(dbreaks[4], dbreaks[8], length.out = 20)
    nival_r <-c(out_a,rev(out_a))
    nival_l  <- c(rep(1.0,20),rep(0.65,20))
    
    twhite <- ch_col_transparent("white", 50 )

    
    plotrix::radial.plot(nival_l, nival_r, rp.type = "p",
                         main = title,
                         radial.lim = c(0,1),
                         start = 3*pi/2,
                         clockwise = TRUE,
                         labels = dlabels,
                         label.pos = dbreaks,
                         label.prop = 1.15,
                         line.col = twhite,
                         poly.col = twhite)
  }
  #######  
  if (shading) {
    
    tblue  <- ch_col_transparent("blue", shade)
    tgreen <- ch_col_transparent("green", shade)

    
    out_a <- seq(dbreaks[4], dbreaks[8], length.out = 20)
    nival_r <- c(out_a,rev(out_a))
    nival_l  <- c(rep(1.0,20),rep(0.65,20))
    
    out_b <- seq(dbreaks[1], dbreaks[2], length.out = 40)
    pluvial_r1 <- c(out_b,rev(out_b))
    pluvial_l1  <- c(rep(1.0,40),rep(0.45,40))
    
    out_c <- seq(dbreaks[10], dbreaks[13], length.out = 40)
    pluvial_r2 <- c(out_c,rev(out_c))
    pluvial_l2  <- c(rep(1.0,40),rep(0.45,40))
    
    plotrix::radial.plot(nival_l, nival_r, rp.type = "p",
                         main = title,
                         radial.lim = c(0,1),
                         start = 3*pi/2,
                         clockwise = TRUE,
                         labels = dlabels,
                         label.pos = dbreaks,
                         line.col = tblue,
                         poly.col = tblue)
    
    text(0.1,0.86,"Nival", col="gray60",pos = 4)
    text(0.1,0.12,"Mixed", col="gray60",pos = 4)
    
    plotrix::radial.plot(pluvial_l1, pluvial_r1, rp.type = "p",
                         radial.lim = c(0,1),
                         start = 3*pi/2,
                         clockwise = TRUE,
                         line.col = tgreen,
                         poly.col = tgreen,
                         add = TRUE)
    
    
    plotrix::radial.plot(pluvial_l2, pluvial_r2, rp.type = "p",
                         radial.lim = c(0,1),
                         start = 3 * pi / 2,
                         clockwise = TRUE,
                         line.col = tgreen,
                         poly.col = tgreen,
                         show.grid = TRUE,
                         add = TRUE)
    
    text(0.1, -0.84,"Pluvial", col = "gray60",pos = 4)
    
  }
  
  # basic regularity plots with single symbol type and colour
  if (!is.null(direction) && !is.null(regularity) && is.null(in_detail)){
    
    
    direction <- direction / 365 * 2 * pi
    ptc <- rep("black",length(direction))
    if(max(in_pch <= 20)) ptc <- in_col
    
   
    plotrix::radial.plot(regularity,direction,
                         rp.type="s",
                         point.symbols = 22,
                         point.col = ptc,
                         bg=in_col,
                         
                         start=3*pi/2,
                         labels=dlabels,
                         label.pos=dbreaks,
                         clockwise=TRUE,
                         cex= in_cex,
                         add=TRUE)
  
  }
  
  # fancy regularity plots with single symbol type and colour
  if (!is.null(direction) && !is.null(regularity) && !is.null(in_detail)){

    direction <- direction / 365 * 2 * pi
    ptc <- rep("black",length(direction))
    if(max(in_pch <= 20)) ptc <- in_col
    
    plotrix::radial.plot(regularity,direction,
                         rp.type="s",
                         point.symbols = as.numeric(in_detail[,1]),
                         point.col = in_detail[,2],
                         bg = in_detail[,3],
                         cex = as.numeric(in_detail[,4]),
                         start=3*pi/2,
                         clockwise=TRUE,
                         radial.lim=c(0,1),
                         show.grid.labels=4,
                         show.grid = TRUE,
                         show.radial.grid = TRUE,
                         add=TRUE)
  }
  
  
  
  if(!is.null(labels)) {
    
    plotrix::radial.plot.labels(regularity, direction,  labels=labels, pos = label_pos,
                                start=3*pi/2, cex=0.7,
                                radial.lim=c(0,1),
                                clockwise=TRUE)
    
  }
  
  
  if(!is.null(days)){
    
    if(length(in_pch) == 1) out_pch <- rep(out_pch,length(days))
    if(length(in_cex) == 1) out_cex <- rep(out_cex,length(days))
    if(length(pt_col) == 1) pt_col <- rep(pt_col,length(days))
    # link days and point colours and sort into day order   
    # order days in ascending order
    adays <- data.frame(days, out_cex, out_pch, pt_col)
    adays <- adays[order(days),]
    
    # convert doy to radians
    days <- adays$days / 365 * 2 * pi
    id <- rep(1.005, length(days))
    
    for( ii in 2:length(days)){
      if (days[ii] == days[ii-1]) id[ii] <-id[ii-1] + 0.02
    }
    
    plotrix::radial.plot(id,days,
                         rp.type="s",
                         point.symbols = adays$out_pch,
                         point.col = adays$pt_col,
                         cex = adays$out_cex,
                         start = 3*pi/2,
                         labels = dlabels,
                         label.pos = dbreaks,
                         clockwise = TRUE,
                         radial.lim = c(0,1),
                         show.grid.labels = 4,
                         show.grid = TRUE,
                         show.radial.grid = TRUE,
                         add = TRUE)
  }
  
}
