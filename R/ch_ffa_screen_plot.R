#'  FFA screening plot
#'
#' @description
#' Generates a flood frequency plot with symbols indicating whether an observation is a
#' high or low outlier, and colour symbols indicating month of the year in which the flood
#' occurred.
#'
#' @param df dataframe with date of event [maxdate] and annual maximum [amax]
#' @param mtitle title for plot
#' @param stn stationID
#' @param mcol array of three colours, default is c("orange", "gray70", "red") use to outline or
#' emphasize low outliers, normal, and high outliers.
#' @param n default is 12, number of colours for number of months
#' @param m smallest angle in radians parameter for generating circular colours default = 0
#' @param M largest angle in radians parameter for generating circular colours default = 2 (*pi)
#' @param offset the zero in radians, default is 0.
#'
#' @return a list containing
#' \itemize{
#' \item Station if specified else "unspecified"
#' \item number of amax events
#' \item array of amax magnitudes
#' \item array of outlier codes (1 = low, 2 = not, 3 = high outlier)
#' }
#'
#' @import TeachingDemos
#' @importFrom MGBT MGBT
#' @importFrom graphics grconvertX grconvertY hist rect title
#' @importFrom stats lm qt
#' @export
#' @references Cohn, T. A., J. F. England, C. E. Berenbrock, R. R. Mason, J. R.
#' Stedinger and J. R. Lamontagne (2013). "A generalized Grubbs‐Beck test statistic
#' for detecting multiple potentially influential low outliers in flood series."
#' Water Resources Research 49(8): 5047-5058 10.1002/wrcr.20392: 10.1002/wrcr.20392.
#'
#'
#'@examples \donttest{
#'# Not tested automatically as can be very slow to execute
#' data(CAN05AA008)
#' amax <- ch_sh_get_amax(CAN05AA008)
#' ch_ffa_screen_plot(amax)}

ch_ffa_screen_plot <- function(df, stn= "unspecified", mtitle = "",
                                n = 12, m = 0, M = 2, offset = 0,
                      mcol = c("orange", "gray70", "red")){


  mon <- as.numeric(format(df$maxdate, "%m"))

  m3s <- expression(paste("m"^3,"/s"))

  result <- array(NA,31)

  mcode <- array(2, dim = length(df$amax))

  nevents <- length(df$amax)
  maxQ <- max(df$amax, na.rm = TRUE)


  mindex <- which.max(df$amax)



  maxDate <- format(df$maxdate[mindex],"%Y-%m-%d")
  mDoy <- df$doy[mindex]

  mmax <- sort(df$amax, decreasing = TRUE)

  mmax <- mmax[mmax < maxQ]

  #df0 <- df[df$amax < maxQ,]

######################### get Grubbs codes

############### high Grubbs


  gtest <- ch_high_Grubbs_test(df$amax)



  ############################## low Grubbs
  mg <- MGBT::MGBT(df$amax)

  gindex <- which(df$amax < mg$LOThresh)

  for (ll in 1: length(gindex)) {
    mcode[gindex[ll]] <- 1
  }

  for (ll in 1:length(mcode)) {
  if (gtest$tout[ll] == 1) mcode[ll] <- 3
}
  ########################################### plotting
  # Generate plotting positions
  Q <- df$amax
  n = length(Q)
  r = n + 1 - rank(Q)  # highest Q has rank r = 1
  T = (n + 1)/r

  mat0 <- pretty(0:maxQ)

  mpch <- c(22, 21, 24)
  mcex <- c(1.20, 0.9, 1.20)
  ccol <- ch_circular_colors(n = 12, m = m, M = M*pi)


  # Set up x axis tick positions and labels
  Ttick = c(1.001,1.01,1.1,1.5,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,25,30,35,40,45,50,60,70,80,90,100)
  xtlab = c(1.001,1.01,1.1,1.5,2,NA,NA,5,NA,NA,NA,NA,10,NA,NA,NA,NA,15,NA,NA,NA,NA,20,NA,30,NA,NA,NA,50,NA,NA,NA,NA,100)
  y = -log(-log(1 - 1/T))
  ytick = -log(-log(1 - 1/Ttick))
  xmin = min(min(y),min(ytick))
  xmax = max(ytick)


  # Fit a line by method of moments, along with 95% confidence intervals
  KTtick = -(sqrt(6)/pi)*(0.5772 + log(log(Ttick/(Ttick - 1))))
  QTtick = mean(Q) + KTtick*sd(Q)
  nQ = length(Q)
  se = (sd(Q)*sqrt((1 + 1.14*KTtick + 1.1*KTtick^2)))/sqrt(nQ)
  LB = QTtick - qt(0.975, nQ - 1)*se
  UB = QTtick + qt(0.975, nQ - 1)*se
  max = max(UB)
  Qmax = max(QTtick)

 par(mar = c(4,5,4,3))

    # Plot peak flow series with Gumbel axis
    plot(jitter(y,8), jitter(Q,8) ,
         ylab = expression( "Peak Flow ("*m^3*s^{-1}*")" ) ,
         xaxt = "n", xlab = "Return Period (yr) [Gumbel]",
         las  = 1,
         tcl = -0.35,
         ylim = c(0, max(Q)),
         xlim = c(xmin, xmax),
         cex = mcex[mcode],
         pch = mpch[mcode],
         col = mcol[mcode],
         bg = ccol[mon],
         main = "" )

    if (nchar(mtitle) <= 50) title(main = mtitle)
    if (nchar(mtitle) > 50) title(main = mtitle, cex.main = 0.95)

    axis(1, at = ytick, labels = as.character(xtlab), cex = 0.65)


    # Add fitted line and confidence limits
    lines(ytick, QTtick, col = "black")
    lines(ytick, LB, col = "black", lty = 3)
    lines(ytick, UB, col = "black", lty = 3)


      mline <- lm(QTtick ~ ytick)
      abline(lm(QTtick ~ ytick), col = "gray50", lwd = 1.5)


    abline(h = mat0, col = "gray50", lty = 2)

    vlines <- c(ytick[5], ytick[8], ytick[13], ytick[23], ytick[29], ytick[34])
    abline(v = vlines , col = "gray50", lty = 3)

    text(0.1, 0, paste( length(Q), "events"), pos = 2, cex = 0.75)

########### add legend with counts
    ltext <- c(paste("low Grubbs n=", mg$klow),
               paste("normal n=",length(Q) - mg$klow - sum(gtest$tout)),
               paste("high Grubbs n=",sum(gtest$tout)))
    mnths <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")

legend = "norm"
if (max(Q) <= 350) legend = "flip"

if (legend == 'norm') {

      legend("topleft", ltext, col = mcol, pch = mpch, pt.cex = mcex, cex = 0.7, bg = "white")

################## add subplot as legend

  ####### create a blank rectangle
  rect(grconvertX(0.52, from = 'npc'), grconvertY(0.07, from = 'npc'),
       grconvertX(1.00, from = 'npc'), grconvertY(0.32, from = 'npc'),
       col = "white", border = NA)

    msub <- TeachingDemos::subplot(hist(mon, breaks = c(0:12), col = ccol, xaxt = "n",bg = "white",
                         xlab = "", main = "", cex.axis = 0.45, ylab = "",
                         freq = FALSE, las = 1),
                    x = grconvertX(c(0.52,1.00), from = 'npc'),
                    y = grconvertY(c(0.07,0.32), from = 'npc')
    )
    op <- par(no.readonly = TRUE)
    par(msub)
    axis(1, at = 0.5:11.5, labels = mnths, tick = TRUE, lwd = -1,
         line = -1, cex.axis = 0.6)
    mtext("Proportion", side = 2, line = 2, cex = 0.7)
    box()
    par(op)
}

if (legend == "flip") {

  ####### create a blank rectangle
  rect(grconvertX(0.11, from = 'npc'), grconvertY(0.75, from = 'npc'),
       grconvertX(0.59, from = 'npc'), grconvertY(1.00, from = 'npc'),
       col = "white", border = NA)

  legend("bottomright", ltext, col = mcol, pch = mpch, pt.cex = mcex, cex = 0.7, bg = "white")
par(bg = "white")
  msub <- TeachingDemos::subplot(hist(mon, breaks = c(0:12), col = ccol, xaxt = "n", bg = "white",
                                      xlab = "", main = "", cex.axis = 0.45, ylab = "",
                                      freq = FALSE, las = 1),
                                 x = grconvertX(c(0.11,0.59), from = 'npc'),
                                 y = grconvertY(c(0.75,1.00), from = 'npc')
  )
  op <- par(no.readonly = TRUE)
  par(msub)
  axis(1, at = 0.5:11.5, labels = mnths, tick = TRUE, lwd = -1,
       line = -1, cex.axis = 0.6)
  mtext("Proportion", side = 2, line = 2, cex = 0.7)
  box()
  par(op)

}

result <- list(stn, nevents, df$amax, mcode)
names(result) <-  c("Station", "n_events", "amax", "outlier_index")
invisible(result)

  }





