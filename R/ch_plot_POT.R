#' ch_plot_POT
#'
#' @description
#' A plot of peaks-over-threshold derived from a plot method from the package POT.
#'
#' A single object matching ‘retlev.uvpot’ was found. It was found in the following places
#' registered S3 method for retlev from namespace POT
#' namespace:POT
#' with value
#'
#' The original function only plotted generic symbols for the observations.
#' It was modified to add informative symbols and colours for FFA screening. These
#' are indicated with a "+" below.
#' The plots are shown in Whitfield and Burn (2025b)
#'
#' @param object output from POT function fitgpd
#' @param npy number of events per year = #events/#years
#' @param main  plot title
#' @param xlab  xaxis label
#' @param ylab  y axis label
#' @param xlimsup upper limit for Pareto
#' @param mon  array of months of observations [1-12] +
#' @param ccol  array of circular colours [12] +
#' @param mcex  size of plot symbols [4] +
#' @param mpch  symbols for points [4] +
#' @param mcode array of indexes for symbol plotting [1-4] +
#' @param sr_pch symbols for overlay [NA, 8] +
#' @param mcol array of colours for outline of points [3] +
#' @param isout array of indexes for timing outliers [1-2] +
#' @param ci confidence interval = TRUE
#' @param points plot points = TRUE
#' @param ...
#'
#' @return returns the pot function invisibly and generates a plot
#'
#' @export
#'
#' @reference
#' Whitfield, P. H. and D. H. Burn (2025*). "Screening Annual Maxima and
#' Peaks-Over-Threshold Series for Flood Frequency Analysis."
#'
#' @author modified by Paul Whitfield original from POT
#'
#' @examples
#' \dontrun{}
#' \donttest{}
#'

ch_plot_POT <- function (object, npy, main, xlab, ylab, xlimsup,
          mon, ccol, mcex, mpch, mcode, sr_pch, mcol, isout,
          ci = TRUE,
          points = TRUE, ...)
{
### changes to call to include symbols and codes
### line starting with mon... to isoout

  par(las = 1)

  if (!inherits(object, "uvpot"))
    stop("Use only with 'uvpot' objects")
  if (object$var.thresh)
    stop("Return Level plot is available only for constant threshold !")
  data <- object$exceed
  loc <- object$threshold[1]
  scale <- object$param["scale"]
  shape <- object$param["shape"]
  n <- object$nat
  pot.fun <- function(T) {
    p <- rp2prob(T, npy)[, "prob"]
    return(qgpd(p, loc, scale, shape))
  }
  eps <- 10^(-3)
  if (!is.null(object$noy))
    npy <- n/object$noy
  else if (missing(npy)) {
    warning("Argument ``npy'' is missing. Setting it to 1.")
    npy <- 1
  }

  if (missing(main))
    main <- "Return Level Plot"
  if (missing(xlab))
    xlab <- "Return Period (yr) [Pareto]"
  if (missing(ylab))
    ylab <- expression( "Peak Flow ("*m^3*s^{-1}*")" )
  if (missing(xlimsup))
    xlimsup <- prob2rp((n - 0.35)/n, npy)[, "retper"]
  ylims <- c(min(data),max(data))

  plot(pot.fun, from = 1/npy + eps, to = xlimsup, log = "x",
       xlab = xlab, ylab = ylab, ylim = ylims, main = main, ...)
  if (points) {  ### add fancy points

    p_emp <- (1:n - 0.35)/n
################################ add symbols and colours
mytest <- 1/(npy * (1 - p_emp))

points(1/(npy * (1 - p_emp)), sort(data), pch = mpch[mcode],
       bg = ccol[mon], col = mcol[mcode])

points(1/(npy * (1 - p_emp)), sort(data), pch = sr_pch[isout], cex = 1.3,
                   col = mcol[4])
########################################################
    }
  if (ci) {   ### add confidence limts
    p_emp <- (1:n - 0.35)/n
    samp <- rgpd(1000 * n, loc, scale, shape)
    samp <- matrix(samp, n, 1000)
    samp <- apply(samp, 2, sort)
    samp <- apply(samp, 1, sort)
    ci_inf <- samp[25, ]
    ci_sup <- samp[975, ]
    lines(1/(npy * (1 - p_emp)), ci_inf, lty = 2 , col = "gray30")
    lines(1/(npy * (1 - p_emp)), ci_sup, lty = 2, col = "gray30")
  }
  invisible(pot.fun)
}
