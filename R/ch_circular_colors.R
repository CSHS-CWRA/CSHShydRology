#' Color Palettes for Circular
#'
#' @description
#' Produces a vector of n contiguous colours. The original version from the package
#' circular produced n colours with 1 and n being the same colour. This version
#' internally produces n + 1 colours and removes the repeated end colour so there
#' are n unique colours. Hue is calculated for points on the circle. Saturation  and
#' value default to 0.85. Default alpha is 1.0 - no transparency.
#'
#'
#' @param n the number of colours (<= 1) to be in the palette
#' @param m the smallest angle in radians
#' @param M the largest angle in radians
#' @param offset the zero in radians
#' @param s saturation further arguments passed to the function hsv
#' @param v value [0.85]
#' @param alpha alpha [1.0] no transparency
#'
#' @return a vector of length n
#' @author Claudio Agostinelli (original) Paul Whitfield (update)
#'
#' @importFrom grDevices hsv
#' @export
#'
#' @examples
#' ch_circular_colors(n=10)

ch_circular_colors <- function (n, m = 0, M = 2 * pi,
                                offset = 0,
                                s = 0.85, v = 0.85, alpha = 1.0) {
  n = n + 1
  hh <- seq(from = (m - offset) %% (2 * pi), to = if ((M - offset) ==
                                                    2 * pi)

    (M - offset)
    else (M - offset)%%(2 * pi), length.out = n)/(M - m)

  mcol <- hsv(h = hh, s= s, v = v, alpha = alpha)
  return(mcol[1:(n-1)])
}
