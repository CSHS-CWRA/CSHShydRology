#' Gumbel-transformed axes
#'
#' Transforms return period or annual exceedance probability to be spaced
#' according to a reduced Gumbel variate. Can be added as a scale layer to
#' a ggplot2 graphic.
#'
#' @param ... Arguments to pass to \code{scale_x_continuous} or
#' \code{scale_y_continuous} from \pkg{ggplot2}.
#' @return The same output as \code{scale_x_continuous} or
#' \code{scale_y_continuous}, but with the appropriate Gumbel spacing.
#' @rdname gumbel_spacing
#' @importFrom scales trans_new breaks_log
#' @examples
#' library(ggplot2)
#'
#' # A flood frequency plot: annual maximum flows against the probability of
#' # being exceeded in any given year, on Gumbel spacing.
#' ams <- ch_rfa_extractamax(Flow ~ Date, CAN05AA008, tol = 350)
#' ams <- ams[order(ams$Flow), ]
#' # Weibull plotting positions
#' ams$aep <- 1 - seq_len(nrow(ams)) / (nrow(ams) + 1)
#'
#' ggplot(ams, aes(aep, Flow)) +
#'   geom_point() +
#'   scale_x_gumbel_aep("Annual exceedance probability") +
#'   ylab(expression(paste("Annual maximum daily discharge (", m^3, "/s)")))
#'
#' # The same data, read as return periods instead.
#' ggplot(ams, aes(1 / aep, Flow)) +
#'   geom_point() +
#'   scale_x_gumbel_rp("Return period (years)")
#'
#' # The y variants behave the same way.
#' ggplot(ams, aes(Flow, 1 / aep)) +
#'   geom_point() +
#'   scale_y_gumbel_rp("Return period (years)")
#' @export
scale_x_gumbel_rp <- function(...) {
  ggplot2::scale_x_continuous(..., trans = ch_gumbel_rp_trans())
}

#' @rdname gumbel_spacing
#' @export
scale_y_gumbel_rp <- function(...) {
  ggplot2::scale_y_continuous(..., trans = ch_gumbel_rp_trans())
}

#' @rdname gumbel_spacing
#' @export
scale_x_gumbel_aep <- function(...) {
  ggplot2::scale_x_continuous(..., trans = ch_gumbel_aep_trans())
}

#' @rdname gumbel_spacing
#' @export
scale_y_gumbel_aep <- function(...) {
  ggplot2::scale_y_continuous(..., trans = ch_gumbel_aep_trans())
}

#' Gumbel transformations used for ggplot2 scales
#'
#' Build the transformation objects underlying the \code{scale_*_gumbel*()}
#' functions. \code{ch_gumbel_rp_trans()} works on return periods (values greater
#' than 1); \code{ch_gumbel_aep_trans()} works on annual exceedance probabilities
#' (values between 0 and 1). Both map their input onto the reduced Gumbel
#' variate \eqn{-\log(-\log(1 - p))}.
#'
#' These are functions rather than stored objects so that the transformation is
#' built when it is used, against the installed version of \pkg{scales}, rather
#' than being fixed when the package is built.
#'
#' @return A \code{transform} object, as produced by
#' \code{\link[scales]{trans_new}}.
#' @rdname ch_gumbel_trans
#' @examples
#' tr <- ch_gumbel_rp_trans()
#' tr$transform(c(2, 10, 100))
#'
#' tr_aep <- ch_gumbel_aep_trans()
#' tr_aep$transform(c(0.5, 0.1, 0.01))
#' @export
ch_gumbel_rp_trans <- function() {
  scales::trans_new(
    "gumbel_rp",
    transform = function(x) -log(-log(1 - 1 / x)),
    inverse = function(x) 1 / (1 - exp(-exp(-x))),
    breaks = scales::breaks_log(),
    domain = c(1, Inf)
  )
}

#' @rdname ch_gumbel_trans
#' @export
ch_gumbel_aep_trans <- function() {
  scales::trans_new(
    "gumbel_aep",
    transform = function(x) -log(-log(1 - x)),
    inverse = function(x) 1 - exp(-exp(-x)),
    breaks = scales::breaks_log(),
    domain = c(1e-100, 1)
  )
}
