#' Gumbel-transformed axes
#'
#' Transforms return period or annual exceedance probability to be spaced
#' according to a reduced Gumbel variate. Can be added as a scale layer to
#' a ggplot2 graphic.
#'
#' @param ... Arguments to pass to `scale_x_continuous` or `scale_y_continuous`
#' from ggplot2.
#' @return The same output as `scale_x_continuous` or `scale_y_continuous`,
#' but with the appropriate Gumbel spacing.
#' @rdname gumbel_spacing
#' @examples
#' library(ggplot2)
#' df <- data.frame(
#'   y = sort(stats::rexp(100), decreasing = TRUE),
#'   aep = 1:100 / 101
#' )
#' 
#' ggplot(df, aes(aep, y)) +
#'   geom_point() +
#'   scale_x_gumbelAEP()
#' 
#' ggplot(df, aes(1 / aep, y)) +
#'   geom_point() +
#'   scale_x_gumbelRP("Return Period")
#' @export
scale_x_gumbelRP <- function(...) {
  ggplot2::scale_x_continuous(..., trans = gumbelRP_trans)
}

#' @rdname gumbel_spacing
#' @export
scale_y_gumbelRP <- function(...) {
  ggplot2::scale_y_continuous(..., trans = gumbelRP_trans)
}

#' @rdname gumbel_spacing
#' @export
scale_x_gumbelAEP <- function(...) {
  ggplot2::scale_x_continuous(..., trans = gumbelAEP_trans)
}

#' @rdname gumbel_spacing
#' @export
scale_y_gumbelAEP <- function(...) {
  ggplot2::scale_y_continuous(..., trans = gumbelAEP_trans)
}

#' Gumbel transformations used for ggplot2 scales
#' @export
gumbelRP_trans <- scales::trans_new(
  "gumbelRP",
  transform = function(x) -log(-log(1 - 1 / x)),
  inverse = function(x) 1 / (1 - exp(-exp(-x))),
  breaks = scales::breaks_log(),
  domain = c(1, Inf)
)

#' Gumbel transformations used for ggplot2 scales
#' @export
gumbelAEP_trans <- scales::trans_new(
  "gumbelAEP",
  transform = function(x) -log(-log(1 - x)),
  inverse = function(x) 1 - exp(-exp(-x)),
  breaks = scales::breaks_log(),
  domain = c(1e-100, 1)
)