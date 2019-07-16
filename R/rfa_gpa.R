###########################################################################
#' Generalized Pareto distribution (GPA)
#'
#' Distribution, density, quantile and random function for the Generalized
#' pareto distriution.
#'
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#'
#' @name GPA
#' 
#' @param p,q Probabilities or quantiles for the GPA distribution.
#'
#' @param n Number of simulations.
#'
#' @param alpha Scale parameter of the GPA
#'
#' @param kap Shape parameter of the GPA
#'
#' @param lower.tail Should the propability of the lower tail be returned
#'
#' @param log Should the log-density be returned
#'
#' @references 
#' 
#' Davison, A. C., & Smith, R. L. (1990). Models for Exceedances over High 
#' Thresholds. Journal of the Royal Statistical Society. Series B 
#' (Methodological), 52(3), 393-442. http://www.jstor.org/stable/2345667
#' 
#'
#' @export
#'
#' @examples
#'
#' kap <- -.2
#' a <- 1
#' xd1 <- rgpa(1e4, a, kap)
#' xd2 <- qgpa(runif(1e4), a, kap)
#'
#' qqplot(xd1, xd2)
#'
#'
#' tt <- seq(0.001,6, len = 100)
#'
#' hist(xd1[xd1<6], main = 'GPA distribution',
#'   freq = FALSE, ylim =c(0,1), xlim = c(0,6))
#'
#' lines(tt, dgpa(tt,a,kap))
#' lines(tt,pgpa(sort(tt), a, kap), col = 2, lty = 2)
#'
#'
pgpa <- function (q,  alpha = 1, kap = 0, lower.tail = TRUE)
{
  if (min(alpha) <= 0)
    stop("invalid alpha")

  if (length(kap) != 1)
    stop("invalid kappa")

  q <- pmax(q, 0)/alpha

  if (kap == 0)
    p <- 1 - exp(-q)
  else {
    p <- pmax(1 - kap * q, 0)
    p <- 1 - p^(1/kap)
  }

  if (!lower.tail)
    p <- 1 - p

  return(p)
}

#' @export
#' @rdname GPA
rgpa <- function (n, alpha = 1, kap = 0)
{
  if (min(alpha) < 0)
    stop("invalid alpha")
  if (length(kap) != 1)
    stop("invalid kappa")
  if (kap == 0)
    return(alpha * rexp(n))
  else return(- alpha * (runif(n)^(kap) - 1)/kap)
}

#' @export
#' @rdname GPA
dgpa <- function (x, alpha = 1, kap = 0, log = FALSE)
{
  if (min(alpha) <= 0)
    stop("invalid alpha")

  if (length(kap) != 1)
    stop("invalid kappa")

  d <- x/alpha
  nn <- length(d)
  alpha <- rep(alpha, length.out = nn)
  index <- (d > 0 & ((1 - kap * d) > 0)) | is.na(d)

  if (kap == 0) {
    d[index] <- log(1/alpha[index]) - d[index]
    d[!index] <- -Inf
  }
  else {
    d[index] <- log(1/alpha[index]) -
                            (1 - 1/kap) * log(1 - kap * d[index])
    d[!index] <- -Inf
  }

  if (!log)
    d <- exp(d)

  return(d)
}

#' @export
#' @rdname GPA
qgpa <- function (p, alpha = 1, kap = 0, lower.tail = TRUE)
{
  if (min(p, na.rm = TRUE) < 0 || max(p, na.rm = TRUE) > 1)
    stop("`p' must contain probabilities in (0,1)")

  if (min(alpha) < 0)
    stop("invalid alpha")

  if (length(kap) != 1)
    stop("invalid kappa")

  if (lower.tail)
    p <- 1 - p

  if (kap == 0)
    return(- alpha * log(p))
  else
    return(-alpha * (p^(kap) - 1)/kap)
}