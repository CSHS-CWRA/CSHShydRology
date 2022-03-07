#' Seasonal statistics for flood peaks
#'
#' @description Return the circular or seasonal statistics of flood peaks.
#' The angle represents the mean timing of the floods and the radius its
#' regularity. For example, a radius of one represents perfect regularity. 
#' Can perform the analyses on multiple sites.
#' 
#' @author Martin Durocher
#'
#' @param x Data. If data.frame with two columns, they must be respectively 
#'   the date and a site variable.
#'
#' @param form Formula that specifies the date and site variable. Must be of the
#'   form \code{date ~ site}.
#'   
#' @param ... Other parameters.
#' 
#' @seealso \link{ch_rfa_distseason}
#'
#' @return Returns the circular or seasonal statistics of flood peaks.
#' @references 
#'
#' Burn, D.H. (1997). Catchment similarity for regional flood frequency analysis
#'   using seasonality measures. Journal of Hydrology 202, 212-230.
#'   https://doi.org/10.1016/S0022-1694(97)00068-1
#'
#' @export
#' 
#' @importFrom stats model.frame
#' 
#' @examples
#'
#' dt <- ch_rfa_extractamax(flow~date, CAN01AD002)$date
#'
#' ch_rfa_seasonstat(dt)
#' 
#' ## Illustration of the analysis of multiple sites
#' 
#' F0 <- function(ii) data.frame(site = ii, dt = sample(dt, replace = TRUE))
#' x <- lapply(1:10, F0)
#' x <- do.call(rbind, x)
#'            
#' st <- ch_rfa_seasonstat(dt ~ site, x)
#' 
#' ch_rfa_julianplot()
#' points(y ~ x, st, col = 2, pch = 16)
#'
ch_rfa_seasonstat <- function(x, ...) UseMethod('ch_rfa_seasonstat', x)

#' @export
ch_rfa_seasonstat.default <- function(x, ...){

  x <- as.Date(x)

  deg <- 2 * pi * DecimalDay(x)

  ## compute average
  xbar <- mean(cos(deg))
  ybar <- mean(sin(deg))

  ## compute the circular statistic
  cs <- Xy2polar(xbar, ybar)

  ans <- c(xbar, ybar, cs$angle, cs$radius)
  names(ans) <- c('x','y','angle','radius')
  
  return(ans)
}

#' @export
#' @rdname ch_rfa_seasonstat
ch_rfa_seasonstat.data.frame <- function(x, ...){
   ans <- lapply(split(x[,1], as.character(x[,2])), ch_rfa_seasonstat)
   return(do.call('rbind', ans))
}

#' @export
#' @rdname ch_rfa_seasonstat
ch_rfa_seasonstat.formula <- function(form, x, ...){
  x <- model.frame(form,as.data.frame(x))
  return(ch_rfa_seasonstat(x))
}

## Convert the day of the year into a decimal value
## take leap year into account
DecimalDay <- function(x){

  ## Extract julian day
  x <- as.Date(x)
  yy <- as.numeric(format(x, "%Y"))
	dd <- as.numeric(format(x, "%j"))

	## verify for leap years
	isLeap <- is.leapyear(yy)

	## Convert in decimal
	(dd - .5) / (365 + isLeap)

}

## Logical is y a leap year
is.leapyear <- function(y)
  ((y %% 4 == 0) & (y %% 100 != 0)) | (y %% 400 == 0)


## Convert cartesian to polar coordinates
Xy2polar <- function(x,y){

  ## compute polar coordinates
 	ang <- atan(abs(y/x))
 	r <- sqrt(x^2 + y^2)

 	## correcting angle for
  if (sign(x) < 0 & sign(y) >= 0) # second quadrant
    ang <- pi - ang
 	else if (sign(x) < 0 & sign(y) < 0) # third quadrant
 	  ang <- pi + ang
 	else if (sign(x) >= 0 & sign(y) < 0) # fourth quadrant
 	 ang <- 2*pi - ang

 	list(radius = r, angle = ang)
}

