#' Seasonal statistics for flood peaks
#'
#' @description Return the circular or seasonal statistics of flood peaks.
#' The angle represent the average timing of the floods and the radius its
#' regularity. For instance a radius of one represents perfect regularity. 
#' Can perform the analyses on multiple sites.
#' 
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#'
#' @param x Data. If data.frame with two columns, they must be respectively 
#'   the date and a site variable.
#'
#' @param form Formula that specifies the date and site variable. Must be of the
#'   form \code{date ~ site}.
#'   
#' @param ... Other parameters.
#' 
#' @seealso \link{ch_rfa_DistSeason}
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
#' dt <- ch_rfa_ExtractAmax(flow~date, flowStJohn)$date
#'
#' ch_rfa_SeasonStat(dt)
#' 
#' ## Illustration of the analysis of multiple sites
#' 
#' F0 <- function(ii) data.frame(site = ii, dt = sample(dt, replace = TRUE))
#' x <- lapply(1:10, F0)
#' x <- do.call(rbind, x)
#'            
#' st <- ch_rfa_SeasonStat(dt ~ site, x)
#' 
#' ch_rfa_JulianPlot()
#' points(y ~ x, st, col = 2, pch = 16)
#'
ch_rfa_SeasonStat <- function(x, ...) UseMethod('ch_rfa_SeasonStat', x)

#' @export
ch_rfa_SeasonStat.default <- function(x, ...){

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
#' @rdname ch_rfa_SeasonStat
ch_rfa_SeasonStat.data.frame <- function(x, ...){
   ans <- lapply(split(x[,1], as.character(x[,2])), ch_rfa_SeasonStat)
   return(do.call('rbind', ans))
}

#' @export
#' @rdname ch_rfa_SeasonStat
ch_rfa_SeasonStat.formula <- function(form, x, ...){
  x <- model.frame(form,as.data.frame(x))
  return(ch_rfa_SeasonStat(x))
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

