#' Return a padded time series for peaks over threshold model.
#' 
#' Return a padded time series where incomplete years are removed and missing days of complete
#' years are imputed. 
#'
#' @param form Formula
#' @param x Data
#' @param z Imputed value.
#' @param tol Number of days to consider a year complete.
#' @param ... Other parameters.
#'
#' @details 
#' 
#' If there is no formula, \code{x} must be a data frame with either 
#' 2 (date, value) or 3 (site, date, value) columns.
#' Similarly, the formula must be of the form \code{value ~ date} or 
#' \code{value ~ site + date}.
#' 
#' @seealso \link{FitPot}.
#' 
#' @export
#'
#' @examples
#' 
#' ## Will remove 1926 that is incomplete
#' xd <- PadPot(flow ~ date, flowStJohn, tol = 365)
#' head(xd)
#' 
#' ## Will pad 1926 that is "complete"
#' xd <- PadPot(flow ~ date, flowStJohn, tol = 1)
#' head(xd)
#' 
#' 
PadPot <- function(x, ...) UseMethod('PadPot', x)

#' @export
#' @rdname PadPot
PadPot.default <- function(x,...)
  PadPot(as.data.frame(x), ...)

#' @export
#' @rdname PadPot
PadPot.formula <- function(form, x, ...){
  
  ## Extracting data from formula
  x <- model.frame(form,x)
  
  ## Reorganizing the column order.
  if(ncol(x) == 2)
    x <- x[ , c(2,1)]
  else if(ncol(x) == 3)
    x <- x[ , c(2,3,1)] 
  
  
  return(PadPot(x,...))
}

#' @export
#' @rdname PadPot
PadPot.data.frame <- function(x, z = 0, tol = 346, ...){

	if(ncol(x) == 2){
	  ans <- .PadPot(x, z, tol)

	} else if(ncol(x) == 3){

		id <- as.character(x[,1])
  	lst <- split(x[,-1], id)
  	lst.site <- tapply(x[,1], id, '[', 1, simplify = FALSE)
  	lst <- mapply(.PadPot, x = lst , site = lst.site,
  								MoreArgs = list(z = z, tol = tol), SIMPLIFY = FALSE)
    ans <- do.call(rbind, lst)

	} else {
	  stop('The input must have 2 or 3 columns.')
	}

	colnames(ans) <- colnames(x)
	rownames(ans) <- NULL

	return(ans)
}


## Return a padded time series
## input
## x: sorted data.frame(date, value)
## z: value to impute
## tol: Minimum number of days to be considered complete
.PadPot <- function(x, z, tol, site = NULL){

	## Remove incomplete years
	yy <- format(x[,1], '%Y')
	yy.nb <- table(yy)
	yy.complete <- names(yy.nb[yy.nb >= tol])
	x <- x[yy %in% yy.complete,]

	## Create a data.frame of all dates
	d <- Map(.AllDates,yy.complete)
  d <- lapply(d, data.frame, value = z)
  d <- do.call(rbind,d)
	colnames(d) <- c('date','value')
	rownames(d) <- NULL

	d[d$date %in% x[,1],2] <- x[,2]

	if(!is.null(site))
	  d <- cbind(site,d)

	return(d)
}

## Return a vector(Date) of all the days of a year
## input
## y: year
.AllDates <- function(y){
	sdate <- as.Date(paste0(y,'/1/1'))
	edate <- as.Date(paste0(y,'/12/31'))
	return(seq(sdate,edate, 'days'))
}
