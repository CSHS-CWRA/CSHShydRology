#' Extracts the annual maxima of a daily time series
#'
#' @return  Returns a data frame containing the annual (Monthly) maxima,
#' the date and the number of observations during the year.
#' 
#' @author Martin Durocher
#'
#' @param form Formula of the form \code{value ~ date} that specifies the
#'   variable from which the annual maximums are extracted and a date variable.
#'
#' @param x Data. If no formula is passed, the first column must be the
#'  value and the second the date.
#'
#' @param tol Filter the years having less than \code{tol} days.
#' 
#' @param nlab,ylab Names for the added columns representing respectively 
#'   the number of yearly observations and the year. 
#'   If set to \code{NULL} the given column is not added. 
#'    
#' @param ... Other parameters.
#' 
#' @export
#' 
#' @importFrom stats get_all_vars
#'
#' @examples
#' 
#' out <- ch_rfa_extractamax(flow ~ date, CAN01AD002, tol = 350)
#' head(out)
#'
ch_rfa_extractamax <- function(x, ...) UseMethod('ch_rfa_extractamax',x)

#' @export
#' @rdname ch_rfa_extractamax
ch_rfa_extractamax.formula <- function(form, x, tol = 0, ...){

  ## reformat dataset according to formula
  x <- get_all_vars(form,x)

  if (ncol(x) == 2) {
    ## Case of one site
    ans <- ch_rfa_extractamax(x, tol = tol, ...)

  } else {
    ## case multiple sites

    ## split the site
    xlst <- split(x[,c(1,3)], x[,2])
    site.value <- sapply(split(x[,2], x[,2]), '[',1)

    ## extract all annual maximums
    ans <- lapply(xlst, ch_rfa_extractamax, tol = tol, ...)

    ## merge the results in one dataset
    cname <- c(colnames(ans[[1]]), colnames(x)[2])

    for (ii in seq_along(site.value))
      suppressWarnings(ans[[ii]] <- data.frame(ans[[ii]], site.value[ii]))

    ans <- do.call('rbind', ans)
    
    ## Fix names
    colnames(ans) <- cname
    rownames(ans) <- NULL
    nc <- length(cname)
    
    ## reorder columns
    if (nc == 3) {
      ans <- ans[,c(1,3,2)]
    } else {
      ans <- ans[,c(1,nc,2,seq(3,nc - 1))]
    }
    
  }
  
  return(ans)
}

#' @export
#' @rdname ch_rfa_extractamax
ch_rfa_extractamax.default <- 
  function(x, 
           tol = 0, 
           nlab = 'n',
           ylab = 'yy',
           ...){

	## Split data by years
	yy <- format(x[,2],'%Y')
	xx <- data.frame(x[,1],seq(nrow(x)))
	lx <- split(xx,yy)

	## Identify the annual maximums and number of obs.
	nx <- sapply(lx, nrow)
	mx <- sapply(lx, function(z) z[which.max(z[,1]),2])

	## Filter the original dataset
	mx <- mx[nx >= tol]
	ans <- x[mx,]
	rownames(ans) <- NULL
	
	## add n obs. and years if needed
	if (!is.null(nlab))
	  ans[,nlab] <- nx[nx >= tol]
	
	if (!is.null(ylab))
	  ans[,ylab] <- yy[mx]

	return(ans)
}

