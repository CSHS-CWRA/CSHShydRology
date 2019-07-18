#########################################################################
#' Plot of the pairwise intersite correlation.
#' 
#' Produce a graphic of the pairwise intersite correlation coefficient with 
#' respect to the distance.  
#'
#' @param x Output from \code{Intersite}.
#' 
#' @param xmat Dataset in wide format.
#' 
#' @param distance Matrix of distances.
#' 
#' @param xlab,ylab Label of the xy-axis.
#' 
#' @param ... Other parameters.
#' 
#' @seealso \link{Intersite}, \link{DataWide}.
#'
#' @export
#'
#' @examples
#' 
#' #' Data(flowAtlantic)
#' 
#' ## Organize annual maximums
#' ams <- flowAtlantic$ams
#' ams$year <- format(ams$date, '%Y')
#' xmat <- DataWide(ams ~ id + year, ams)
#' 
#' ## Compute distance between sites
#' coord <- flowAtlantic$info[, c('lon','lat')]
#' rownames(coord) <- flowAtlantic$info$id
#' h <- GeoDist(coord)
#' 
#' ## make sure that columns match
#' h <- h[colnames(xmat), colnames(xmat)]
#'
#' ## estimate intersite correlation using a model
#' isite <- Intersite(xmat, distance = h, method = 'exp')
#' print(isite)
#' plot(isite, xmat, h)
#' 
plot.isite <- function(x, xmat, distance, xlab = NULL, ylab = NULL, ...){

  if(is.null(ylab))
    ylab <- 'Intersite correlation'

  pcorr <- x$corr[lower.tri(x$corr)]
  pmdl <- x$model[lower.tri(x$model)]

  if(x$method == 'exp' ){

    ph <- distance[lower.tri(x$corr)]
    pid <- order(ph)

    if(is.null(xlab))
      xlab <- 'Distance'

    plot(ph, pcorr, xlab = xlab, ylab = ylab, ...)
    lines(ph[pid], pmdl[pid], col = 2, lwd = 1.5)

  } else if(x$method == 'emp'){

    if(is.null(xlab))
      xlab <- 'Pairs'

    plot(pmdl, xlab = xlab, ylab = ylab, ...)
    abline(h = x$para[1], col = 2, lwd = 1.5)

  } else {
    warning('There is no intersite correlation to display')
  }

}