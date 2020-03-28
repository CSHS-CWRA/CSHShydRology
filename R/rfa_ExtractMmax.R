#' @export
#' @rdname ExtractAmax
ExtractMmax <- function(x, ...) UseMethod('ExtractMmax',x)

#' @export
#' @rdname ExtractAmax
ExtractMmax.default <- function(x, tol = 28, ...){
  
  dd <- x[,2]
  
  ## Make sure that the data are sorted
  if(!is.null(dd))
    x <- x[order(dd),]
  
  ## split data in month
  xs <- split(x, format(dd,'%Y-%m'))
  mx <- lapply(xs, function(z) z[which.max(z[,1]),])
  nn <- sapply(xs, nrow)
  
  ans <- do.call(rbind, mx[nn>= tol])
  rownames(ans) <- NULL
  
  return(ans)
}

#' @export
#' @rdname ExtractAmax
ExtractMmax.formula <- function(form, x, tol = 28, ...){
   ## reformat dataset according to formula
  x <- get_all_vars(form,x)

  if(ncol(x) == 2){
    ## Case of one site
    ans <- ExtractMmax(x, tol =tol)

  } else {
    ## case multiple sites

    ## split the site
    xlst <- split(x[,c(1,3)], x[,2])
    site.value <- sapply(split(x[,2], x[,2]), '[',1)

    ## extract all montly maximums
    ans <- lapply(xlst, ExtractMmax, tol = tol)

    ## merge the results in one dataset
    cname <- c(colnames(ans[[1]]), colnames(x)[2])

    for(ii in seq_along(site.value))
      suppressWarnings(ans[[ii]] <- data.frame(ans[[ii]], site.value[ii]))

    ans <- do.call('rbind', ans)
    
    ## Fix names
    colnames(ans) <- cname
    rownames(ans) <- NULL
    nc <- length(cname)
    
    ## reorder columns
    if(nc == 3){
      ans <- ans[,c(1,3,2)]
    } else {
      ans <- ans[,c(1,nc,2,seq(3,nc-1))]
    }
    
  }
  
  return(ans)
}