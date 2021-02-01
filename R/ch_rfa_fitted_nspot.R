###############################################################################
#' Extract information from a nonstationary POT model
#' 
#' Return the thresholds, trends, residuals and parameters of the model.
#' 
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#'
#' @param object Output form \cite{FitNsPot}
#' 
#' @param newdata Datasets used to evaluate the trend and the treshold a 
#'   given dates. 
#'   
#' @param type Type of output. 
#' 
#' @param ... Other parameters.
#'
#' @export
#'
#' @details 
#'  
#' The function \code{fitted} return a data.frame containing the original 
#' observation, date, threshold, trend and standardized values. The latter
#' correspond the observations after substracting the threshold and dividing by
#' the mean excess (trend).
#' 
#' For \code{residuals}, the argument \code{type} can be \code{'thresh'} for
#' returning the value above the threshold or \code{'scale'} for the 
#' standardized value. 
#' 
#' The function \code{coef} returned some parameters of the fitted model. 
#' Can be one of \code{'kappa'} for the shape parameter, \code{'trend'}, 
#' \code{'thresh'} or \code{'all'}.
#' 
#' @seealso \link{FitNsPot}
#' 
fitted.nspot <- function(object, newdata = NULL, ...){
  
  ## Compute the threshold
  if(!is.null(newdata)){
    thresh.xmat <- model.matrix(attr(object$threshold$data, 'term'), newdata)
  } else {
    x0 <- object$threshold$data[object$peak, , drop = FALSE]
    thresh.xmat <- model.matrix(attr(object$threshold$data, 'term'), x0)
                                
  }

  thresh.y <- thresh.xmat %*% object$threshold$beta
  
  ## Compute the mean excess
  if(!is.null(newdata)){
    trend.xmat <- model.matrix(attr(object$trend$data, 'term'), newdata)
  } else {
    trend.xmat <- model.matrix(attr(object$trend$data, 'term'), 
                               object$trend$data)
  }
  
  trend.y <- trend.xmat %*% object$trend$beta
  
  mlink <- make.link(object$trend$link)
  trend.y <- mlink$linkinv(trend.y)
  
  ## Compute the variable
  if(!is.null(newdata)){
    dd <- model.frame(attr(object$data, 'term'), newdata)
  } else {
    dd <- object$data[object$peak, , drop = FALSE]
  }
  
  y <- model.response(dd)
  z <- (y - thresh.y)/trend.y
  
  return(data.frame(time = dd[,2], 
                    original = y, 
                    threshold = thresh.y, 
                    trend = trend.y, 
                    scale = z))
}

#' @export
#' @rdname fitted.nspot
residuals.nspot <- function(object, type = 'thresh', ...){
  
  cc <- fitted(object)
  
  if(type == 'scale'){
    ans <- cc$scale
  
  } else if(type %in% c('thresh','threshold')){
    ans <- cc$original - cc$threshold
  
  } else {
    stop("The type of residuals must be : 'thresh' or 'scale'") 
  }
  
  return(ans)
}

#' @export
#' @rdname fitted.nspot
coef.nspot <- function(object, type = 'all', ...){
  
  if(type == 'kappa'){
    ans <- object$kappa + c(1,0)
  
  } else if(type == 'trend'){
    ans <- object$trend$beta
    
  } else if(type %in% c('thresh','threshold')){
    ans <- object$trend$beta
    
  } else if(type == 'all'){
    ans <- c(object$kappa, object$threshold$beta, object$trend$beta)
    names(ans) <- c('kappa',
                    paste0('thresh.', names(object$threshold$beta)),
                    paste0('trend.', names(object$trend$beta)))
  }

  return(ans)
}