########################################################################
#' Prediction at ungauged sites using region of influence and kriging
#'
#' Return the prediction of a local regression model using region of
#' influence (ROI) where the residuals are further predicted
#' by kriging.
#' 
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#'
#' @param x Data for training the model.
#'
#' @param xnew Data at new locations (Validation set).
#'
#' @param nk Number of sites in the neighborhoods. If \code{nk} is a list, the 
#'   first element represents a vector of the neighborhood size for all sites 
#'   used for training. The second element . 
#'
#' @param phy Formula defining the physical descriptors.
#'
#' @param similarity Formula defining the covariates used
#'   to evaluate the similarity between site, i.e. Euclidean distance 
#'   to the target.
#'
#' @param kriging Formula defining the spatial covariates.
#'   Necessary for predicting residuals using spatial correlation.
#'
#' @param model Variogram model. See \link[gstat]{vgm}
#'
#' @param ker Should a (Epanechnikov) kernel be used in to weight
#'  local regression model. Otherwise uniform weights are used.
#'  
#' @param se Logical. Should the standard error be returned.
#' 
#' @param fold Number of group or group used to perform cross-validation.
#' 
#' @param fitted Logical. Should the fitted value be returned. Useful to
#'   obtain fitted values at gauged sites from a ROI model when kriging is not
#'   used. 
#'   
#' @param object Output from \code{FitRoi}.
#' 
#' @param ... Other parameters.
#'
#' @return
#'
#' \item{pred}{Prediction at new sites.}
#' \item{pred.se}{Standard deviation at new sites.}
#' \item{phy}{Part of the prediction attributed to physical descriptor.}
#' \item{phy.se}{Standard deviation associated with the physical descriptor.}
#' \item{fitted}{Fitted values (training sites).}
#' \item{fitted.se}{Standard deviation of the fitted values.}
#' \item{vgm}{Sample variogram.}
#' \item{model}{Fitted variogram model.}
#' 
#' @seealso \code{\link{CvRoi}}, \code{\link[gstat]{krige}}, \code{\link{lm}}
#' 
#' @references 
#' 
#' Martin Durocher, Donald H. Burn, Shabnam Mostofi Zadeh & Fahim Ashkar (2019) 
#'   Estimating flood quantiles at ungauged sites using nonparametric regression 
#'   methods with spatial components, Hydrological Sciences Journal, 64:9, 
#'   1056-1070, https://doi.org/10.1080/02626667.2019.1620952
#'
#' @export
#'
#' @examples
#' 
#' data(flowUngauged)
#' 
#' ## Using multidimensional scaling for projecting coordinates
#' coord <- flowUngauged[,c('lon','lat')]
#' coord <- cmdscale(GeoDist(coord))
#' colnames(coord) <- c('lon','lat')
#' 
#' 
#'
#' ## Transform data if necessary
#' xdf <- with(flowUngauged, 
#'          data.frame(y      = l1,
#'                     area   = scale(log(area)),
#'                     wb     = scale(log(wb)),
#'                     stream = scale(log(stream)),
#'                     map    = scale(log(map)),
#'                     coord))
#'
#'  ## select a validation and training set
#'  set.seed(9382)
#'  vid <- runif(nrow(xdf)) > .8
#'  tid <- !vid
#'
#'  # formula of the relationship between flood quantile and descriptors
#'  fphy <- log(y) ~ area + map + poly(wb,3) + poly(stream,3)
#'  fsimilarity <- ~ area + map
#'
#'  ## Fit a local regression model
#'  fit <- FitRoi(x = xdf[tid,], xnew = xdf[vid,], nk = 60,
#'                phy = fphy, similarity = fsimilarity)
#'  print(fit)
#'  response <- log(xdf[vid,'y']) 
#'  sd(response - fit$pred)
#'
#'  ## Refit the model and perform the kriging of the residuals
#'  fitk <- FitRoi(x = xdf[tid,], 
#'                xnew = xdf[vid, ], 
#'                nk = 60,
#'                phy = fphy, 
#'                similarity = fsimilarity , 
#'                model = 'Exp',
#'                kriging = ~ lon + lat)
#'                
#'  print(fitk)
#'  sd(response - fitk$phy)
#'  sd(response - fitk$pred)
#'
FitRoi <- 
  function(x, 
           xnew, 
           nk, 
           phy, 
           similarity, 
           kriging = NULL, 
           model = 'Exp', 
           ker = TRUE,
           se = FALSE,
           fitted = FALSE){

  x <- as.data.frame(x)
  xnew <- as.data.frame(xnew)

  ## Extract coordinate and compute euclidean distance
  crd <- rbind(model.frame(similarity, x),
               model.frame(similarity, xnew))

  h <- as.matrix(dist(crd))

  ## Extract coordinate of the spatial model
  if(!is.null(kriging))
    crd2 <- rbind(model.frame(kriging,x),
                  model.frame(kriging,xnew))

  ## Create a vector of neighborhood size for all sites.
  if(is.list(nk)){
    nk0 <- unlist(nk)
  } else{
    nk0 <- rep(nk[1],nrow(h))
  }

  ## add a column to store weights and extract response
  x <- get_all_vars(phy, x, .ww = 0)
  y0 <- eval(phy[[2]], envir = x)
  

  train <- seq(nrow(x))
  valid <- seq(nrow(x)+1,nrow(crd))

  ##-------------------------
  ## Fitt model at target
  ##-------------------------
  
  ## allocate memory 
  .ww <- NULL ## avoid global variable with no clear bind error forcheck
  pred <- rep(NA, length(valid))
  
  if(se)
    predSe <- rep(NA, length(valid))
  
  for(ii in seq_along(valid)){

    ## Delineate a neighborhood
    nn <- order(h[valid[ii], train])[1:nk0[ valid[ii] ] ]

    ## Fit local linear model. With weight if necessary.
    if(ker){
      x[nn,'.ww'] <- 1-(h[valid[ii],nn]/max(h[valid[ii],nn]))^2 
      fit <- lm(phy, data = x[nn,], weights = .ww)

    } else{
      fit <- lm(phy, data = x[nn,])
    }
    
    if(se){
      out <- predict(fit, xnew[ii,], se.fit = TRUE)
      pred[ii] <- out$fit
      predSe[ii] <- out$se.fit
    
    } else{
      pred[ii] <- predict(fit, xnew[ii,])
    }
  }

  ## Save the specification of the model provided when the function is called
  ans <- list(call = list(nk = nk,
                          npred = length(valid), 
                          nsite = length(train),
                          ker = ker, 
                          kriging = kriging, 
                          phy = phy,
                          similarity = similarity))

  ##------------------------------------
  ## Fit training set if required
  ## ----------------------------------------
  
  if(!is.null(kriging))
    fitted <- TRUE
  
  if(fitted){

    ## For all training set
    yhat <- rep(NA, length(train))
    
    if(se)
      yhatSe <- rep(NA, length(train))
    
    for(ii in train){

      ## Delineate a neighborhood
      nn <- order(h[ii,train])[1:nk0[ii]]

      ## Fit local linear model. With weight if necessary.
      if(ker){
        x[nn,".ww"] <- 1-(h[ii,nn]/max(h[ii,nn]))^2
        fit <- lm(phy, data = x[nn,], weights = .ww)
      } else {
        fit <- lm(phy, data = x[nn,])
      }
      
      ## Predict the linear model and its standard deviation (if necessary)
      if(se){
        out <- predict(fit, x[ii,], se.fit = TRUE)
        yhat[ii] <- out$fit
        yhatSe[ii] <- out$se.fit
      
      } else{
        yhat[ii] <- predict(fit, x[ii,])
      }
    }
    
    ## Save results
    ans$fitted <- yhat
    
    if(se)
      ans$fitted.se <- yhatSe
      
    ## compute residuals
    yres <- y0-yhat
    
    ans$resid <- yres
  }
  
  ##------------------------------------
  ## Perform kriging if necessary
  ## ----------------------------------------
  
  if(!is.null(kriging)){
    
    ## Save known component
    ans$phy <- pred
    
    if(se)
      ans$phy.se <- predSe
    
    ## Extract the coordinates for the residuals
    crd2 <- data.frame(yres = c(yres, rep(0,length(pred))), crd2)
    sp::coordinates(crd2) <- kriging

    ## Estimate the variogram
    if(all(class(model) == 'character')){
      model <- gstat::vgm(model)
      vgm0 <- gstat::variogram(yres~1, crd2[train,])
      model0 <- gstat::fit.variogram(vgm0, model)
      
      ans$vgm <- vgm0
    
    } else if(any(class(model) == "variogramModel")){
      model0 <- model 
    
    } else{
      stop('The kriging model is not valid.') 
    }
    
    ## save model
    ans$model <- model0
      
    ## Perform simple kriging on the residuals
    out <- gstat::krige(yres~1, crd2[train,], crd2[valid,],
                          model = model0, beta = 0, 
                          debug.level = 0)

    
    ## Evaluate prediction
    ans$pred <- pred + out$var1.pred
    ans$krige <- out$var1.pred
    
    if(se)
      ans$krige.se <- sqrt(out$var1.var)
      
  } else {
    
    ## kriging is not used
    ans$pred <- pred
    
    if(se)
      ans$pred.se <- predSe
  }

  ## return
  class(ans) <- 'roi'
  return(ans)
}

#' @export
#' @rdname FitRoi
print.roi <- function(x, ...){
 cat('\n\nRegion of Influence (ROI)\n')
 cat('\nNumber of sites:', x$call$nsite)
 cat('\nNumber of targets:', x$call$npred)

 cat('\n\nRegression:')
 cat('\n  Physic:', format(x$call$phy))
 cat('\n  size:', sort(unique(x$call$nk)))
 cat('\n  Similarity', format(x$call$similarity))
 cat('\n  Kernel:', format(x$call$ker))

 if(!is.null(x$model)){
   cat('\n\nKriging:')
   cat('\n  Coord:', format(x$call$kriging),'\n\n')
   print(as.data.frame(x$model)[,1:4])
 }

}

#' @export
#' @rdname FitRoi
predict.roi <- function(object, x, fold = 5, ...){
  
  ## Organize the cross-validation group info
  if(length(fold) == 1){
    k <- 1:fold
    fold <- sample(rep_len(k, nrow(x)))
    
  } else{
    k <- sort(unique(fold))
  }
    
  ## allocate memory
  ans <- rep(0, nrow(x))
  
  for(ii in k){
    
    if(sum(fold != ii) <= object$call$nk)
      stop('Requires more observations')
      
    ## For each cross-validation group
    validSet <- which(fold == ii)
    
    if(is.null(object$call$kriging)){
      
      ## Fit model without kriging
      hat <- FitRoi(x = x[-validSet,], 
                  xnew = x[validSet,] , 
                  nk = object$call$nk,
                  phy = object$call$phy,
                  similarity = object$call$similarity)
      
      
    } else {
      
      ## Fit model with kriging
      hat <- FitRoi(x = x[-validSet,], 
                    xnew = x[validSet,], 
                    nk = object$call$nk,
                    phy = object$call$phy,
                    similarity = object$call$similarity,
                    kriging = object$call$kriging, 
                    model = object$model)
    }
    
    ans[validSet] <- hat$pred
  }
  
  return(ans)
}

#' @export
#' @rdname FitRoi
residuals.roi <- function(object, x, fold = 5, ...){
  response <- eval(object$call$phy[[2]], envir = x)
  return(predict(object, x = x, fold = fold) - response)
  
}