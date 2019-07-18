########################################################################
#' Cross-validation using region of influence and kriging
#'
#' Return a matrix of criteria evaluated by using region of
#' influence (ROI) and kriging. It includes: Root mean square error 
#' (\code{rmse}), relative RMSE (\code{rrmse}), Nash-Sutcliffe (\code{nsh}),
#' Mean absolute deviation (\code{mad}), relative MAD (\code{rmad}) and
#' the skill score based on MAD (\code{smad}). 
#' The latter \code{smad} has the same form as \code{nsh} except that 
#' absolute error are taken instead of square error.
#' 
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#'
#' @param x Data.
#'
#' @param nk List of neighborhood sizes to try.
#'
#' @param phy Formula defining the physical descriptors.
#'
#' @param similarity Formula defining the covariates used
#'   to evaluate the similarity between site, i.e. Euclidean distance to 
#'   the target.
#'
#' @param kriging Formula defining the spatial covariates.
#'   Necessary for predicting residuals using spatial correlation.
#'
#' @param model Variogram model. See \link[gstat]{vgm}.
#'
#' @param ker Should a (Epanechnikov) kernel be used in to weight
#'  local regression model. Otherwise uniform weight are used.
#'  
#' @param fold Number of group used in the cross-validation scheme. 
#'   Can also be a vector defining the group for each site.
#'   
#' @param verbose Logical. Should a progress bar be displayed.
#' 
#' @param crit Cross-validation criteria used to evaluate the best choice.
#' 
#' @param best.col,best.pch,best.cex Argument for the point indication the best
#'   choice.
#'
#' @param ... More arguments to pass to the plot function (\link{par}).
#'
#' 
#' @references 
#' 
#' Martin Durocher, Donald H. Burn, Shabnam Mostofi Zadeh & Fahim Ashkar (2019) 
#'   Estimating flood quantiles at ungauged sites using nonparametric regression 
#'   methods with spatial components, Hydrological Sciences Journal, 64:9, 
#'   1056-1070, https://doi.org/10.1080/02626667.2019.1620952
#'
#' @seealso \link{FitRoi}
#'
#' @export
#'
#' @examples
#' 
#' attach(flowUngauged)
#' 
#' ## Using multidimensional scaling for projecting coordinates
#' coord <- cbind(lon,lat)
#' coord <- cmdscale(GeoDist(coord))
#' colnames(coord) <- c('lon','lat')
#'
#' ## Transform data if necessary
#' xdf <- data.frame(y      = log(l1),
#'                   area   = scale(log(area)),
#'                   wb     = scale(log(wb)),
#'                   stream = scale(log(stream)),
#'                   map    = scale(log(map)),
#'                   coord)
#'
#'  ## select a validation and training set
#'  set.seed(9382)
#'  vid <- runif(nrow(xdf)) > .8
#'  tid <- !vid
#'
#'  # formula of the relationship between flood quantile and descriptors
#'  fphy <- y ~ area + map + wb + stream
#'  fsimilarity <- ~ area + map
#'
#'  ## Perform cross-validation.
#'  system.time(out <- CvRoi(x = xdf, nk = seq(20,150, 10), fold = 5,
#'                  phy = fphy,  similarity = fsimilarity, model = 'Exp'))
#'
#'  head(out, 'nsh')
#'  plot(out, 'mad')
#'
CvRoi <- 
  function(x, 
           nk, 
           phy, 
           similarity,
           kriging = NULL, 
           ker = TRUE, 
           model = 'Exp',
           fold = 5,
           verbose = TRUE){

  x <- as.data.frame(x)

  ## Get response variable
  y <- eval(phy[[2]], envir = x)
  n <- length(y)

  ## Create indexes of cross-validation group
  if(length(fold) == 1){
    ifold <- sample(rep_len(1:fold, n))
    fold <- 1:fold
  } else {
    ifold <- as.integer(factor(fold))
    fold <- 1:length(unique(ifold))
  }

  ## ----------------------------------------------------------------
  ## Function that compute the cross-validation criteria for a given
  ## neighborhood size
  ## ---------------------------------------------------------------
  FunCv <- function(nk0){

    ## Compute prediction by local regression
    pred <- rep(NA,n)
    for(jj in fold){

      pred[ifold == jj] <-
        FitRoi(x = x[ifold != jj,],
               xnew = x[ifold == jj,],
               nk = nk0,
               phy = phy,
               similarity = similarity,
               kriging = kriging,
               model = model,
               ker = ker)$pred
    }

    ## Compute cross-validation criteria
    err <-pred - y 
    rerr <- pred / y - 1

    return(c(rmse  = sqrt(mean(err^2)),
             rrmse = sqrt(mean(rerr^2)),
             nsh   = 1 - sum(err^2)/sum((y-mean(y))^2),
             mad   = mean(abs(err)),
             rmad  = mean(abs(rerr)),
             smad  = 1 - sum(abs(err))/sum(abs(y-mean(y)))
    ))
    
  }

  ## ----------------------------
  ## Perform cross-validation
  ## ----------------------------
  cv <- data.frame(matrix(NA, length(nk),6))
  colnames(cv) <- c('rmse','rrmse','nsh','mad','rmad','smad')
  best <- Inf

  if(verbose)
    bar <- txtProgressBar()

  for(ii in seq_along(nk)){

    if(verbose)
      setTxtProgressBar(bar, ii/length(nk))

    ## Get the cross-validation criteria for every sites
    cv[ii,] <- FunCv(nk[ii])
  }

  ## return
  
  ans <- cbind(nk, cv)
  class(ans) <- append('roicv', class(ans))
  
  return(ans)
}

#' @export
#' @rdname CvRoi
head.roicv <- function(x, crit = 'mad', ...){

  lstCrit <- c('rmse','rrmse','nsh','mad','rmad','smad')
  bid <- which(crit == lstCrit)+1
  
    ## Find the best row
  if(crit %in% c('nsh','smad')){
    best <- order(x[,bid], decreasing = TRUE)[1:3]
  } else{
    best <- order(x[,bid])[1:3]
  }
  

  cat('\nCross-validation for Region of Influence (ROI)\n')
  cat('\nBest 3 sizes of neighborhood\n')
  ans <- as.matrix(x[best,])
  print(ans)

}

#' @export
#' @rdname CvRoi
plot.roicv <- 
  function(x, 
           crit = 'mad', 
           best.col = 'red',
           best.pch = 16,
           best.cex = 1, ...){

  ## Find the columns of the required criteria
  lstCrit <- c('rmse','rrmse','nsh','mad','rmad','smad')
  
  bid <- which(crit == lstCrit)+1
  
  ## Find the best row
  if(crit %in% c('nsh','smad')){
    best <- which.max(x[,bid])
  } else{
    best <- which.min(x[,bid])
  }
  
  # produce the graph
  form <- as.formula(paste(crit,'~nk'))
  plot(form, x, type = 'l', ...)
  
  points(x[best,1],
         x[best,bid], 
         col = best.col, 
         pch = best.pch, 
         cex = best.cex)
}

