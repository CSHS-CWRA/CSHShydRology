# Flood Frequency Analysis, plotting and bootstrap condfidence intervals
#
# 2015-08-20 Dave Hutchinson
# see http://headwateranalytics.weebly.com/blog/flood-frequency-analysis-in-r for full description
#######################################################################################

#' FrequencyAnalysis: Fit a given extreme value distribution to an extreme value series
#' @param series A vector representing an extreme value series (e.g., annual maximum flood)
#' @param distribution A three-character name of the extreme value distribution (see ?dist.list())
#' @param nep A vector of non-exceedance probabilities
#' @return A list object containing: (1) distribution information and (2) output
#'  (quantile estimates at various non-exceedance probabilities)
#'  
#' \url{http://headwateranalytics.weebly.com/blog/flood-frequency-analysis-in-r} contains a demo
#' @export
#' @import lmomco
FrequencyAnalysis <- function( series, distribution, nep = nonexceeds() ) {

    distribution <- tolower(distribution)
    transformed <- FALSE
    
  # add log Pearson Type 3 to list of distributions supported
  # by lmomco package
    base.dist <- c('lp3', dist.list())
  
    if( any(distribution %in% base.dist) ) {
        
        # log transform series 
        if( distribution == 'lp3' ) {
            series <- log10(series)
            transformed <- TRUE
            distribution <- 'pe3'
        }
        
        # compute L-moments
        samLmom <- lmom.ub(series)
    
        # estimate distribution parameters
        distPar <- lmom2par(samLmom, type = distribution)
    
        # compute quantiles for nonexceedances
        quant <- par2qua(f = nep, para = distPar)
            
        if( distribution == 'pe3' & transformed ) {
            distribution <- 'lp3'
            quant <- 10^quant
        }
        
        # return result as list object
        return(
            list(
                distribution = list(
                    name = distribution,
                    logTransformed = transformed,
                    parameters = distPar),
                output = data.frame(nep = nep, rp = prob2T(nep), estimate = quant) 
            ) )
            
    } else {
        stop(
            sprintf('Distribution \'%s\' not recognized!', distribution))
    }
}


#' BootstrapCI: estimate confidence interval for each given 
#' non-exceedance probability.
#' @param fitted.model Fitted distribution (see ?frequencyAnalysis)
#' @param series A vector representing an extreme value series (e.g., annual maximum flood)
#' @param distribution A three-character name of the extreme value distribution (see ?dist.list())
#' @param n.resamples An integer representing number of re-samples to conduct
#' @param nep A vector of non-exceedance probabilities
#' @param ci The confidence interval 
#' @export
#' @import lmomco
#' @return A list containing a data frame of confidence bounds for quantile estimates for each 
#' non-exceedance probability, a matrix containing estimated distribution parameters for each resample,
#' and a matrix of quantile estimates for each resample
#' 
#' \url{http://headwateranalytics.weebly.com/blog/flood-frequency-analysis-in-r} contains a demo
BootstrapCI <- function(series, distribution, n.resamples=1E3, nep=nonexceeds(), ci=0.90) {

  # compute frequency analysis
  fa <- FrequencyAnalysis(series=series, distribution=distribution, nep=nep)
  
  # extract fitted model parameters and flag as to whether the 
  # distribution is based on log transformed data
    base.params <- fa$distribution$parameters
    isTransformed <- fa$distribution$logTransformed
    
    # create output matrices to store parameter sets and quantile estimates
    param.sets <- matrix(NA, nrow = n.resamples, ncol = length(base.params$para))
    quantile.estimates <- matrix(NA, nrow = n.resamples, ncol = length(nep), 
                dimnames = list(NULL, nep) ) 
    
    # begin bootstrapping procedure
    for(i in 1:n.resamples) {
    
        valid.moments <- FALSE
    j <- 0
    
    # allow up to 20 re-tries to re-sample 
    while(!valid.moments & j < 20) {  
      
      # sample 'n' random variates from base distribution
          data <- rlmomco(n=length(series), base.params)
            
          # compute sample l-moments
      sample.moms = lmom.ub(data)
        
      valid.moments <- are.lmom.valid(sample.moms)
      j <- j + 1
    }
    
    # error handling
        if(!valid.moments) {
       stop("Bootstrapping failed to sample valid l-moments")
        } else {
      # estimate distribution parameters
          dist.par <- lmom2par(sample.moms, base.params$type)
        
          # store the distribution parameters
          param.sets[i,] <- dist.par$para
  
          # estimate quantiles at NEP
          estimated <- qlmomco(nep, dist.par)
        
      # convert quantile estimates to real values if
      # distribution was transformed
          if(isTransformed) estimated <- 10^estimated
        
          # store the quantiles at the desired AEP values
          quantile.estimates[i,] <- estimated
      } 
  
    }
  
  # now calculate confidence limits for quantiles
  p <- c((1-ci)/2, (1+ci)/2)
  ci <- sapply(colnames(quantile.estimates), 
               FUN=function(x){
                quantile(quantile.estimates[,x], probs=p, na.rm=TRUE)})

    # now return list object containing output
    return(
        list(
              ci = data.frame(
                nonexceed_prob = nep,
                lower = as.vector(ci[1,]),
                true = fa$output$estimate,
                upper = as.vector(ci[2,]) ),
              parameters = param.sets,
              quantiles = quantile.estimates)
  )
  
}

#' FrequencyPlot Generates a nice-looking (hydrologist-centric) frequency plot
#' @param series A vector representing an extreme value series (e.g., annual maximum flood)
#' @param ci A data frame containing confidence intervals (lower, true, upper) derived from 
#'  calling BootstrapCI()
#'  
#' \url{http://headwateranalytics.weebly.com/blog/flood-frequency-analysis-in-r} contains a demo
#' @export ggplot
#' @import ggplot2
#' @import scales
frequencyPlot <- function(series, ci) {
  
  # determine plotting positions
  bwpeaks <- data.frame(PROB = pp(series, sort = FALSE), FLOW = series)
  xbreaks <- c(0.002,0.01,0.1,0.25,0.5,0.8,0.9,0.95,0.975,0.99,0.995, 0.998)
  log.range <- log10(range(series, ci[,-1], na.rm = TRUE))
  lower <- 10^floor(log.range[1])
  upper <- 10^ceiling(log.range[2])
  cap <- lower
  ybreaks <- NULL
  while(cap < upper) {
    ybreaks <- c(ybreaks, seq(cap, cap*9, by = cap))
    cap <- cap * 10
  }
  
  # now plot
  ggplot(bwpeaks) + 
    geom_point(aes(x=PROB, y=FLOW)) + 
    theme_bw() + 
    scale_y_continuous(trans="log10", breaks=ybreaks, name="Discharge") +
    scale_x_continuous(trans=probability_trans(distribution="norm"),
                       breaks=xbreaks, labels=signif(prob2T(xbreaks), digits=4),
                       name="Return period [yrs]") +
    geom_line(data=ci, aes(x=nonexceed_prob, y=true), color="red") +
    geom_line(data=ci, aes(x=nonexceed_prob, y=lower), color="red", lty=2) +
    geom_line(data=ci, aes(x=nonexceed_prob, y=upper), color="red", lty=2)
  
}