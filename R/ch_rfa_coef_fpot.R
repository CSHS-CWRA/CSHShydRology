#' @export
#' @rdname FitPot
coef.fpot <- 
  function(object, 
           rate = FALSE, 
           ci = FALSE, 
           alpha = 0.05, 
           ...){

  if(rate)
    ans <- c(object$nexcess/object$ntot,object$estimate)
  else
    ans <- object$estimate

  if(ci){

    ## Full likelihood
    llikFull <- logLik(object)

    ## Initiate searching intervals for parameters
    ase <- sqrt(object$varcov[1,1])

    abnd0 <- c(pmax(1e-8, object$estimate[1] - 10 * ase),
               object$estimate[1] + 10* ase)

    kbnd0 <- c(-1,2)


    ## Bound for the deviance
    khi <- qchisq(1-alpha,1)

    ## Deviance for the profile likelihood of the scale parameter
    Dscl <- function(z){

      nllik <- function(p) -sum(dgpa(object$excess, z, p, log = TRUE))

      ## Compute the profile likelihood
      suppressWarnings(llik0 <- -optimize(nllik, kbnd0)$objective)

      return(2*(llikFull-llik0))

    }

    ## Deviance for the profile likelihood of the shape parameter
    Dshp <- function(z){

      nllik <- function(p) -sum(dgpa(object$excess, p, z, log = TRUE))

      suppressWarnings(llik0 <- -optimize(nllik, abnd0)$objective)

      return(2*(llikFull-llik0))
    }

    ## Find the boundary

    suppressWarnings(lbScl <-
        optimize(function(z) abs(Dscl(z)-khi),
                 c(abnd0[1],object$estimate[1]))$minimum)

    suppressWarnings(ubScl <-
                       optimize(function(z) abs(Dscl(z)-khi),
                                c(object$estimate[1],abnd0[2]))$minimum)

    suppressWarnings(lbShp <-
                       optimize(function(z) abs(Dshp(z)-khi),
                                c(kbnd0[1],object$estimate[2]))$minimum)

    suppressWarnings(ubShp <-
                       optimize(function(z) abs(Dshp(z)-khi),
                                c(object$estimate[2],kbnd0[2]))$minimum)

    bnd <- data.frame(lower = c(NA,lbScl,lbShp),
                      upper = c(NA,ubScl,ubShp))

    rownames(bnd) <- c('rate','alpha','kappa')

    if(!rate) bnd <- bnd[-1,]

    ans <- data.frame(estimate = ans, bnd)

  }

  return(ans)
}