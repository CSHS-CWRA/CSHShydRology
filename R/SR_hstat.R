#' SR_hstat
#'
#'  @description These are updated versions of the functions described in
#'  Sau & Rodriguez (2018).  Daniela Rodriguez kindly shared their original code.
#'  The main modifications here were that the data tables are not read in, but included
#'  as variables and some language was changed to English from Spanish.
#'
#'  This function is basically a wrapper for the series of functions written
#'  by Sau & Rodriguez, and these revisions were made by Paul Whitfield.
#'  Originally separate functions, they were nested with their parent.
#'
#'  Therefore there are three main functions and the execution code within.
#'  All the original comments were preserved subsequent to conversion
#'  to English letter equivalents; these are in comment lines starting with ##.
#'
#' @param x An array of positions on the unit circle in radians
#' @param alpha a confidence limit.
#'
#' @return a list containing
#' \describe{
#'   \item (mu) (circular number of mean directions)
#'   \item (ventana) (circular number of window)
#'   \item (muini) (circular number initial value of mu )
#'   \item (conc) (estimate of the concentration parameter defined in Ko (1992).)
#'   \item (malpha) (1 - alpha)
#'   \item (distance) (array of distance to be compared to corte)
#'   \item (corte) (cut value)
#'   \item (isout) (sequential indexes of outlier wherein 0 is an outlier and a 1 is not)
#'   \item (porcout) (proportion of outliers in sample)
#'  }
#' @references
#' Sau, M.F., and D. Rodriguez. 2018. "Minimum distance method for
#' directional data and outlier detection."  Advances in Data Analysis
#' and Classification 12:587-603.
#'
#' Ko D (1992) Robust estimation of the concentration parameter of the von Mises-Fisher
#' distribution. Ann Stat 20:917–928
#'
#' Whitfield, P. H. and D. H. Burn (2025 in review). "Screening Annual Maxima and
#' Peaks-Over-Threshold Series for Flood Frequency Analysis."

#'
#' @import CircStats
#' @import circular
#' @import movMF
#'
#' @author Mercedes Fernandez Sau, Daniela Rodrigues, Paul Whitfield
#'
#' @examples
#' data1 <- circular::rwrappedcauchy(100, mu=circular(0), rho=0.7, control.circular=list(units="degrees"))
#  alpha <- 0.05
#' result <- SR_hstat(data1, alpha)
#' result$isout # lists the outliers with 0 being an outlier and 1 not.


######################### Text from original set of functions
#FUNCTIONS
##M?nima distancia para un h m?nimo
#Minimum distance for an h min
# S&R was originally a series of separate functions, these are now
# nested into three main functions
################################################
# the following is the original example code. This is now the functions example,
# and the main actions of the function.
#
## Example
## the data
##data1 <- rwrappedcauchy(100, mu=circular(0), rho=0.7, control.circular=list(units="degrees"))
## alpha<-0.05
## malpha<-1-alpha

## MDEVM <- mindisthminconc(data1) # minimum distance estimator under von mises distribution
##     estmu <- MDEVM$mu
##     estconc <- MDEVM$conc
## detection <- vmd2deteccion(data1,estmu)
## distance <- detection$norma
## corte <- cortando(estconc,malpha)
##     isout<- 1*(distance <= corte)
## isout # 0 is an outlier
## porcout<-1-mean(isout)
## porcout  # outliers proportions in de sample

# so there are only three functions that are called
# mindisthminconc(data1)
# vmd2deteccion(data1,estmu)
# cortando(estconc,malpha)

# all the others get called only within middisthminconc
# now nested within three main functions

SR_hstat <- function(x, alpha = 0.05){
  
  ###############################################
  ##estimador de m?nima distancia
  #estimator of minimum distance
  ###############################################
  
  ################################################## minimum distance function
  
  mindisthminconc <- function(x)
  {
    
    ini <- inicialesrob2(x)
    
    conc <- ini$conc
    muini <- ini$mu
    options(warn = -1)  ### added to suppress coercion warning
    ini <- c(muini,1)
    options(warn = 0)
    
    
    ## optim is a general purpose optimization (stats)
    res <- optim(ini, method = "L-BFGS-B",
                 intargumento,
                 kk = conc,
                 xm = x,
                 lower = c(0,0.001),
                 upper = c(2*pi,1.41))
    
    resultado <- list(mu=res$par[1], ventana=res$par[2],
                      muini=muini, conc=conc)
    return(resultado)
  }
  
  ############################################ nucepan
  ##nucleo de epanechnikov
  # core
  # called in nucleo
  nucepan <- function(x)
  {
    
    a <- 1.5*(1-x*x)
    kepan <- a*(abs(x) < 1)
    kepan
  }
  
  ############################################ nucleo
  ##me da el Kh
  # called in suavizado
  nucleo <- function(punto, xt, ventana)  ### nucleo = core punto = spot ventana = window
  {
    
    puntobis <- cbind(cos(punto), sin(punto))
    xtbis <- cbind(cos(xt), sin(xt))
    prodinter <- as.vector((xtbis) %*% t(puntobis))
    arg <- (1 - prodinter)/(ventana * ventana)
    nucleovec <- nucepan(arg)
    nucleovec
  }
  ############################################# suavizado
  ## suavizado para epanechnikov (fsombrero)
  # smoothed for  Epanechikov[kernel] fhat
  # called in argumento
  suavizado <- function(punto, x, ven)  ## suavizado = smoothed
  {
    ## funcion de la ventana
    # window function
    
    intk <- 12 / 5
    lambda <- intk * sqrt(2)
    cven <- 1 / (lambda * ven)
    wei <- nucleo(punto, x, ven)
    suav <- cven * mean(wei, na.rm = T)
    suav
  }
  
  ############################################## argumento
  # called in intargumento
  argumento <- function(punto, cc, x, conc)  ## argumento = argument
  {
    mu <- cc[1]
    ven <- cc[2]
    
    arg <- (suavizado(punto, x, ven) - dvm(punto, mu, conc))  ## dvm(CircStats) return the von Mises at a particular value
    
    argum <- arg * arg
    
    argum
  }
  
  
  
  ############################################## intargumento
  # called in mindisthminconc
  intargumento <- function(cc, xm, kk)
  {
    mm <- cc[1]
    hh <- cc[2]
    grillas <- seq(0, 2*pi, length=1000)   ## grillas = grids
    resul <- grillas
    for(j in 1:1000)
    {
      resul[j] <- argumento(grillas[j], cc, xm, kk)
    }
    res <- mean(resul)
    res
  }
  
  ################################################  inicialesrob2
  # called in mindisthminconc
  inicialesrob2 <- function(muestra)  ## muestra = sample
  {
    
    muini <- median(muestra)
    muini <- ((muini<2*pi) * (muini>0) * muini) +
      ((2*pi + muini) * (muini<0)) + ((muini - 2 * pi) * (muini>2 * pi))
    mediana <- cbind(cos(muini), sin(muini))
    muestrat <- cbind(cos(muestra), sin(muestra))
    prodinter <- median(as.vector((mediana) %*% t(muestrat)))
    
    conc <- c2menos(prodinter)   ## c2menos is a S&R function
    resultado <- list(mu = muini, conc = conc)
    
    return(resultado)
  }
  
  # original code read files for V1 and V2 here defined internally
  ##trabajokofunc2menos <- read.table("~/My Dropbox/daniela/trabajosfuturos/mindistesf/simulacion/circulo/trabajokofunc2menos.txt", quote="\"")
  ##trabajokofunc2menos <- read.table("trabajokofunc2menos.txt", quote="\"")
  
  V1 <- c(0.010, 0.020, 0.030, 0.040, 0.050, 0.060, 0.070, 0.080, 0.090, 0.100, 0.110,
          0.120, 0.130, 0.140, 0.150, 0.160, 0.170, 0.180, 0.190, 0.200, 0.210, 0.220,
          0.230, 0.240, 0.250, 0.260, 0.270, 0.280, 0.290, 0.300, 0.310, 0.320, 0.330,
          0.340, 0.350, 0.360, 0.370, 0.380, 0.390, 0.400, 0.410, 0.420, 0.430, 0.440,
          0.450, 0.460, 0.470, 0.480, 0.490, 0.500, 0.510, 0.520, 0.530, 0.540, 0.550,
          0.560, 0.570, 0.580, 0.590, 0.600, 0.610, 0.620, 0.630, 0.640, 0.650, 0.660,
          0.670, 0.680, 0.690, 0.700, 0.710, 0.720, 0.730, 0.740, 0.750, 0.760, 0.770,
          0.780, 0.790, 0.800, 0.810, 0.820, 0.830, 0.840, 0.850, 0.860, 0.870, 0.880,
          0.890, 0.900, 0.905, 0.910, 0.915, 0.920, 0.925, 0.930, 0.935, 0.940, 0.945,
          0.950, 0.955, 0.960, 0.965, 0.970, 0.975, 0.980, 0.985, 0.990, 0.991, 0.992,
          0.993, 0.994, 0.995, 0.996, 0.997)
  
  V2 <- c(0.0100,  0.0200,  0.0300,  0.0400,  0.0501,  0.0601,  0.0702,  0.0803,
          0.0904,  0.1006,  0.1107,  0.1210,  0.1312,  0.1415,  0.1519,  0.1623,
          0.1728,  0.1833,  0.1939,  0.2046,  0.2153,  0.2262,  0.2371,  0.2481,
          0.2592,  0.2703,  0.2816,  0.2930,  0.3046,  0.3162,  0.3280,  0.3399,
          0.3519,  0.3641,  0.3765,  0.3890,  0.4017,  0.4146,  0.4277,  0.4410,
          0.4545,  0.4683,  0.4822,  0.4964,  0.5109,  0.5257,  0.5408,  0.5562,
          0.5719,  0.5879,  0.6043,  0.6212,  0.6384,  0.6561,  0.6742,  0.6929,
          0.7120,  0.7318,  0.7521,  0.7731,  0.7948,  0.8173,  0.8405,  0.8647,
          0.8897,  0.9158,  0.9430,  0.9713,  1.0010,  1.0321,  1.0648,  1.0992,
          1.1356,  1.1741,  1.2149,  1.2585,  1.3050,  1.3550,  1.4088,  1.4671,
          1.5306,  1.6001,  1.6768,  1.7619,  1.8573,  1.9651,  2.0884,  2.2311,
          2.3990,  2.5998,  2.7160,  2.8452,  2.9897,  3.1525,  3.3374,  3.5492,
          3.7944,  4.0812,  4.4212,  4.8304,  5.3317,  5.9597,  6.7686,  7.8485,
          9.3619, 11.6337, 15.4221, 23.0017, 25.5287, 28.6874, 32.7489, 38.1643,
          45.7461, 57.1190, 76.0742)
  
  trabajokofunc2menos <- data.frame(V1, V2)
  
  ################################################ c2menos
  # only called in inicialesrob2
  c2menos <- function(tt)
  {
    
    dimtr <- dim(trabajokofunc2menos)[1]
    dimtr1 <- dimtr-1
    aa <- rep(0,dimtr)
    aa[1] <- trabajokofunc2menos[1,2] * (tt <= trabajokofunc2menos[1,1])
    aa[dimtr] <- trabajokofunc2menos[dimtr,2] * (tt > trabajokofunc2menos[dimtr,1])
    for(i in 2:dimtr1)
    {
      aa[i] <- trabajokofunc2menos[i,2] * (tt > trabajokofunc2menos[(i-1),1]) * (tt <= trabajokofunc2menos[i,1])
    }
    sum(aa)
    
  }
  
  #######################################
  
  ################################################## end of main function mindisthmin
  ################################################## minimum distance function end
  
  
  ################################################### main function vmd2deteccion
  ##Minima distancia para una Von Mises Fisher en dim 2 con k prefijados
  
  vmd2deteccion <- function(datos, mu)  ## datos = data
  {
    puntobis <- cbind(cos(mu), sin(mu))
    xtbis <- cbind(cos(datos), sin(datos))
    distancias <- as.vector((xtbis) %*% t(puntobis))
    norma <- 2 - 2 * distancias
    resultado <- list(norma = norma)
    return(resultado)
  }
  ################################################### main function vmd2deteccion ends
  
  ################################################### main function cortando
  cortando <- function(kk, poda)  ##cortando = cutting poda = pruning
  {
    ################################################
    # called in cortando with integrate
    intfun <- function(t,k)
    {
      exp(k * t) * (1 - t^2) ^ (-1/2)
    }
    
    intfun2 <- function(aa,kk,poda)  ## aa is from c2menos
    {
      cte <- integrate(intfun, -1, 1, k=kk)$value
      integrate(intfun, -1, aa, k=kk)$value - (cte * (1 - poda))
    }
    y1mpoda <- uniroot(intfun2, lower = -0.9999, upper = 1, kk=kk, poda=poda)$root
    # one dimensional root (zero) finding search from lower to upper
    2 - 2 * y1mpoda
    
  } # intfun was moved into cortando
  
  ################################################### main function cortand ends
  ##############################################
  # main actions of function
  malpha <- 1-alpha
  
  
  
  MDEVM <- mindisthminconc(x) # minimum distance estimator under von mises distribution
  estmu <- MDEVM$mu
  estconc <- MDEVM$conc
  
  
  detection <- vmd2deteccion(x,estmu)
  distance <- detection$norma
  
  corte <- cortando(estconc,malpha)
  
  isout<- 1*(distance <= corte)
  ## isout # 0 is an outlier
  
  porcout<-1-mean(isout)
  ## porcout  # outliers proportions in de sample
  
  
  result <- list (MDEVM$mu, MDEVM$ventana, MDEVM$muini, MDEVM$conc,
                  malpha, distance, corte, isout, porcout)
  names(result) <- c("mu", "ventana", "muini", "conc",
                     "malpha", "distance", "corte", "isout", "porcout")
  
  return (result)
  
}
