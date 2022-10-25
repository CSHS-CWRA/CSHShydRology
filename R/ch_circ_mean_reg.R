#' Calculates the circular mean, median, and regularity
#'  
#'@description Calculate the circular mean, median, and regularity using a year of 365 days.
#'  Days of year are converted to degrees
#'  internally, results are returned as positive days of year
#'  
#' @param dataframe a dataframe of day year of event; can be amax or pot. 
#'  
#' @return Returns a list of the following statistics
#'   \item{n}{number of samples}
#'   \item{mean}{circular mean of array}
#'   \item{median}{circular median of array}
#'   \item{rho}{regularity or mean resultant length}
#'  
#' @references 
#'  Pewsey, A., M. Neuhauser, and G. D. Ruxton. 2014. Circular Statistics in R, 
#'  192 pp., Oxford University Press.
#'  Whitfield, P. H. 2018. Clustering of seasonal events: A simulation study using 
#'  circular methods. Communications in Statistics - Simulation and Computation 47(10): 3008-3030.
#'  Burn, D. H., and P. H. Whitfield. 2021*. Changes in the timing of flood events resulting 
#'  from climate change.
#'
#' @import circular
#' @export
#' @seealso  \code{\link{ch_sh_get_amax}}   
#' @examples 
#' data(CAN05AA008)
#' am <- ch_sh_get_amax(CAN05AA008)
#' m_r <- ch_circ_mean_reg(am)

ch_circ_mean_reg <- function(dataframe){
  doys <- dataframe$doy
  days <- dataframe$days
  n <- length(doys)
  doys <- doys / days * 360  # doy as degrees
  
  
 x <- circular::circular(doys, units = "degrees", zero = pi/2, rotation = "clock")
  
 meanday <-  circular::mean.circular(x)
 if (meanday < 0) meanday <- 360 + meanday
 medianday <- circular::median.circular(x)
 if (medianday < 0) medianday <- 360 + medianday
 rho <- circular::rho.circular(x)
 
 result <- list(n, as.numeric(meanday)*365/360, as.numeric(medianday)*365/365, rho)
 names(result) <- c("n", "mean", "median", "regularity")
 
  return(result)
}