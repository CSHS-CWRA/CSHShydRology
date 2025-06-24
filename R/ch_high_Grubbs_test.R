#'  Grubbs test for high events
#'
#' @description
#' Checks for outliers among the highest fifteen events, assumes the data are
#' normally distributed and uses the Grubbs test to determine if the highest
#' value is an outlier. If it is an outlier the next largest is tested until
#' the value is not an outlier, or 15 values have been tested.
#'
#' @param amax A vector of the annual maxima
#'
#' @return result A vector of the same length as amax with a 1 for those values
#' that are high outliers else zero.
#'
#' @author Paul Whitfield
#'
#' @importFrom outliers grubbs.test
#' @export
#'
#' @examples
#' set.seed(1234)
#' x = rnorm(10)
#' x[11] <- 15
#' x[12] <- 35
#' ch_high_Grubbs_test(x)

ch_high_Grubbs_test <- function(amax){

tmax <- amax
# create a sequential index
tindex <- c(1:length(tmax))
# create a vector with all values being zero
tout <- rep(0,length(tmax))

tdata <- data.frame(tindex, tmax, tout)
# sort the dataframe based on amax
tdata1 <- tdata[order(tdata$tmax, decreasing = TRUE), ]

for (li in 1:15) {
  interim <- outliers::grubbs.test(tdata1$tmax[li:length(tdata1[,1])], type = 10)
#  print(interim$p.value)

  if (interim$p.value <= 0.05) tdata1$tout[li] <- 1
  if (interim$p.value > 0.05) break
}

# sort the result into original sequence
tdata  <- tdata1[order(tdata1$tindex, decreasing = FALSE), ]
return (tdata)
}
