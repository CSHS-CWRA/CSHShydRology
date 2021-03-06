#' @title Helper function for selecting points for an axis so not all are necessary
#'
#' @description Sub-samples a vector every n places. Many times there are so many 
#' years the labels on the plot overlap. This function returns the position and label 
#' for the subset. The function can be used on any type of simple array. 
#' @param years a vector of years
#' @param n sample size
#' @return a list containing:
#' \describe{
#' 	\item{position}{array of axis positions}
#' 	\item{label}{array of labels}
#' 	}
#' @export
#' @author Paul Whitfield <paul.h.whitfield@gmail.com>
#' @examples
#' myears <- c(1900:2045)
#' myears <- ch_sub_set_Years(myears, 20)
#' myears
#'
#' a <- LETTERS
#' my_alpha <- ch_sub_set_Years(a, 5)
#' my_alpha

ch_sub_set_Years <- function(years, n) {
  pts <- c(1:length(years))
  pts <- pts[1:(length(years) / n) * n]
  
  years <- years[pts]
  
  result <- list(pts, years)
  names(result) <- c("position", "label")
  return(result)
}