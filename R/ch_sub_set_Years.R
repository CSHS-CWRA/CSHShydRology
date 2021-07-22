#' Sub-samples a vector every n places
#'
#' @description Sub-samples a vector every n places and produces the position and label 
#' for the subset. 
#' 
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