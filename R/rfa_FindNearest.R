###############################################################################
#' Find the nearest sites
#'
#' Return a dataset including only the nearest site to a target.
#' The target has a zero distance and will be at the first column of the output.
#' 
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#'
#' @param x Dataset with sites in columns and time in row.
#'
#' @param distance Distances with the target.
#'
#' @param n Number of sites to keep.
#'
#' @param super.distance,super.n Distance and number of sites for the super
#'   region. This allows to pre-filter sites according to a complementary
#'   measure of similarity.
#'
#' @param row Logical. Should only the row of the nearest site be kept.
#'   Must be used with correlation or square matrix.
#'
#' @section References:
#'
#' Mostofi Zadeh, S., Burn, D.H., 2019. A Super Region Approach to Improve
#'   Pooled Flood Frequency Analysis. Canadian Water Resources Journal
#'   0, 1–14. https://doi.org/10.1080/07011784.2018.1548946
#'
#' Durocher, M., Burn, D.H., Mostofi Zadeh, S., 2018. A nationwide regional
#'   flood frequency analysis at ungauged sites using ROI/GLS with copulas and
#'   super regions. Journal of Hydrology 567, 191–202.
#'   https://doi.org/10.1016/j.jhydrol.2018.10.011
#'
#' @export
#' 
#' @seealso \link{DataWide}
#'
#' @examples
#' 
#' ## Organize data in the proper format
#' attach(flowAtlantic)
#' ams$year <- format(ams$date, '%Y')
#' xmat <- DataWide(ams ~ id + year, ams)
#' dim(xmat)
#'
#' ## Find the nearest neirghbors using great-circle distance.
#' ## Note that it is a good habit to make sure that the column names matches
#' 
#' h <- GeoDist(info[colnames(xmat), c('lon','lat')])
#' xmat0 <- FindNearest(xmat, h[1,], 5)
#' dim(xmat0)
#'
#' ## Distance based on catchment characteristics
#' charac <- scale(log(info[,c('area','map')]))
#' h.super <- as.matrix(dist(charac))
#'
#' ## Find among the 20 sites with the ones with the most similar
#' ## characteristics and keep the 5 nearest neighbors of the target.
#' xmat0 <- FindNearest(xmat, h[1,], 5, h.super[1,], 20)
#' dim(xmat0)
#'
#' ## subsample a distance or correlation matrix
#' xmat0 <- FindNearest(h, h[1,], 5, row = TRUE)
#' dim(xmat0)
#'
FindNearest <-
  function(x,
           distance,
           n,
           super.distance = NULL,
           super.n = NULL,
           row = FALSE){

  ## Put a Inf distance to site that are not part of the super region
  if(!is.null(super.distance)){
    cid <- order(super.distance)[1:super.n]
    distance[-cid] <- Inf
  }

  ## find nearest site
  cid <- order(distance)[1:n]

  ## Remove the farthest sites
  if(row){
    ans <- x[cid,cid]
  } else{
    ans <- x[,cid]
  }

  return(ans)
}