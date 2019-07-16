##############################################################################
#' Transpose dataset to a wide format
#'
#'  Return a matrix where columns correspond to data.
#'  Each row represent a specific time and missing values are added where need.
#'  The pivoted variable should be a numerical value.
#'
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#'
#' @param x Dataset in long format. First column should be the value 
#'   to transpose.
#'   The second and third columns are respectively site and time variable.
#'   If the time variable is omitted, the pivot will ignore it and all columns
#'   will start at the first position.
#'
#' @param form Formula that specifies the site and time variable.
#'   Must have the form: `value ~ site + time`.
#'
#' @export
#'
#' @examples
#' 
#' data(flowAtlantic)
#' xd <- flowAtlantic$ams 
#' 
#' ## transpose by year
#' xd$year <- format(xd$date, '%Y')
#' DataWide(ams ~ id + year, xd)[60:70,1:5]
#' 
#' ## Without time
#' DataWide(ams ~ id, xd)[1:6,1:5]
#'
DataWide <- function(x,...) UseMethod('DataWide',x)

#' @export
#' @rdname DataWide
DataWide.data.frame <- function(x, order.time = TRUE, order.site = FALSE){

  ## Verify if there is a time variable
  if(ncol(x) == 3){
    ind <- FALSE

    ## verify that there is no duplicate
    if(max(table(x[,2],x[,3]))>1)
      stop('There is some duplicates')

  } else if(ncol(x) == 2){
    ind <- TRUE
  } else{
    stop('Wrong number of variables')
  }

  ## Verify that there is no missing value in the data
  ## including site and time
  x <- na.omit(x)

  ##---------------------------------##
  ## reshape data in matrix wide format
  ##---------------------------------##

  ## Extract level for the site and time id
  site.un <- unique(x[,2])
  site.name <- as.character(site.un)
  site.order <- order(site.un)

  nsite <- length(site.name)

  if(nsite < 2)
    stop('Must have more than one site')

  ## Transform site and time id in integer
  x[,2] <- as.integer(factor(as.character(x[,2]), levels = site.name))

  if(ind){
    vtime <- lapply(split(x[,2],x[,2]),seq_along)
    x <- cbind(x,unsplit(vtime, x[,2]))

  } else {

    time.un <- unique(x[,3])
    time.name <- as.character(time.un)
    time.order <- order(time.un)

    x[,3] <- as.integer(factor(as.character(x[,3]), levels = time.name))
  }

  ## split the data in list
  xlst <- split(x, x[,2])

  ## allocate memory
  nc <- length(xlst)
  nr <- max(x[,3])
  xmat <- matrix(NA, nr, nc)

  ## Affect the value to the wide format
  for(jj in 1:nc)
    xmat[xlst[[jj]][,3],jj] <- xlst[[jj]][,1]

  ## Format row and columns
  colnames(xmat) <- site.name

  if(ind){
    rownames(xmat) <- 1:nr
  } else{
    rownames(xmat) <- time.name
  }

  ## If needed, sort the rows and columns
  if(order.site)
    xmat <- xmat[ ,site.order]

  if(order.time & !ind)
    xmat <- xmat[time.order,]

  return(xmat)
}

#' @export
#' @rdname DataWide
DataWide.formula <- function(form, x, ...)
  DataWide(model.frame(form, x), ...)

#' @export
#' @rdname DataWide
DataWide.matrix <- function(x, ...)
  DataWide(as.data.frame(x), ...)