#'  Converts tidyhydat daily flow tibble data to ECDE format
#'

#' @description Accessing daily flow data using \pkg{tidyhydat} is quick and efficient. However, it sometimes
#' conflicts with other functions as \pkg{tidyhydat} changes variable names and some default entries. This function converts a tibble obtained 
#' from a \pkg{tidyhydat} tibble to a dataframe with standard Environment and Climate Change Canada Data Explorer (ECDE) headings.
#'
#'
#' @param filename Tibble of daily flows retrieved using \pkg{tidyhydat} function \code{hy_daily_flows}.

#' @author Paul Whitfield <paul.h.whitfield@gmail.com>
#'
#' @return A dataframe or a list of with formats consistent with datafiles from ECDE 
#' @export
#'
#' @examples
#'mdata <- hy_daily_flowsd(station_number=c("05CK004"))
#'m_data <- ch_tidyhydat_ECDE(mdata)
#'
#'mdata <- hy_daily_flows(station_number=c("05CK004","08MF005","05BB001"))
#'mnew <- ch_tidyhydat_ECDE(mdata)
#'str(mnew[[1]])
#'str(mnew[[2]])
#'str(mnew[[3]]) 
#'#note the order is in increasing alphabetical order
#'
#'

ch_tidyhydat_ECDE <- function (data) {
  
  ndata <- data.frame(data)   # untibble
 
  ndata$Parameter[ndata$Parameter=="Flow"]<-1  #revert parameter to internal codes
  ndata$Parameter[ndata$Parameter=="Level"]<-2
  ndata$Parameter <-as.integer(ndata$Parameter)
 
  ndata$Symbol[is.na(ndata$Symbol)]<-""  #remove the NA that replaced "" in original
  
  result <-data.frame(ndata[,1],ndata[,3],ndata[,2],ndata[,4],ndata[,5])
  names(result) <-c("ID","PARAM","Date", "Flow", "SYM")  #assign the original names
  
  nstations <-unique(result$ID)

  if (length(nstations)==1) {
 
           return (result)
  }
  
  if(length(nstations!=1)){
  print(paste("Original tibble contained ",length(nstations)," stations. a list of dataframes is returned"))
   
    resulta <- split(result,result$ID)
    return(resulta)}
}