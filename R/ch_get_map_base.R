#'@title ch_get_map_base
#'
#'@description  Prepares for mapping by acquiring the base map and ancillary data: 
#' boundaries and rivers.  The maps are obtained using OpenStreetMap::openmap which 
#' originally accessed the following map types:
#' "osm", "osm-bw", "maptoolkit-topo", "waze", "bing", "stamen-toner", 
#' "stamen-terrain", "stamen-watercolor", "osm-german", "osm-wanderreitkarte", 
#' "mapbox", "esri", "esri-topo", "nps", "apple-iphoto", "skobbler", 
#' "hillshade", "opencyclemap", "osm-transport", "osm-public-transport", 
#' "osm-bbike", "osm-bbike-german".
#' 
#' In April 2022 access all of these failed, limiting the available 
#' maps to: one of "osm", "bing", "stamen-toner",
#' "stamen-terrain", "stamen-watercolor", "apple-iphoto", "opencyclemap",
#' "osm-transport", "osm-public-transport".
#' 
#' In January 2023, ne_download failed as it produced an incorrect url.
#' 
#' Access to "nps" [default] was added as a work around until OpenstreetMap is updated.
#' 
#' "nps": This layer presents the U.S. National Park Service (NPS) Natural Earth 
#' physical map at 1.24km per pixel for the world and 500m for the coterminous 
#' United States.
#' 
#'@param maplat vector of latitudes (2)
#'@param maplong vector of longitudes (2)
#'@param map_proj map projection currently NA/"latlong" or "albers"/"equalarea"
#'@param map_directory directory where map data will be stored; will be 
#' created if it does not exist.
#'@param map_type map type: select one of  \option{osm},  \option{bing},  
#'\option{stamen-toner}, \option{stamen-terrain}, \option{stamen-watercolor},
#'\option{apple-iphoto}, \option{opencyclemap}, \option{osm-transport},
#'\option{osm-public-transport}, \option{nps [default]},
#'
#'@return Returns a list containing:
#'\describe{
#'\item{map_d}{map data directory}
#'\item{plines10}{provincial and state boundaries}
#'\item{rlines10}{rivers and lakes}
#'\item{map_proj}{projection used}
#'\item{latitude}{bottom and top latitudes}
#'\item{longitude}{east and west longitudes}
#'}
#' 
#'@importFrom  rnaturalearth ne_load ne_download
#'@author Paul Whitfield
#'@export
#'@examples \donttest{
#'# Note: example not tested automatically as it is very slow to execute due to the downloading
#'latitude <- c(48.0,  61.0)
#'longitude <- c(-110.0, -128.5)
#'mapdir <- tempdir()
#'# get map data
#'m_map <- ch_get_map_base(latitude,longitude, 
#'                      map_proj = "Albers", 
#'                      map_directory = mapdir, 
#'                      map_type = "nps")
#'}                      

ch_get_map_base <- function(maplat, maplong, 
                            map_proj = NA, 
                            map_directory = ".", 
                            map_type = "nps")
  {
  
  uleft <- c(maplat[2],maplong[2])
  lright <- c(maplat[1],maplong[1])
  
  cdn_latlong = "+proj=longlat"
  
# form a projection string based on latitude and longitude  
  cdn_aea <- paste("+proj=aea +lat_1=",maplat[1],
                          " +lat_2=", maplat[2],
                          " +lat_0=", (maplat[1] + maplat[2])/2,
                          " +lon_0=", (maplong[1] + maplong[2])/2,
                          " +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83",
                          " +units=m +no_defs", sep = "")
  

 
  if (is.na(map_proj)) map_proj <- cdn_latlong 
  if (tolower(map_proj) == "latlong") map_proj <- cdn_latlong
  if (tolower(map_proj) == "albers") map_proj <- cdn_aea
  if (tolower(map_proj) == "equalarea") map_proj <- cdn_aea
  
  # get basic map
  
  ##################  work around 2022-05-25  OpenStreetMap fails to access "nps"

  if (map_type == "nps") {
    nps_file <- "https://server.arcgisonline.com/ArcGIS/rest/services/World_Physical_Map/MapServer/tile/{z}/{y}/{x}.jpg"
    result <- ch_test_url_file(nps_file)
    if (result == "error" | result == "warning") {
      stop("nps file does not exist")
    } 
    
    map_a <- OpenStreetMap::openmap(uleft, lright, minNumTiles = 3,
              type = nps_file)
    
  }
  
  if (map_type != "nps"){
  
   map_a <- OpenStreetMap::openmap(uleft, lright,
             type = map_type,
             minNumTiles = 9L)
  }

   # change projection
   map_d <- OpenStreetMap::openproj(map_a, projection = map_proj)
   
#####################################################################
   # if map_directory does not exist create it
   
   if (!dir.exists(map_directory)) {
     
     print(paste("Creating a new directory for map data", map_directory))
     
     dir.create(map_directory)
     setwd(map_directory)
   }
##################################################################  
# if files don't exist get zip files and unzip


    if (!file.exists("ne_10m_admin_1_states_provinces_lakes.prj"))  {
      
      cadd <- "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_1_states_provinces_lakes.zip"
      
      result <- ch_test_url_file(cadd)
      if (result == "error" | result == "warning") {
        stop("natural earth provinces and lakes file does not exist")
      } 
      
      zip_file = tempfile()
      utils::download.file(cadd, zip_file)
      utils::unzip(zip_file, unzip = "unzip", exdir = map_directory)
    }  
       
    if (!file.exists("ne_10m_rivers_lake_centerlines.prj"))
    {
      radd <- "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_rivers_lake_centerlines.zip"
      result <- ch_test_url_file(radd)
      if (result == "error" | result == "warning") {
        stop("natural earth rivers file does not exist")
      } 
      
      zip_file = tempfile()
      utils::download.file(radd, zip_file)
      utils::unzip(zip_file, unzip = "unzip", exdir = map_directory)
      }
    
   
     plines10 <- rnaturalearth::ne_load(scale = 10, 
                                      type = "states", 
                                      category = 'cultural', 
                                      destdir = map_directory, 
                                      returnclass = "sf")
   
      
    rivers10 <- rnaturalearth::ne_load(scale = 10, 
                                       type = "rivers_lake_centerlines", 
                                       category = 'physical', 
                                       destdir = map_directory, 
                                       returnclass = "sf")
    
   

    
#####################################################################
# if map_directory does not exist create it and download data
  
  if (!dir.exists(map_directory)) {
    
    print(paste("Creating a new directory for map data", map_directory))
    
    dir.create(map_directory)
    setwd(map_directory)
    
    plines10 <- rnaturalearth::ne_download(scale = 10, 
                                           type = "states", 
                                           category = 'cultural', 
                                           destdir = map_directory, 
                                           returnclass = "sf")
    rivers10 <- rnaturalearth::ne_download(scale = 10, 
                                           type = "rivers_lake_centerlines", 
                                           category = 'physical',
                                           destdir = map_directory, 
                                           returnclass = "sf")
    
    }
    
    map_data <- list(map_d, plines10, rivers10, map_proj, maplat, maplong)
    names(map_data) <- c("map_d", "plines10", "rivers10", "map_proj", 
                         "latitude","longitude")
    return(map_data)
  
}



