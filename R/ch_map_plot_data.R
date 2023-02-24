#'  Generate a map for a defined area
#'  
#'@description  Generates a map for a defined area. Options to plot station 
#'  locations, magnitudes, trends etc.  Watershed boundaries and add user 
#'  defined labels. See article in CWRA "Water News" Spring 2023. 
#'  The elements are added to the map in an order that puts the symbols on top.
#'  Large basins, WSC basins, rivers, Provinces, then data symbols.  Labels are 
#'  added last.  
#'  
#'@param map_data a list produced by the function ch_get_map_base()
#'@param locations a dataframe with longitude, latitude. a third column may contain indexes of symbol types.
#'@param lo_pch plotting symbols: default is lo_pch=c(19,19)
#'@param lo_col plotting symbol colours: default is lo_col= c("black","black")
#'@param lo_bg plotting symbol background (20-24) default is "white" 
#'@param lo_cex plot symbol size default is 0.8 
#'@param lo_text legend title text, default is "Station"
#'@param lo_title names for different items in legend, default is "Location"
#'
#'# adding large basin boundaries
#'@param lb_basins a list with basin shapefiles
#'@param lb_border colour for watershed boundaries: default is "darkred"
#'@param lb_lwd width of watershed boundary
#'@param lb_clip clip basins at map edge, default is TRUE
#'
#'# adding WSC basin boundaries
#'@param sb_basins a list with basin shapefiles
#'@param sb_border colour for watershed boundaries: default is "darkred"
#'@param sb_lwd width of watershed boundary
#'@param sb_clip clip basins at map edge, default is FALSE
#'
#'# adding trends
#'@param trends a dataframe with four columns (Longitude, Latitude, trend, and pvalue) the trend and pvalue 
#'  are in original units such as slope and probability.  These are converted to indexes (1, 2, 3) decreasing, no trend, 
#'  increasing and (1, 2) not significant and significant 
#'@param tr_pch plotting symbols: default is tr_pch = c(25, 20, 24)
#'@param tr_col plotting symbol colours: default is tr_col = c("red","black","darkblue")
#'@param tr_cex  plotting symbol size for non-significant and significant:default is tr_cex = c(0.50, 1.0, 0.0
#'@param tr_p trend significance level: default is tr_p = 0.05
#'
#'# adding variable with colour gradient
#'@param var a dataframe with three columns (Longitude, Latitude, value)
#'@param vr_pch a symbol to plot: default is vr_pch = 22
#'@param vr_cex  size for plot symbol: default is vr_cex = 2.0
#'@param vr_text a label to include in the legend
#'@param vr_range set ranges for color gradient  default is (0, 1)
#'@param vr_colors colours for gradient default is ("darkred", "red","white","green", "darkgreen")
#'
#'# adding variable with symbol diameter
#'@param sc_var a dataframe with three columns (Longitude, Latitude, value)
#'@param sc_pch a symbol to plot: default is vr_pch = 20
#'@param sc_text a label to include in the legend  default is ""
#'@param sc_range set ranges for color gradient default is (0, 1) if not scaled against largest
#'@param sc_color symbol colour default is "magenta"
#'
#'# adding rivers
#'@param rivers  plot rivers in blue: default is TRUE
#'@param boundaries plot provincial boundaries: default is TRUE
#'
#'# adding provincial boundaries
#'@param plabels add the names of provinces: default is TRUE
#'@param pl_cex  adjusts size of provincial labels: default is 1.0
#'@param legend add a legend to the plot: default is FALSE
#'@param le_text legend categories: default is NA. 
#'@param x_labels a dataframe with seven columns (long, lat, pos, cex, font, col, text). Each row provides details for a single label : default is NA
#'@param tr_ltext text for legend
#'@param tr_lsz symbol sizes for trends default is c(1,0.40, 0.40, 0.40, 1)
#'@param ... Other mapping parameters  
#'
#'@return  Produces a map on an output device.
#'  
#'@import sf
#'@import dplyr
#'@author Paul Whitfield
#'@export
#'@examples \donttest{
#'# Note: example not tested automatically as it is very slow to execute due to the downloading
#'# get base map
#'latitude <- c(48.0,  61.0)
#'longitude <- c(-110.0, -128.5)
#'mapdir <- tempdir()
#'# get map data
#'m_map <- ch_get_map_base(latitude,longitude, 
#'                      map_proj = "Albers", 
#'                      map_directory = mapdir, 
#'                      map_type = "nps")
#' # add symbols
#' stations <- HYDAT_list
#' stations <- stations[,c("Longitude", "Latitude")]
#' stations <- na.omit(stations)
#' ch_map_plot_data(m_map, sc_var = stations, sc_text = "Years")
#' }                    




##########################################################
ch_map_plot_data <- function(map_data, 
                             locations = NULL, 
                             lo_pch = 19,  
                             lo_col = "black", 
                             lo_bg = "white", 
                             lo_cex = 0.8, 
                             lo_text = "Station",
                             lo_title = "Location",
                             lb_basins = NULL, 
                             lb_border = "darkred", 
                             lb_lwd = 2.,
                             lb_clip = TRUE,
                             sb_basins = NULL, 
                             sb_border = "darkred", 
                             sb_lwd = 1.,
                             sb_clip = FALSE,
                             trends = NULL, 
                             tr_pch =c(25, 20, 24), 
                             tr_col = c("red","black","darkblue"), 
                             tr_cex = c(0.50, 1.0, 0.0), tr_p = 0.05, 
                             tr_ltext = c("Significant Increase", "Increase", "No Change", "Decrease", "Significant Decrease"),
                             tr_lsz = c(1,0.40, 0.40, 0.40, 1),
                             var = NULL, 
                             vr_pch = 22, 
                             vr_cex = 2.0, 
                             vr_text = NA, 
                             vr_range = c(0,1),
                             vr_colors = c("darkred", "red","white","green", "darkgreen"),
                             sc_var = NULL,
                             sc_pch = 20,
                             sc_range = c(0,1),
                             sc_text = "",
                             sc_color = "magenta",
                             rivers = TRUE, 
                             boundaries = TRUE, 
                             plabels = TRUE, 
                             pl_cex = 1.0,
                             legend = FALSE, 
                             le_text = NA,
                             x_labels = NULL,
                             ...)
{
  
  sf::sf_use_s2(FALSE)
  
  cdn_latlong = "+proj=longlat"
  # proj4string for Canadian Albers equal area projection
  cdn_aea = "+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-110 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
  
  
  # pull apart the map_data list
  map_d    <- map_data[[1]]
  plines10 <- map_data[[2]]
  rivers10 <- map_data[[3]]
  map_proj <- map_data[[4]]
  maplat   <- map_data[[5]]
  maplong  <- map_data[[6]]
  ####################################  plot base map
  
  
  plot(map_d)
  
  ################################################# add large basin boundaries
  
  if (!is.null(lb_basins)) {
    
    if (lb_clip) {
      for (kb in 1:length(lb_basins)) {
        
        n_basin <- sf::st_crop(lb_basins[[kb]], xmin = min(maplong), xmax = max(maplong), ymin = min(maplat), ymax = max(maplat))
        n_basin <- sf::st_transform(n_basin, sf::st_crs(map_proj))
        
        plot(n_basin,  border = lb_border, lwd = lb_lwd, col = NA, add = TRUE)
      }
    }
    
    if (!lb_clip) {
      for (kb in 1:length(lb_basins)) {
        
        n_basin <- sf::st_transform(lb_basins[[kb]], sf::st_crs(map_proj))
        plot(n_basin,  border = lb_border, lwd = lb_lwd, col = NA, add = TRUE)
      }
    }
  }
  
  ################################################# end basin boundaries
  
  ################################################# add small basin boundaries
  
  if (!is.null(sb_basins)) {
    
    if (sb_clip) {
      for (kb in 1:length(sb_basins)) {
        
        n_basin <- sf::st_crop(sb_basins[[kb]], xmin = min(maplong), xmax = max(maplong), ymin = min(maplat), ymax = max(maplat))
        n_basin <- sf::st_transform(n_basin, sf::st_crs(map_proj))
        
        plot(n_basin,  border = sb_border, lwd = sb_lwd, col = NA, add = TRUE)
      }
    }
    
    if (!sb_clip) {
      for (kb in 1:length(sb_basins)) {
        
        n_basin <- sf::st_transform(sb_basins[[kb]], sf::st_crs(map_proj))
        plot(n_basin,  border = sb_border, lwd = sb_lwd, col = NA, add = TRUE)
      }
    }
  }
  
  ################################################# end basin boundaries
  
  ################################################# add lines and features
  if (rivers) { 
    rline <- sf::st_crop(rivers10, xmin = maplong[1],xmax = maplong[2],
                         ymin = maplat[1],ymax = maplat[2])
    rivers_1 <- sf::st_transform(rline, sf::st_crs(map_proj))
    
    plot(rivers_1,  col = "blue", add = TRUE)
  }
  
  if (boundaries) {
    pline <- sf::st_crop(plines10, 
                         xmin = maplong[1], 
                         xmax = maplong[2],
                         ymin = maplat[1], 
                         ymax = maplat[2])
    plines_1 <- sf::st_transform(pline, sf::st_crs(map_proj))
    
    plot(plines_1, lwd = 2, col = NA, add = TRUE)
  }
  
  #####################################################################  add provincial labels
  
  if (plabels) {
    
    # set latitude and longitude for provincial and territorial labels
    llong <- c(-124, -124, -115, -105.8, -136.3, -120.1, -120.1, -86.7, -73.4, -97.3, -95,
               -61.3, -61.3, -66.15, -66.15, -63.3, -63.7)
    llat <- c(55, 55, 56.5, 55, 63.3, 64, 64, 51.5, 51.5, 56, 65.8, 54, 54, 46.9, 46.9, 46.3, 44.7)
    lpos <- c(3, 1, 1, 1, 1, 3, 1, 1, 1, 1, 1, 3, 1, 3, 1, 1, 1)
    Labels <- c("British", "Columbia", "Alberta", "Saskatchewan", "Yukon", "Northwest", "Territories",
                "Ontario", "Quebec", "Manitoba", "Nunavut", "Newfoundland", "and Labrador",	
                "New", "Brunswick", "P.E.I.", "Nova Scotia")
    
    
    ### trim list of provincial labels to only those inside the map box
    map_label <- data.frame(llong,llat,lpos,Labels)
    map_label1 <- map_label[map_label$llong <= maplong[1],] 
    map_label2 <- map_label1[map_label1$llong >= maplong[2],] 
    map_label3 <- map_label2[map_label2$llat >= maplat[1],] 
    map_label4 <- map_label3[map_label3$llat <= maplat[2],]
    
    
    map_labels <- map_label4 %>%
      dplyr::mutate(Label = as.factor(Labels)) %>%
      dplyr::mutate(lpos = as.factor(lpos)) %>%
      sf::st_as_sf(coords = c("llong","llat")) %>%
      sf::st_set_crs(4326)
    
    
    pt_labelsa <- sf::st_transform(map_labels, map_proj)
    
    
    for (kk in 1:length(pt_labelsa$lpos)) {
      xy <- unlist(pt_labelsa$geometry[kk])
      lab <- as.character(pt_labelsa$Label[kk])
      text(xy[1],xy[2],lab, pos = lpos[kk],  col = "black", cex = pl_cex)
    }
    
    
  }
  
  #####################################################################  end of add provincial labels
  
  
  
  ################################################################### plotting locations
  if (!is.null(locations)) {                      
    
    
    if (ncol(locations) == 3) {
      
      mlong <- locations[,1]
      mlat  <- locations[,2]
      mcode <- locations[,3]
      
    }
    
    
    if (ncol(locations) == 2) {     ### only locations provided
      
      # create a simple features object with crs = epsg 4326 (longlat with WGS84 geoid)
      mlong <- locations[,1]
      mlat  <- locations[,2]
      mcode <- rep(1,length(mlong))
      
    }
    # create a simple features object with crs = epsg 4326 (longlat with WGS84 geoid)
    pt_sf <- data.frame(mlong, mlat, mcode) %>%
      dplyr::mutate(mcode = as.factor(mcode)) %>%
      sf::st_as_sf(coords = c("mlong", "mlat")) %>%
      sf::st_set_crs(4326)
    
    
    # reproject 
    pt_sfa <- sf::st_transform(pt_sf, map_proj)
    
    mcode <- unlist(mcode)
    
    ################################################# plot points 
    plot(pt_sfa, add = TRUE,
         pch = lo_pch[mcode],
         col = lo_col[mcode],
         bg = lo_bg[mcode])
    
    if (legend) {
      
      legend("topright", lo_text, pch = lo_pch,  inset = c(0.06,0.115), col = lo_col, 
             pt.bg = lo_col, 
             cex = lo_cex, bg = "white", title = lo_title)
    }
  }
  
  
  ##################################################################### end of locations
  
  
  #####################################################################  plotting trends
  if (!is.null(trends)) {                      
    
    if (!length(trends[1,]) == 4) {
      print("trends data frame does not have four columns:  long, lat, slope, pvalue")
      return()
    } 
    
    mlong <- as.numeric(unlist(trends[1]))
    mlat  <- as.numeric(unlist(trends[2]))
    trend_a <- trends[3]
    pvalue <- trends[4]
    
    trend <- ch_tr_sign(trend_a)
    signif <- ch_tr_signif(pvalue, pvalue = tr_p)
    
    
    # create a simple features object with crs = epsg 4326 (longlat with WGS84 geoid)
    pt_sf <- data.frame(mlong, mlat, trend, signif) %>%
      dplyr::mutate(trend = as.factor(trend)) %>%
      dplyr::mutate(signif = as.factor(signif)) %>%
      sf::st_as_sf(coords = c("mlong", "mlat")) %>%
      sf::st_set_crs(4326)
    
    # reproject 
    pt_sfa <- sf::st_transform(pt_sf, map_proj)
    # pt_sfa <- st_crop(pt_sfa, xmin=maplong[1],xmax=maplong[2],ymin=maplat[1],ymax=maplat[2])
    
    tcode <- unlist(trend)
    pcode <- unlist(signif)
    
    ################################################# plot points 
    plot(pt_sfa, add = TRUE,
         pch = tr_pch[trend],
         col = tr_col[trend],
         bg = tr_col[trend],
         cex = tr_cex[signif])
    
    
    if (legend) {
      tr_lpch <- c(tr_pch[3], tr_pch[3], tr_pch[2], tr_pch[1], tr_pch[1])
      tr_lcol <- c(tr_col[3], tr_col[3], tr_col[2], tr_col[1], tr_col[1])
      legend("topright", tr_ltext, pch = tr_lpch, cex = 0.60, inset = c(0.06,0.115), col = tr_lcol, pt.bg = tr_lcol, 
             pt.cex = tr_lsz, bg = "white", title = "Trends")
    }
  }
  #####################################################################  end of trends
  
  ##################################################################### plot variable scaled colour
  if (!is.null(var)) {
    
    
    if (!length(var[1,]) == 3) {
      print("varaible data frame does not have three columns:  long, lat, variable")
      return()
    } 
    
    mlong <- as.numeric(unlist(var[1]))
    mlat  <- as.numeric(unlist(var[2]))
    var_a <- as.numeric(unlist(var[3]))
    
    
    # create a simple features object with crs = epsg 4326 (longlat with WGS84 geoid)
    pt_sf <- data.frame(mlong, mlat, var_a ) %>%
      dplyr::mutate(var_a = as.factor(var_a)) %>%
      sf::st_as_sf(coords = c("mlong", "mlat")) %>%
      sf::st_set_crs(4326)
    
    # reproject 
    pt_sfa <- sf::st_transform(pt_sf, map_proj)
    
    
    plot(pt_sfa, add = TRUE,
         pch = 19,
         col = "black",
         cex = 0.30)
    
    plot(pt_sfa, add = TRUE,
         pch = vr_pch,
         col = "black",
         bg = ch_color_gradient(var_a, vr_colors),
         cex = 0.5 * vr_cex)
    
    
    if (legend) {
      
      lx <- seq(vr_range[1], vr_range[2], by = (vr_range[2] - vr_range[1]) / 10)
      legend("topright", legend = lx, pch = 22,  inset = c(0.06,0.115), pt.bg = ch_color_gradient(lx, vr_colors), cex = 0.8,  
             bg = "white",  title = vr_text)
    }
    
  }
  
  ###################################################################  end of variable scaled colour
  
  
  ##################################################################### plot scaled symbols
  
  
  if (!is.null(sc_var)) {
    
    if (!length(sc_var[1,]) == 3) {
      print("variable data frame does not have three columns:  long, lat, variable")
      return()
    } 
    
    mlong <- as.numeric(unlist(sc_var[1]))
    mlat  <- as.numeric(unlist(sc_var[2]))
    var_a <- as.numeric(unlist(sc_var[3]))
    var_b <- var_a
    
    
    # create a simple features object with crs = epsg 4326 (longlat with WGS84 geoid)
    pt_sf <- data.frame(mlong, mlat, var_a ) %>%
      dplyr::mutate(var_a = as.factor(var_a)) %>%
      sf::st_as_sf(coords = c("mlong", "mlat")) %>%
      sf::st_set_crs(4326)
    
    # reproject 
    pt_sfa <- sf::st_transform(pt_sf, map_proj)
    
    
    
    plot(pt_sfa, add = TRUE,
         pch = sc_pch,
         col = "black",
         cex = 0.30)
    
    
    plot(pt_sfa, add = TRUE,
         pch = sc_pch,
         col = sc_color,
         bg = sc_color,
         cex = var_b*4)
    
    
    if (legend) {
      
      lx <- seq(sc_range[1], sc_range[2], by = (sc_range[2] - sc_range[1]) / 10)
      legend("topright", legend = lx, pch = sc_pch,  inset = c(0.06,0.115), pt.bg = sc_color, 
             col = sc_color, pt.cex = lx * 4, title = sc_text,  
             bg = "white") 
    }
    
  }
  
  ###################################################################  end of variable scaled colour
  
  
  #####################################################################  add special labels
  
  if (!is.null(x_labels)) {
    
    llong <- unlist(x_labels[1])
    llat <- unlist(x_labels[2])
    lpos <-  unlist(x_labels[3])
    lcex <- unlist(x_labels[4])
    lfont <- unlist(x_labels[5])
    lcol <- unlist(x_labels[6])
    llabels <- unlist(x_labels[7])
    
    xmap_labels <- data.frame(llong, llat, lpos, lcol, lfont, lcex, llabels) %>%               
      dplyr::mutate(llabels = as.factor(llabels)) %>%
      dplyr::mutate(lpos = as.factor(lpos)) %>%
      dplyr::mutate(lcol = as.factor(lcol)) %>%
      dplyr::mutate(lcex = as.factor(lcex)) %>%
      dplyr::mutate(lfont = as.factor(lfont)) %>%
      sf::st_as_sf(coords = c("llong","llat")) %>%
      sf::st_set_crs(4326)
    
    x_labelsa <- sf::st_transform(xmap_labels, map_proj)
    
    message(length(llong))
    
    for (kk in 1:length(llong)) {
      xy <- unlist(x_labelsa$geometry[kk])
      text(xy[1],xy[2],llabels[kk], pos = lpos[kk],  font = lfont[kk], col = lcol[kk], cex = lcex[kk])
    }
    
    
  }
  
  #####################################################################  end of add x_labels
  return()
  
}

######################################################################## end of function



