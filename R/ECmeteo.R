
# Tools to Locate Stations  -----------------------------------------------------------

#' @title Find Meteorological Stations from Environment and Climate Change Canada
#' @description Function for locating the nearest 'n' stations to a specified coordinate. Returns detailed list of Canadian stations that have available meteorological data in hourly or daily format.
#' @usage
#' ECm.find(lat, lon, n, timeframe)
#' @param lat   Latitude in decimal degrees.
#' @param lon  Longitude in decimal degrees. Note: Canadian longitudes are negative.
#' @param n     The number of stations to be located.
#' @param timeframe 1: Hourly, 2: Daily
#' @param verbose TRUE/FALSE (by default=FALSE) after finding the stations explicit show the data.frame results
#' @return Two dataframes:
#' \itemize{
#' \item Station.Inventory - data.frame with the complete database from ECCC up to actual date
#' \item Station.Selection - data.frame with the closest 'n' stations based on the required timeframe.
#' }
#' @examples
#' ## EX. 1: Find the closest 20 stations to 49.6, -114.5 that have hourly data.
#' ECm.find(lat=49.6, lon=-114.5, n=20, timeframe=1)
#'
#' ## EX. 2: Find the closest 10 stations to Ottawa, Canada that have daily data.
#' ECm.find(lat=45.42, lon = -75.70, n=10, timeframe=2)
#' @author Victor Munoz
#' @export
ECm.find<-function(lat=49.6, lon=-114.5, n=20, timeframe=2,verbose=F){

        require(geosphere) #Distance from coordinates
        # require(maps)
        #
        # #Check that the coordinates are within Canada
        # country<-maps::map.where(database="world", lon, lat)
        #
        # if(!country%in%c("Canada", "NA")){
        #         warning('Specified coordinates are outside Canadian boundaries. Check coordinates.')
        # }


        #Downloading the information
        if(!exists("Station.Inventory")){

                cat("This process may take few minutes\n")

                Station.Inventory<-read.csv("ftp://client_climate@ftp.tor.ec.gc.ca/Pub/Get_More_Data_Plus_de_donnees/Station Inventory EN.csv",
                                            stringsAsFactors = F,skip = 3)

                names(Station.Inventory)<-c("Station.Name","Province","Climate.Identifier","WebID","WMO.Identifier","TC.ID","Latitude","Longitude",
                                   "Northing","Easting","Elevation","First.Year","Last.Year","HLY.First.Year","HLY.Last.Year",
                                   "DLY.First.Year","DLY.Last.Year","MLY.First.Year","MLY.Last.Year")

                Station.Inventory<<-Station.Inventory


        }

        ##Arrange and include distance to Station.Selection

        #Obtain distance from specified coordinates to stations. Distance is converted to km and rounded.
        station.distance<-geosphere::distVincentySphere(c("Longitude"=lon,"Latitude"=lat),
                                             cbind(Station.Inventory$Longitude,
                                                    Station.Inventory$Latitude))/1000
        #Include distance in the network file
        Station.Selection<-cbind(Station.Inventory,Distance=round(station.distance,1))

        #Sorted unregulated stations
        Station.Selection<-Station.Selection[order(Station.Selection$Distance),]


        ##Select based on timeframe
        if(timeframe==2){
                Station.Selection<-Station.Selection[!is.na(Station.Selection$DLY.First.Year),]
                }

        if(timeframe==1){
                Station.Selection<-Station.Selection[!is.na(Station.Selection$HLY.First.Year),]
                }

        #Include attributes

        Station.Selection$Lat.for.Distance<-lat
        Station.Selection$Lon.for.Distance<-lon
        Station.Selection$timeframe<-timeframe

        #Export the info
        Station.Selection<-head(Station.Selection,n)

        Station.Selection<<-Station.Selection

        #Display the selected network
        if(verbose==T){View(Station.Selection)}

        cat("Last Station at :",round(Station.Selection$Distance[n])," km\n")
}


# Meteorological Tools ----------------------------------------------------


#' @title Download Station Information from ECCC (One Station)
#' @description Function to capture the available daily or hourly meteorological information from one meteorological station. Used in \code{\link[Hydro]{ECm.capture.all}}.
#' @param WebID   WebID used by ECCC, it is the page number used internally by ECCC.
#' @param timeframe 1: Hourly, 2: Daily
#' @param flag (Default=FALSE) Are flags associated to the info needed?
#' @param Climate.Identifier (Default=FALSE ) Parameter WebID or Climate.Identifier?, If info provided by WebID is really Climate.Identifier use TRUE. If that is the case WebID is a string.
#' @return A data.frame consisting of all available meteorological data for one station.
#' @author Victor Munoz
#' @export
ECm.capture<-function(WebID=10200, timeframe=2,flag=F,Climate.Identifier=F){

        require(RCurl)

        options(warn=-1)

        #In case that ECm.capture was done without ECm.find

        if(!exists("Station.Inventory")){

                cat("This process may take some few minutes\n")

                Station.Inventory<-read.csv("ftp://client_climate@ftp.tor.ec.gc.ca/Pub/Get_More_Data_Plus_de_donnees/Station Inventory EN.csv",
                                            stringsAsFactors = F,skip = 3)

                names(Station.Inventory)<-c("Station.Name","Province","Climate.Identifier","WebID","WMO.Identifier","TC.ID","Latitude","Longitude",
                                            "Northing","Easting","Elevation","First.Year","Last.Year","HLY.First.Year","HLY.Last.Year",
                                            "DLY.First.Year","DLY.Last.Year","MLY.First.Year","MLY.Last.Year")

                Station.Inventory<<-Station.Inventory

                Station.Selection<-Station.Inventory
        }

        #Find parameters for selected station
        if(Climate.Identifier==F){
                stationid.db<-Station.Inventory[Station.Inventory$WebID%in%WebID,]


        }else{
                stationid.db<-Station.Inventory[Station.Inventory$Climate.Identifier%in%WebID,]

        }

        options(warn=0)

        if(nrow(stationid.db)==0){
                return(warning("Invalid WebID or Climate.Identifier, check in Station.Inventory file"))
        }

        if(nrow(stationid.db)>1){
                return(warning("Select just one WebID or Climate Identifier"))
        }


        if(is.na(stationid.db$DLY.First.Year)==T&timeframe==2){
                return(warning("Requested hourly information (timeframe=2), but station does not have daily info"))
        }


        if(is.na(stationid.db$HLY.First.Year)==T&timeframe==1){
                return(warning("Requested hourly information (timeframe=1), but station does not have hourly info"))
        }

        options(warn=-1)

        print(paste0("ID: ",stationid.db$Climate.Identifier,
                     "| Station: ",stationid.db$Station.Name))

        opts <- list(
                fresh.connect = TRUE,
                forbid.reuse = TRUE,
                maxconnects=3
        )

        options(RCurlOptions = opts)

        #Define a variable valid year
        if(timeframe==2){
                #Years from Daily DB

                years.avail<-as.list(stationid.db$"DLY.First.Year": stationid.db$"DLY.Last.Year")
                #Define URL
                urls<-paste0("http://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv",
                             "&stationID=",stationid.db$WebID,"&Year=",years.avail,"&timeframe=",timeframe,
                             "&submit=Download+Data")
                }

        if(timeframe==1){
                #Years from Daily DB
                years.avail<-as.list(rep(c(stationid.db$"HLY.First.Year": stationid.db$"HLY.Last.Year"),
                                         each=12))
                #Define months and URL
                urls<-paste0("http://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv",
                             "&stationID=",stationid.db$WebID,"&Year=",years.avail,"&Month=",1:12,
                             "&timeframe=",timeframe,"&submit=Download+Data")

                }

        #URL for all the years
        EC.info<-RCurl::getURIAsynchronous(urls,
                                           .opts = list(verbose = TRUE),
                                    binary = rep(FALSE, length(urls)))

        closeAllConnections()

        # EC.info<<-EC.info
        #Complete Table
        if(timeframe==1){

                #different random files for multitasking
                Sys.sleep(runif(1))
                set.seed(runif(1))

                #Save a temporal file
                tfile<-tempfile()

                save(EC.info,file=tfile)

                #Read lines
                csv.file<-readLines(tfile)

                # Try different ways to parse
                EC.table.lst<-lapply(3:0,FUN=function(x){

                        skip.val<-grep("Date/Time",csv.file)[1]-x

                        #Defile the line to skipe
                        EC.table<-read.csv(text=EC.info,
                                           skip=skip.val,stringsAsFactors = F)

                })

                #Find number of cols
                EC.table.col<-sapply(EC.table.lst,FUN=function(x)ncol(x)*nrow(x))

                #Take the maximum which is the optimal parse
                EC.table<-EC.table.lst[[match(max(EC.table.col),EC.table.col)]]

                #Adjust time format
                EC.table$Date.Time<-as.POSIXct(EC.table$Date.Time, format = "%Y-%m-%d %H:%M" )

                EC.table<-EC.table[complete.cases(EC.table$Date.Time),]

        }

        if(timeframe==2){

                EC.table<-read.csv(text = EC.info,skip = 24,stringsAsFactors = F)

                #Adjust time format
                EC.table$Date.Time<-as.Date(EC.table$Date.Time,format = "%Y-%m-%d")

                EC.table<-EC.table[complete.cases(EC.table$Date.Time),]


        }

        #change the names of columns

        #Special characters in space
        EC.table.name<-trimws(gsub("[^0-9A-Za-z']", " ",
                                   names(EC.table), perl=TRUE))

        #spaces in .
        EC.table.name<-gsub("\\s+",".",EC.table.name)

        #Column definition
        names(EC.table)<-EC.table.name

        #Remove Column not needed

        not.needed.col<-match(c("Time","Day","Year","Month"),
                              names(EC.table))

        Flag.col<-c(grep("Flag",names(EC.table)),
                    match(c("Data.Quality","Weather"),names(EC.table))
        )

        numeric.col<-!(1:ncol(EC.table)%in%c(not.needed.col,Flag.col,1))

        EC.table[,numeric.col]<-apply(EC.table[,numeric.col],2,as.numeric)

        #Without Flags, flag is included as not.needed
        if(flag==F){not.needed.col<-c(not.needed.col,Flag.col)}

        not.needed.col<-not.needed.col[complete.cases(not.needed.col)]

        EC.table<-EC.table[,-not.needed.col]

        options(warn=0)

        return(EC.table)

}

#' @title Download Station Information
#' @description Function to capture the available monthly, daily or hourly meteorological information from the selected meteorological stations using \code{\link[Hydro]{ECm.find}}. Uses \code{\link[Hydro]{ECm.capture}} with the option to use multiple processors.
#' @usage ECm.capture.all(multicore, timeframe)
#' @param multicore True or false; State whether the function may utilize multicore capabilities for faster processing.
#' @param timeframe 1: Hourly, 2: Daily, 3: Monthly
#' @return a dataframe consisting of all available meteorological data for each station.
#' @examples
#' ## EX 1: Return all hourly data for previous selected 'n' stations.
#' Assume multicore capabilities.
#' ECm.capture.all(multicore = T, timeframe = 1)
#' @author Victor Munoz
#' @export
ECm.capture.all<-function(flag=F){


        Latitude<-Station.Selection$Lat.for.Distance[1]
        Longitude<-Station.Selection$Lon.for.Distance[1]
        timeframe<-Station.Selection$timeframe[1]


        ECm.stations<-lapply(Station.Selection$WebID,FUN=function(x){

                ECm.capture(WebID = x,timeframe = timeframe,flag = flag,Climate.Identifier = F)
        })

        names(ECm.stations)<-Station.Selection$Climate.Identifier

        attr(x = ECm.stations,"Latitude")<-Latitude
        attr(x = ECm.stations,"Longitude")<-Longitude
        attr(x = ECm.stations,"timeframe")<-timeframe
        attr(x = ECm.stations,"n")<-n


        if(timeframe==2){
                ECCC.stations.dly<-ECm.stations
                ECCC.stations.dly<<-ECCC.stations.dly
                filename<-paste0("EC Daily Canada Data - ",length(ECm.stations)," Stations.RData")
                save(ECCC.stations.dly, file=filename,ascii=FALSE, compress=TRUE)


        }

        if(timeframe==1){
                ECCC.stations.hrly<-ECm.stations
                ECCC.stations.hrly<<-ECCC.stations.hrly
                filename<-paste0("EC Hourly Canada Data - ",length(ECm.stations)," Stations.RData")
                save(ECCC.stations.hrly, file=filename,ascii=FALSE, compress=TRUE)


        }

}


