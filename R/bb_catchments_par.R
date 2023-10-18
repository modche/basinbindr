#' Creates a catchment polygon from list of basic basins running in parallel and calculates area and saves a geopackage
#'
#' @param station_basins a list, each element has a vector with primary basin id from germanyshape
#' @param ncores integer, number of cores to be used during parallel processing
#' @param germanyshape basin shapefile as a \code{sf} object; can be generated with [bb_readgermanyshape()].
#' @param polygon_col name of basin id column in shapefile
#' @param stations \code{sf} point object of stations, columns will be retained as polygon attributes, order of stations must be identical with the 'station_basins'
#' @param station_id_col name of basin id column in shapefile
#' @param dissolve boolean, if TRUE subbasins are dissolved into one catchment polygon
#' @param save boolean, if TRUE catchments of stations will be saved as a geopackage under 'dsn'
#' @param dsn path and file name of geopackage (if 'save' TRUE)
#' @param layer layer name within geopackage to be saved, e.g. Bundesland (if 'save' TRUE)
#'
#' @return \code{sf} polygon object with column area_bb for catchment area in km2, columns from 'stations' will be retained
#' @export
#'
#' @examples
#' \dontrun{}
#' will follow
bb_catchments_par <- function(station_basins, ncores, germanyshape, polygon_col = 'objectid',
                          stations=NULL, station_id_col = "stationid", dissolve=TRUE,
                          save=FALSE, dsn="",layer=NULL){
    n <- length(station_basins) # wrapper
    catchments <- NULL
    
    # create a cluster of cores
    cl <- makeCluster(ncores)
    
    # register the cluster for parallel computation
    doParallel::registerDoParallel(cl)
    catchments <- foreach (i = 1:n, .combine="rbind", .errorhandling = "remove", .packages='sf') %do% { #
        # tryCatch( {
        #     c(...)
        # }, error = function(e) {
        #     cat("Error occurred: ", conditionMessage(e), "\n")
        #     #NULL
        # })
        
        # station_id
        station <- stations[i,]
        station_id <- station[[station_id_col]]
        #print(paste(i," : ",station_id))
        
        # extract ids of all sub-basins according to polygon with station in it
        selection <- station_basins[[i]]
        if (is.null(selection)){ # empty ids - e.g. no match of station with bbasins
            return(NULL)
        }
        
        #browser()
        
        #extract all sub-basins from germanyshape
        basins <- germanyshape[germanyshape[[polygon_col]] %in% selection,]
        
        # dissolve subcatchments into one polygon
        if (dissolve==TRUE){
            basin_diss <- sf::st_union(basins) # dissolve
            basin <- sf::st_make_valid(basin_diss) # validate
            basin <- nngeo::st_remove_holes(basin)
        } else {
            basin <- basins
        }
        
        # # Validate/Plot
        # ggplot() + geom_sf(data=basin, fill="tan") +
        #     geom_sf(data=station, shape=2, size=5, color="purple")
        # 
        #calculate total area of catchment in km2
        station$area_bb <- sum(as.numeric(sf::st_area(basin)))/1e6
        print(paste(station_id," area: ", station$area_bb))

        # Add attributes to polygon to save
        catchment <- st_drop_geometry(station)#[,station_id_col]
        if (dissolve== TRUE){   # TODO: eleganter lösen
            st_geometry(catchment) <- basin # works only for single geometry
        } else {
            catchment <- cbind(basin, catchment) # ---> works only for undissolved
        }

        # Return catchment of this iteration
        catchment
    }
    stopCluster(cl)
    
    if (save==TRUE){ # TODO: could be put into a separate function and called in a wrapper
        sf::st_write(catchments, dsn=dsn, layer=layer, delete_layer = TRUE)
    }
    
    return(catchments)
}