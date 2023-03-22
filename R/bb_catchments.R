#' Creates a catchment polygon from list of basic basins and calculates area and saves a geopackage
#'
#' @param station_basins a list, each element has a vector with primary basin id from germanyshape
#' @param germanyshape basin shapefile as a \code{sf} object; can be generated with [bb_readgermanyshape()].
#' @param polygon_col name of basin id column in shapefile
#' @param save boolean, if TRUE catchments of stations will be saved as a geopackage under \code{dsn}
#' @param dsn of basin id column in shapefile
#' @param layer layer name in geopackage to be saved, e.g. Bundesland (if save TRUE)
#' @param stations \code{sf} point object of stations, columns will be retained 
#' @param station_id_col name of basin id column in shapefile
#'
#' @return \code{sf} polygon object with catchment area in km2 in column area_bb, columns from \code{stations} will be retained
#' @export
#'
#' @examples
#' \dontrun{}
#' will follow
bb_catchments <- function(station_basins, germanyshape, polygon_col = 'objectid', 
                          save=FALSE,dsn="",layer="none",
                          stations=NULL, station_id_col = "stationid"){
    n <- length(station_basins) # wrapper
    catchments <- NULL
    for (i in 1:n) {
        # station_id
        station <- stations[i,]
        station_id <- station[[station_id_col]]
        print(paste(i," : ",station_id))
        
        #extract ids of all sub-basins according to polygon with station in it
        selection <- station_basins[[i]]
        
        #extract all sub-basins from germanyshape
        basins <- germanyshape[germanyshape[[polygon_col]] %in% selection,]
        
        # dissolve subcatchments into one polygon
        basin_diss <- sf::st_union(basins)
        basin_diss_valid <- st_make_valid(basin_diss)
        ggplot() + geom_sf(data=basin_diss_valid, fill="tan") +
            geom_sf(data=station, shape=2, size=5, color="purple")
        
        #calculate total area of catchment in km2
        station$area_bb <- sum(as.numeric(sf::st_area(basin_diss_valid)))/1e6
        print(paste(station_id," area: ", station$area_bb))
        
        # Polygon to save
        catchment <- st_drop_geometry(station)#[,station_id_col]
        st_geometry(catchment) <- basin_diss_valid
        
        # Append catchment to others
        catchments <- rbind(catchments, catchment)
    }
    
    if (save){ # TODO: could be put into a separate function and called in a wrapper
        sf::st_write(catchments, dsn=dsn, layer=layer, delete_layer = TRUE)
    }
    
    return(catchments)
}