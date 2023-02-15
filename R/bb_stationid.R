#' @title Extract primary basin ID for one or multiple gauging stations
#'
#' @description Extract primary basin ID for one or multiple gauging stations. 
#'     With the primary basin ID the upstream sub-basins can be identified. 
#' @param stations \code{sf} point object. Alternatively, a \code{data.frame} with lat and lon coordinates of gauging station(s). See details.
#' @param germanyshape basin shapefile as a \code{sf} object; can be generated with [bb_readgermanyshape()].
#' @param debug boolean; TRUE prints working process
#' @param polygon_col name of primary basin ID column in shapefile, e.g. \code{objectid}
#'
#' @return Primary basin ID that includes the gauging station. For each line in \code{stations} one primary basin ID will be returned. 
#' @details Stations must be a \code{sf} point object. Alternatively, \code{data.frame} with one or more gauging stations (\code{rows}) with \code{x} (longitude, WGS84) and \code{y} (latitude, WGS84) in two \code{columns}.
#' @export
#'
#' @examples
#' # will follow
bb_stationid <- function(stations, 
                   germanyshape, 
                   debug = FALSE, 
                   polygon_col = 'polygon_id') {
    
    # Checks
    if(!polygon_col %in% names(germanyshape)) stop(paste0(polygon_col, ' not found as data column in germanyshape.'))
    
    # Check sf object or create it from lat/lon coordinates
    if (!all(sf::st_is(stations, "POINT"))){ # check if sf point object
        # Check column availability to create an sf object
        cols <- c("x","y")
        if(!all(cols %in% names(stations))) stop(paste0("stations was not an sf object; and ",cols[1]," and ", cols[2], ' not found as data column in stations.'))
        
        print(paste("Warning: Simple feature object created from",cols[1]," and ", cols[2],"with crs=4326"))
        stations <- sf::st_as_sf(stations, coords = c("lat","lon"), crs = 4326, remove = FALSE)
    }
    
    # Check coordinate system of stations and basins # TODOOOOO ####
    
    X <- sf::st_coordinates(stations)[,1]
    Y <- sf::st_coordinates(stations)[,2]
    
    # Identify basin-id for each station
    ids <- rep(NA, nrow(stations)) # Initialize variables
    for (i in 1:nrow(stations)) {
        if(debug) cat(i," ")
        
        # reduce number of possible intersects by binding box
        bb_shape <- dplyr::filter(germanyshape, X[i] > xmin, X[i] < xmax, Y[i] > ymin, Y[i] < ymax) 
        
        # Identify corresponding polygon
        # start_time2 <- Sys.time()
        find_id <- sf::st_join(stations[i,], bb_shape, join = sf::st_within) # within prüfen
        # end_time2 <- Sys.time()
        # print(end_time2 - start_time2)
        # start_time1 <- Sys.time()
        #find_id2 <- sf::st_intersection(stations[i,], bb_shape) # within prüfen
        # end_time1 <- Sys.time()
        # print(end_time1 - start_time1)
        
        # # Check results visually
        # find_ids <- sf::st_drop_geometry(find_id)[,polygon_col]
        # idx_ids <- which(sf::st_drop_geometry(bb_shape)[,polygon_col] %in% find_ids)
        # ggplot() + 
        #     geom_sf(data=bb_shape, color="grey") + 
        #     geom_sf(data=bb_shape[idx_ids[1],], fill="pink") + 
        #     #geom_sf(data=bb_shape[idx_ids[2],], fill="tan") + 
        #     geom_sf_text(data=bb_shape, aes_string(label=polygon_col), color="grey20", size=4) + 
        #     geom_sf_text(data=bb_shape[idx_ids,], aes_string(label=polygon_col), color="tan2", size=8) + 
        #     geom_sf(data=stations[i,], color="blue", size=5) +
        #     geom_sf_text(data=stations[i,], aes(label=camels_id), color="black", size=6) +
        
        # write basin-id to output
        if(nrow(find_id) != 1) message('Station has more than 1 matching polygon. Using first.') #prüfen wo fehlerhaft
        ids[i] <- sf::st_drop_geometry(find_id)[1,polygon_col] #extract_id[1]
    }
    if(debug) cat("\n")
    return(ids)    
}
