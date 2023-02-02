#' @title Extract primary basin ID for one or multiple gauging stations
#'
#' @description Extract primary basin ID for one or multiple gauging stations. 
#'     With the primary basin ID the upstream sub-basins are identified. 
#' @param stations \code{data.frame} with gauging station(s) with x and y coordinates. See details.
#' @param germanyshape basin shapefile as a \code{sf} object; can be generated with [bb_readgermanyshape()].
#' @param debug boolean; TRUE prints working process
#' @param polygon_col name of primary basin ID column in shapefile, e.g. \code{objectid}
#'
#' @return Primary basin ID that includes the gauging station. For each line in \code{stations} one primary basin ID will be returned. 
#' @details Stations must be a \code{data.frame} with one or more gauging stations (\code{rows}) with \code{x} (longitude, WGS84) and \code{y} (latitude, WGS84) in two \code{columns}.
#' @export
#'
#' @examples
#' # will follow
bb_stationid <- function(stations, 
                   germanyshape, 
                   debug = FALSE, 
                   polygon_col = 'polygon_id') {
    
    if(!polygon_col %in% names(germanyshape)) stop(paste0(polygon_col, ' not found as data column in germanyshape.'))
    
    ids <- rep(NA, nrow(stations))
    station_sf <- sf::st_as_sf(stations, coords = c("x","y"), crs = 4326, remove = FALSE)
    
    x <- stations$x
    y <- stations$y
    
    for (i in 1:nrow(stations)) {
        if(debug) cat(i," ")
        
        bb_shape <- dplyr::filter(germanyshape, x[i] > xmin, x[i] < xmax, y[i] > ymin, y[i] < ymax) 
        find_id <- sf::st_join(station_sf[i,], bb_shape, join = sf::st_within) # within prüfen
        extract_id <-  dplyr::pull(find_id, {polygon_col})
        
        if(length(extract_id) != 1) message('Station has more than 1 matching polygon. Using first.') #prüfen wo fehlerhaft
        ids[i] <- extract_id[1]
    }
    if(debug) cat("\n")
    return(ids)    
}