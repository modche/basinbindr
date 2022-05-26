#' Function to extract basin ID (polygon_id) for one or multiple gauging stations.
#'
#' @param stations data.frame with gauging station with x and y coordinates.
#' @param germanyshape basin shapefile as a \code{sf} object; can be generated with [bb_readgermanyshape()].
#' @param debug boolean; TRUE prints working process
#' @param polygon_col name of id column in shapefile
#'
#' @return Basin ID that includes the gauging station. For each line in \code{stations} one ID will be returned. 
#' @details Stations must be a data.frame of one or more gauging stations with x and y (longitude, latitude in WGS84) columns.
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
        find_id <- sf::st_join(station_sf[i,], bb_shape, join = sf::st_within) 
        extract_id <-  dplyr::pull(find_id, {polygon_col})
        
        if(length(extract_id) != 1) message('Station has more than 1 matching polygon. Using first.')
        ids[i] <- extract_id[1]
    }
    if(debug) cat("\n")
    return(ids)    
}