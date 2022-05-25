
#' Function to extract ID (polygon_id) for one or multiple gauging stations.
#'
#' @param stations data.frame with gauging station with x, y
#' @param shape path to main shapefile
#' @param debug boolean; prints working process
#' @param polygon_col name of id column in shapefile
#'
#' @return ID of basin that includes the gauging station.
#' @details Stations must be a data.frame of one or more gauging stations with x and y (longitude, latitude in WGS84) columns.
#' @export
#'
#' @examples
#' # will follow
bb_ids <- function(stations, 
                   shape, 
                   debug = FALSE, 
                   polygon_col = 'polygon_id') {
    
    ids <- rep(NA, nrow(stations))
    station_sf <- stations %>% sf::st_as_sf(coords = c("x","y"), crs = 4326, remove = FALSE)
    
    x <- stations$x
    y <- stations$y
    
    for (i in 1:nrow(stations)) {
        if(debug) cat(i," ")
        
        bb_shape <- shape %>% dplyr::filter(x[i] > xmin, x[i] < xmax, y[i] > ymin, y[i] < ymax) 
        extract_id <- st_join(station_sf[i,], bb_shape, join = sf::st_within) %>% 
            dplyr::pull({polygon_col})
        
        if(length(extract_id) != 1) message('Station has more than 1 matching polygon. Using first.')
        ids[i] <- extract_id[1]
    }
    if(debug) cat("\n")
    return(ids)    
}