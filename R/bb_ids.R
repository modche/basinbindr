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
        
        bb_shape <- shape %>% filter(x[i] > xmin, x[i] < xmax, y[i] > ymin, y[i] < ymax) 
        extract_id <- st_join(station_sf[i,], bb_shape, join = st_within) %>% 
            pull({polygon_col})
        
        if(length(extract_id) != 1) message('Station has more than 1 matching polygon. Using first.')
        ids[i] <- extract_id[1]
    }
    if(debug) cat("\n")
    return(ids)    
}