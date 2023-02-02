#' Delineation of catchment(s)
#'
#' @param station_ids vector with primary basin IDs for one or more gauging stations
#' @param germanyshape basin shapefile as a \code{sf} object; can be generated with [bb_readgermanyshape()].
#' @param polygon_col name of primary basin ID column in shapefile
#' @param geb name of domain column ('Gebietskennzahl'), default = 'geb_kz_num')
#' @param gew name of water body column ('Gewässerkennzahl', default = 'gew_kz_num')
#' @param remove_artifical logical, should artificial water bodies such as canals be removed from procedure?
#'
#' @return will follow
#' @export
#'
#' @examples
#' \dontrun{
#' will follow
#' }
bb_delineate <- function(station_ids, 
                         germanyshape,
                         polygon_col = 'objectid',
                         geb = 'geb_kz_num',
                         gew = 'gew_kz_num',
                         remove_artifical = TRUE) {

result <- list()    
    
for (i in 1:length(station_ids)) {
    
# set domain and water body number for station polygon
geb_station    <- germanyshape[[geb]][station_ids[i] == germanyshape[[polygon_col]]]
gew_station    <- germanyshape[[gew]][station_ids[i] == germanyshape[[polygon_col]]]
     
# filter upstream polygons starting from station polygon
basin_bind <- dplyr::filter(germanyshape, 
                            germanyshape[[geb]] <= geb_station,
                            germanyshape[[gew]] >= gew_station)

# remove polygons with large differences between domain and water body number
if (remove_artifical) {
    rm_artificials   <- (basin_bind[[gew]] - basin_bind[[geb]]) < 1e14  
    basin_bind_clean <- basin_bind[rm_artificials,] # remove over 70 canals and transitions
}

# count how many polgons form the catchment
#n <- nrow(basin_bind_clean)

result[[i]] <- basin_bind_clean[[polygon_col]] # bug, weil variable nur aus if drüber kommen kann

} #end loop

return(result)

} # end function.
