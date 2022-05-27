#' Calculates catchment area
#'
#' @param station_basins a list, each element has a vector with primary basin id from germanyshape
#' @param germanyshape basin shapefile as a \code{sf} object; can be generated with [bb_readgermanyshape()].
#' @param polygon_col name of id column in shapefile
#'
#' @return A vector with catchment area in km2 for each list element (i.e. each catchment)
#' @export
#'
#' @examples
#' \dontrun{
#' will follow
#' }
bb_area <- function(station_basins,
                    germanyshape,
                    polygon_col = 'objectid') {
n <- length(station_basins) #wrapper
area <- rep(NA, n)
    for (i in 1:n) {
    #extract ids of all sub-basins according to polygon with station in it
        selection <- station_basins[[i]]
    #extract all sub-basins from germanyshape
        basins    <- germanyshape[germanyshape[[polygon_col]] %in% selection,]
    #calculate total area of catchment in km2
        area[i]   <- sum(as.numeric(sf::st_area(basins)))/1e6
    }
return(area)
}
