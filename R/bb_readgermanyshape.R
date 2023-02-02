#' @title Reads germanyshape with \code{sf}
#' 
#' @description Read basin shapes with all primary basins with \code{sf} package and stores as global variable defined by \code{name}.
#' @param file Location of the shapefile or geopackage file 
#' @param name Variable name that the shapefile should be assigned to.
#' @param quiet logical; suppress info from \code{sf} package on name, driver, size and spatial reference etc.
#'
#' @return Loads the basins shape and stores it in a variable given by \code{name}. 
#'     The \code{sf} package will generate some printouts or warnings during processing.
#' @export
#'
#' @details The original file is the primaray basin file (Basiseinzugsgebiete) from BfG from the year 2021. Projection is WGS84 (EPSG: 4326).
#'
#' @examples
#' \dontrun{
#' gpkg_file <- '~/basisbasins.gpkg'
#' bb_readgermanyshape(file = gpkg_file, name = 'basins_germany')
#' 
#' #wrapper for this:
#' basins_germany <- sf::st_read('~/basisbasins.gpkg')
#' }
 
bb_readgermanyshape <- function(file = NULL, name = 'shape', quiet = FALSE) {
    assign({name}, sf::st_read(file, quiet = quiet), envir = .GlobalEnv)
    }
