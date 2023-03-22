#' Delineation of catchment(s)
#'
#' @param bbasin_ids vector with primary basin IDs for one or more gauging stations
#' @param germanyshape basin shapefile as a \code{sf} object; can be generated with [bb_readgermanyshape()].
#' @param polygon_col name of primary basin ID column in shapefile germanyshape
#' @param geb name of domain column ('Gebietskennzahl'), default = 'geb_kz_num')
#' @param gew name of water body column ('Gewässerkennzahl', default = 'gew_kz_num')
#' @param remove_artifical logical, should artificial water bodies such as canals be removed from procedure?
#' @param coastal_area logical, should coastal waters be flagged? (not yet implemented)
#' @param check_border logical, if TRUE  it is checked if the delineated catchment area intersects the outer boundary of the basinshapefile, a flag is returned
#' @param plot logical, if TRUE plots are saved in one pdf for each bbasin_id, useful for quality checking the results
#' @param plot_dsn character, defines path to save plots (if plot TRUE) 
#' @param stations \code{sf} point object with station ids corresponding to bbasin_ids, (only needed if plot TRUE) 
#' @param station_id_col character with  column name of station ids in \code{stations}, (only needed if plot TRUE) 
#' @param plot_rivers river shapefile as a \code{sf} line object or character with rivers file name to plot (if plot TRUE), default NULL. If NULL no rivers to plot
#' 
#' @return will follow
#' @export
#'
#' @examples
#' \dontrun{
#' will follow
#' }
bb_delineate <- function(bbasin_ids, 
                         germanyshape,
                         polygon_col = 'objectid',
                         geb = 'geb_kz_num',
                         gew = 'gew_kz_num',
                         remove_artifical = TRUE,
                         #coastal_area = FALSE, # @TODO: potentially add a flag for coastal catchments (gkw > 9...)
                         check_border=FALSE,
                         plot=FALSE,
                         plot_dsn="",
                         stations=NULL,
                         station_id_col=NULL,
                         plot_rivers=NULL) {
    
    # Load rivers data if not NULL (TODO: create an extra function to read river data)
    if (plot==TRUE & !is.null(plot_rivers)){
        # Check and Load data
        if (is.character(plot_rivers)) {
            rivers_shp <- sf::st_read(plot_rivers, quiet = FALSE)
            print(paste("Rivers data loaded:", plot_rivers))
        } else {
            rivers_shp <- plot_rivers
        }
        if (!all(sf::st_is(rivers_shp, "LINE"))){
            print("Rivers data to plot loaded")
        } else {
            print("Warning: Rivers data is not a simple feature line object")
        }
        
        # Transform rivers data is necessary
        if (sf::st_crs(rivers_shp)!=sf::st_crs(germanyshape)){ # potentially as bb_transform()
            rivers_shp <- sf::st_transform(rivers_shp, sf::st_crs(germanyshape))
            print(paste("river coordinate system checked, transformed into", sf::st_crs(rivers_shp)[1]))
        } else {
            print(paste("river coordinate system checked, already in", sf::st_crs(rivers_shp)[1]))
        }
    }
    
    # Start identification of upstream bbasins (catchemnt area)
    result <- list()    
    for (i in 1:length(bbasin_ids)) {
        # Checks
        if (!bbasin_ids[i] %in% germanyshape[[polygon_col]]){
            print(paste("Basin-id", bbasin_ids[i], "not found, continue with next"))
            next
        }
        
        # set domain and water body number for station polygon
        bbasin_outlet <- germanyshape[bbasin_ids[i] == germanyshape[[polygon_col]],]
        geb_station    <- bbasin_outlet[[geb]]
        gew_station    <- bbasin_outlet[[gew]]
        
        # filter upstream polygons starting from station polygon
        basin_bind <- dplyr::filter(germanyshape, 
                                    germanyshape[[geb]] <= geb_station,
                                    germanyshape[[gew]] >= gew_station)
        
                # remove polygons with large differences between domain and water body number
        if (remove_artifical) {
            rm_artificials   <- (basin_bind[[gew]] - basin_bind[[geb]]) < 1e14  
            basin_bind_clean <- basin_bind[rm_artificials,] # remove over 70 canals and transitions
        } else {
            basin_bind_clean <- basin_bind # no artificial water bodies removed
        }
        
        # flag catchment if it touches the outer border
        if (check_border) {
            # @TODO: implement check 
            print("Border check still to be implemented")
        }
        
        # Check results visually and safe plots to pdf #### 
        if (plot==TRUE){
            # Select station if provided
            if (!is.null(stations)){
                station <- stations[i,]
                station_id <- station[[station_id_col]]
            } else {
                station_id <- ""
            }
            plt1 <- ggplot() + labs(title=paste("bbasin", polygon_col, bbasin_ids[i],"station_id",station_id)) +
                geom_sf(data=bbasin_outlet, fill="tan2", color="red") +
                geom_sf_text(data=bbasin_outlet, aes_string(label=gew), color="grey20", size=4) 
            plt2 <- ggplot() + labs(title="(cleaned) bbasins upstream of station") +
                geom_sf(data=basin_bind_clean, fill="tan") +
                geom_sf(data=bbasin_outlet, fill="tan2", color="red") 
            plt3 <- ggplot() + labs(title="all bbasins upstream of station based on gew") +
                geom_sf(data=basin_bind, color="grey", fill="pink") +
                geom_sf(data=basin_bind_clean, fill="tan") +
                geom_sf(data=bbasin_outlet, fill="tan2", color="red")
            plt4 <- ggplot() + labs(title="excluded artificial bbasins (during cleaning)") +
                geom_sf(data=dplyr::filter(basin_bind, !objectid %in% basin_bind_clean$objectid),
                         color="grey", fill="pink") 
            # Add rivers to plot
            if (!is.null(rivers_shp)){
                rivers_crop <- sf::st_crop(rivers_shp, basin_bind_clean)
                plt2 <- plt2 + geom_sf(data=rivers_crop, color="blue")
            }
            # List of plots
            plots <- list(plt1, plt2, plt3, plt4)
            # Add catchment outlet to plots
            if (!is.null(stations)){
                plots <- lapply(plots, function(p) {
                    p <- p + geom_sf(data=station, shape=2, size=5, color="purple")
                })
            }
            # Print plots to file
            pdf(file=paste0(plot_dsn,"bbasin_id_", bbasin_ids[i], "_station_id_", station_id,".pdf"))
            lapply(plots, function(p) print(p))
            dev.off()
        }
        
        # Result
        result[[i]] <- basin_bind_clean[[polygon_col]]
        
    } #end loop
    
    return(result)

} # end function.
