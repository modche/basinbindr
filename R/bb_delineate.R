#' Delineation of catchment(s)
#'
#' @param bbasin_ids vector with primary basin IDs for one or more gauging stations
#' @param germanyshape basin shapefile as a \code{sf} object; can be generated with [bb_readgermanyshape()].
#' @param polygon_col name of primary basin ID column in shapefile germanyshape
#' @param geb name of domain column ('Gebietskennzahl'), default = 'geb_kz_num')
#' @param gew name of water body column ('Gewässerkennzahl', default = 'gew_kz_num')
#' @param remove_artifical logical, should artificial water bodies such as canals be removed from procedure?
#' @param coastal_area logical, should coastal waters be flasgged? (not yet implemented)
#' @param station_ids vector of character with station ids corresponding to bbasin_ids, e.g. used for plots if plot_dsn not FALSE
#' @param check_border logical, if TRUE  it is checked if the delineated catchment area intersects the outer boundary of the basinshapefile, a flag is returned
#' @param plot logical, if TRUE plots are saved in one pdf for each bbasin_id, useful for quality checking the results
#' @param plot_dsn character, defines path to save plots (if TRUE) 
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
                         station_ids=NULL,
                         check_border=FALSE,
                         plot=FALSE,
                         plot_dsn="") {
    
    result <- list()    
    
    for (i in 1:length(bbasin_ids)) {
        
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
        
        # Check results visuallyggplot() 
        if (plot==TRUE){
            plt1 <- ggplot() + labs(title=paste("bbasin", polygon_col, bbasin_ids[i],"station_id",station_ids[i])) +
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
                        color="grey", fill="pink") +
                geom_sf(data=bbasin_outlet, fill="tan2") 
            pdf(file=paste0(plot_dsn,"bbasin_id_", bbasin_ids[i], "_station_id_", station_ids[i],".pdf"))
            print(plt1)
            print(plt2)
            print(plt3)
            print(plt4)
            dev.off()
        }
        
        # count how many polgons form the catchment
        #n <- nrow(basin_bind_clean)
        
        result[[i]] <- basin_bind_clean[[polygon_col]]
        
    } #end loop
    
    return(result)

} # end function.
