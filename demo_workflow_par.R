#workflow
library(sf)
library(ggplot2)

# set working space ####
path_project <- getwd()
source("../R/_init.R") # defines paths to GIS data, minimum basin shapefile needs to be defined
file_basins <- paste0(path_gis,'/basisezg_bfg_d_v2/basisezg_bfg_d_v2.shp')
#file_rivers <- paste0(path_rivers,'/GER_Net.shp') # EU Hydro Note: here still beta version
border_shp <- sf::st_read(dsn=paste0(path_gis,"/basisezg_bfg_d_v2_boundary.shp"))  # line shapefile of borders (generated from basisezg in QGIS)


# 1. Load data ####

# 1.a basic basins shapefile ####
bb_readgermanyshape(file = file_basins, name = 'basins_germany.shp')
# ggplot() + 
#     #geom_sf(data=borders.shp, color="black") + 
#     geom_sf(data=basins_germany.shp[1:300,], color="blue")


# 1.b station meta data ####
#camels_meta <- read.table('../input_data/pegel_xy_area_2023_01_12.txt', header = T)
camels_meta <- data.table::fread('../input_data/metadata.csv')

# Remove stations without coordinates
camels_coords <- camels_meta[!is.na(camels_meta$lon) & !is.na(camels_meta$lat)]

# Transform into coordinate system of basins shape
camels_stations <- sf::st_as_sf(camels_coords, coords = c('lon', 'lat'), crs = sf::st_crs(4326)) # define coordinate system
ggplot(camels_stations) + geom_sf()

if (sf::st_crs(camels_stations)!=sf::st_crs(basins_germany.shp)){ # potentially as bb_transform()
    camels_stations <- sf::st_transform(camels_stations, sf::st_crs(basins_germany.shp))
    print(paste("station coordinate system checked, transformed into", sf::st_crs(camels_stations)[1]))
} else {
    print(paste("station coordinate system checked, already in", sf::st_crs(camels_stations)[1]))
}


# 1.c Select example stations from each BL and check visually
# examples <- c()
# for (nut in unique(camels_stations$nuts_lvl2)){
#     print(nut)
#     examples <- c(examples, sample(which(camels_stations$nuts_lvl2==nut), 40))
# }
# examples_ids <- camels_stations$camels_id[examples]
# #write.csv2(examples_ids, file="../example_stations_360.csv", row.names = F) # for reproducibility
# #sf::st_write(camels_stations[examples,], dsn="../output_data/ex360_stations.gpkg") # for gis checks
# # Load a previous selection of examples
# examples_ids <- read.csv2(file="../example_stations.csv")
examples <- which(camels_stations$camels_id %in% examples_ids[,1])
# Plot selected example stations
#ggplot(camels_stations[examples,]) + geom_sf() + geom_sf_text(aes(label=camels_id))


# 2. Extract ids of basic basins for each station (i.e. bbasin of the catchment outlet)
bboutlet_ids <- bb_stationid(camels_stations[examples,], germanyshape = basins_germany.shp, polygon_col = 'objectid', debug = TRUE)

# # Save polygons at outlets bboutlet_ids
# bboutlet_basins_shp <- basins_germany.shp[match(bboutlet_ids, basins_germany.shp$objectid),]
# bboutlet_basins_shp$camels_id <- camels_stations$camels_id[examples]
# ids_without_match <- bboutlet_basins_shp$camels_id[is.na(bboutlet_basins_shp$objectid)]
# # Plot & save
map <- ggplot() + 
    geom_sf(data=border_shp) +
    geom_sf(data=bboutlet_basins_shp, aes(fill=land, color=land)) +
    geom_sf(data=camels_stations[camels_stations$camels_id %in% ids_without_match,], color="red", size=1.2)
#ggsave(file="../figures/map_ex360_bboutlets.png", plot=map)

# st_write(bboutlet_basins_shp, dsn="../output_data/ex360_bboutlet.gpkg", delete_layer = TRUE)


# 3. Extract all basic basins ids of the catchment (i.e. upstream area of station)
basins_ids <- bb_delineate(bbasin_ids=bboutlet_ids, germanyshape=basins_germany.shp, remove_artificial = TRUE, 
                           plot=TRUE, plot_dsn="../figures/bbasins_360/",
                           stations=camels_stations[examples,], station_id_col = "camels_id",
                           plot_rivers=file_rivers
                           )

# 4. Union and Save catchments as geopackage
# Station shape (with metadata)
stations_shp <- camels_stations[examples,] # all stations to be divided into layers

# # Create catchments without layer parallized code ####
# start_time <- Sys.time()
# examples_shp <- bb_catchments(station_basins=basins_ids, germanyshape = basins_germany.shp, polygon_col = 'objectid',
#                                   save=FALSE, dsn="../output_data/camels_examples_singlelayer.gpkg", dissolve=T,
#                                   stations=stations_shp, station_id_col = "camels_id")
# end_time <- Sys.time()
# print(end_time - start_time)
# Create catchments without layer (parallel)
library(doParallel)
library(foreach)
start_time2 <- Sys.time()
examples_shp_par <- bb_catchments_par(station_basins=basins_ids, ncores=40, germanyshape = basins_germany.shp, polygon_col = 'objectid',
                                  save=FALSE, dsn="../output_data/camels_examples_singlelayer.gpkg", dissolve=T,
                                  stations=stations_shp, station_id_col = "camels_id")
end_time2 <- Sys.time()
print(end_time2 - start_time2)

# Flag borders
examples_shp$border <- as.integer(st_intersects(examples_shp, border_shp, sparse=FALSE))

# Check/Plot catchments ####
range(examples_shp_par$area, na.rm=T)
range(examples_shp_par$area_bb, na.rm=T)
map <- ggplot() + 
    geom_sf(data=border_shp) +
    geom_sf(data=examples_shp_par, aes(fill=nuts_lvl2), alpha=0.5)
#ggsave(file="../figures/map_ex360_catchments_nuts_lvl2.png", plot=map)
plt <- ggplot(examples_shp_par, aes(x=area, y=area_bb, color=border)) + geom_abline(slope=1, intercept=0) + 
    geom_point(alpha=0.5) 
plt
#ggsave(file="../figures/scatter_area_bb_ex360_catchments.png", plot=plt)

# Save catchments as geopackage ####
#sf::st_write(examples_shp_par, dsn="../output_data/ex360_catchments_singlelayer.gpkg", delete_layer = TRUE)


# Create catchments by federal state as layer in geopackage 
# TODOOOO code as a wrapper function around bb_catchments with layer as column name to separate groups
start_time2 <- Sys.time()
identities <- stations_shp$nuts_lvl2 # Bundesländer
#identities <- stations_shp$camels_id # Stations
catchments_shp <- NULL # results will be saved here
for (layer in unique(identities)){
    print(layer)
    idx_layer <- which(identities %in% layer) # get indices of stations in layer
    
    # subset stations and basins for layer
    stations_layer <- stations_shp[idx_layer,]
    basins_layer <- basins_ids[idx_layer]
    
    # get catchment polygons for layer
    layer_shp <- bb_catchments(station_basins=basins_layer, germanyshape = basins_germany.shp, polygon_col = 'objectid',
                               stations=stations_layer, station_id_col = "camels_id", dissolve=T,
                               save=FALSE, dsn="../output_data/camels_examples_2.gpkg", layer=layer)
    layer_shp$layer <- layer
    
    # Append layer to results
    catchments_shp <- rbind(catchments_shp, layer_shp)
}
end_time2 <- Sys.time()
print(end_time2 - start_time2)
ggplot() + geom_sf(data=catchments_shp[1:3,], aes(fill=layer))

# Flag catchments touching the border
setwd(path_gis)
border_shp <- sf::st_read(dsn="../basisezg_bfg_d_v2_boundary.shp")  # line shapefile of borders (generated from basisezg in QGIS)
setwd(path_project)
catchments_shp$border <- as.integer(st_intersects(catchments_shp, border_shp, sparse=FALSE))

# Plot
map <- ggplot() + geom_sf(data=catchments_shp, aes(fill=border, color=border))
map + geom_sf(data=border_shp)

# Save all layers jointly in one layer
sf::st_write(catchments_shp, dsn="../output_data/camels_examples_singlelayer.gpkg", delete_layer = TRUE)
catchments_shp <- sf::st_read(dsn="../output_data/camels_examples_singlelayer.gpkg")



# Save stations geopackage
# Flag delineated/non-delineated ####

# #sf::st_write(camels_stations[examples,], dsn="../output_data/example_stations.gpkg") # for gis checks




