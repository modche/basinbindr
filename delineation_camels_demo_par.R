# Delineate catchments using the basinbindr parallel workflow 
# for CAMELS 2023/10
#
# @author: Pia Ebeling

### Set up work space ####						  
library(sf)
library(ggplot2)
library(foreach)	
library(doParallel) # to delineate catchments in parallel												 

path_project <- getwd()
source("../R/_init.R") # defines paths to GIS data (path_gis, required) and to rivers dataset (optional)


# 1. Load data ####

# 1.a Load basic basins shapefile and border ####
file_basins <- paste0(path_gis,'/basisezg_bfg_d_v2/basisezg_bfg_d_v2.shp')
file_rivers <- paste0(path_rivers2,'/eu_river_merged.gpkg') # Merged EU Hydro for Germany as geopackage
bb_readgermanyshape(file = file_basins, name = 'basins_germany.shp')
border_shp <- sf::st_read(dsn=paste0(path_gis,"/basisezg_bfg_d_v2_boundary.shp"))  # line shapefile of borders (generated from basisezg in QGIS)


# 1.b Load station data ####
camels_meta <- data.table::fread('../input_data/metadata.csv')

# Remove stations without coordinates
camels_coords <- camels_meta[!is.na(camels_meta$lon) & !is.na(camels_meta$lat)]

# Create SF object using the corresponding coordinate system
camels_stations <- sf::st_as_sf(camels_coords, coords = c('lon', 'lat'), crs = sf::st_crs(4326)) # define coordinate system
ggplot(camels_stations) + geom_sf()

# 2. Preprocess data ####
# Transform	(if required)	   		
if (sf::st_crs(camels_stations)!=sf::st_crs(basins_germany.shp)){ # potentially as bb_transform()
    camels_stations <- sf::st_transform(camels_stations, sf::st_crs(basins_germany.shp))
    print(paste("station coordinate system checked, transformed into", sf::st_crs(camels_stations)[1]))
} else {
    print(paste("station coordinate system checked, already in", sf::st_crs(camels_stations)[1]))
}


# Save geopackage of stations
sf::st_write(camels_stations, dsn=paste0("../output_data/camels_stations_n",nrow(camels_stations),".gpkg"), delete_layer = TRUE)


# 3. Delineate example catchments with basinbindr for testing ####
# 3a. Subset examples for each Federal state (BL)
n <- 3 # number of samples per BL
examples <- c()
for (nut in unique(camels_stations$nuts_lvl2)){
    print(nut) 
    set.seed(1234)
    examples <- c(examples, sample(which(camels_stations$nuts_lvl2==nut), n))
}
examples_ids <- camels_stations$camels_id[examples]
# #write.csv2(examples_ids, file="../example_stations_360.csv", row.names = F) # for reproducibility
# #sf::st_write(camels_stations[examples,], dsn="../output_data/ex360_stations.gpkg") # for gis checks
# # Load a previous selection of examples
# examples_ids <- read.csv2(file="../example_stations.csv")
examples <- which(camels_stations$camels_id %in% examples_ids[,1])
# 3b. Plot selected example stations
ggplot(camels_stations[examples,]) + geom_sf() + geom_sf_text(aes(label=camels_id))


# 3c. Get basin ids of each station
# Basin ids at the catchment outlet (i.e. at station location)
bboutlet_ids <- bb_stationid(camels_stations[examples,], germanyshape = basins_germany.shp, polygon_col = 'objectid', debug = TRUE)

# # Save polygons at outlets bboutlet_ids
# bboutlet_basins_shp <- basins_germany.shp[match(bboutlet_ids, basins_germany.shp$objectid),]
# bboutlet_basins_shp$camels_id <- camels_stations$camels_id[examples]
# ids_without_match <- bboutlet_basins_shp$camels_id[is.na(bboutlet_basins_shp$objectid)]
# # Plot & save
# map <- ggplot() +
#     geom_sf(data=border_shp) +
#     geom_sf(data=bboutlet_basins_shp, aes(fill=land, color=land)) +
#     geom_sf(data=camels_stations[camels_stations$camels_id %in% ids_without_match,], color="red", size=1.2)
# ggsave(file=paste0("../figures/map_ex",n,"_bboutlets.png"), plot=map)
# st_write(bboutlet_basins_shp, dsn="../output_data/ex360_bboutlet.gpkg", delete_layer = TRUE)


# Basins ids of the whole catchment (i.e. upstream area of station)
basins_ids <- bb_delineate(bbasin_ids=bboutlet_ids, germanyshape=basins_germany.shp, remove_artificial = TRUE, 
                           plot=TRUE, plot_dsn=paste0("../figures/bbasins_",n,"/"),
                           stations=camels_stations[examples,], station_id_col = "camels_id",
                           plot_rivers=file_rivers
                           )

# 3d. Delineate example catchments
start_time2 <- Sys.time()
examples_shp_par <- bb_catchments_par(station_basins=basins_ids, ncores=40, 
                                      germanyshape = basins_germany.shp, polygon_col = 'objectid',
                                      save=TRUE, dsn=paste0("../output_data/camels_examples",n,"_singlelayer.gpkg"), 
                                      dissolve=T, stations=camels_stations[examples,], station_id_col = "camels_id")
end_time2 <- Sys.time()
print(end_time2 - start_time2)

# 3e. Flag borders
examples_shp_par$border <- as.factor(st_intersects(examples_shp_par, border_shp, sparse=FALSE))

# Save catchments as geopackage ####
#sf::st_write(examples_shp_par, dsn=paste0("../output_data/ex",n,"_catchments_singlelayer.gpkg"), delete_layer = TRUE)

# Plot delineated catchments
range(examples_shp_par$area, na.rm=T)
range(examples_shp_par$area_bb, na.rm=T)
map <- ggplot() + 
    geom_sf(data=border_shp, color="black") +
    geom_sf(data=examples_shp_par, aes(fill=nuts_lvl2), alpha=0.5)
#ggsave(file=paste0("../figures/map_ex",n,"_catchments_nuts_lvl2.png"), plot=map)
plt <- ggplot(examples_shp_par, aes(x=area, y=area_bb, color=border)) + geom_abline(slope=1, intercept=0) + 
    geom_point(alpha=0.5) 
plt
#ggsave(file=paste0("../figures/scatter_area_bb_ex",n,"_catchments.png"), plot=plt)







# 4. Delineate all CAMELS catchments
n <- nrow(camels_stations)
start_time2 <- Sys.time()
print(start_time2)

# 4a. Basin ids at the catchment outlet (i.e. at station location)
bboutlet_ids <- bb_stationid(camels_stations, germanyshape = basins_germany.shp, polygon_col = 'objectid', debug = TRUE)

# 4b. Basins ids of the whole catchment (i.e. upstream area of station)
basins_ids <- bb_delineate(bbasin_ids=bboutlet_ids, germanyshape=basins_germany.shp, remove_artificial = TRUE, 
                           plot=F, plot_dsn=paste0("../figures/bbasins_n",n,"/"),
                           stations=camels_stations, station_id_col = "camels_id",
                           plot_rivers=file_rivers
)

# 4c. Delineate catchment polygons
camels_shp_par <- bb_catchments_par(station_basins=basins_ids, ncores=40, 
                                      germanyshape = basins_germany.shp, polygon_col = 'objectid',
                                      save=TRUE, dsn=paste0("../output_data/camels_n",n,"_singlelayer.gpkg"), 
                                      dissolve=T, stations=camels_stations, station_id_col = "camels_id")
end_time2 <- Sys.time()
print(end_time2 - start_time2)

# 4d. Flag borders
camels_shp_par$border <- as.factor(st_intersects(camels_shp_par, border_shp, sparse=FALSE))

# 4e. Save geopackage of catchments
n_delin <- nrow(camels_shp_par)
sf::st_write(camels_shp_par, dsn=paste0("../output_data/camels_catchments_n",n_delin,".gpkg"), delete_layer = TRUE)


# 4f. Check non-delineated catchments/stations
ids_err <- camels_stations$camels_id[!camels_stations$camels_id %in% camels_shp_par$camels_id]
ggplot() + 
    geom_sf(data=border_shp, color="black") +
    geom_sf(data=camels_stations[camels_stations$camels_id %in% ids_err,], size=4, color="red")
# Save geopackage of stations
camels_stations_delineated <- camels_stations[!camels_stations$camels_id %in% ids_err,]
sf::st_write(camels_stations_delineated, dsn=paste0("../output_data/camels_stations_n",nrow(camels_stations_delineated),".gpkg"), delete_layer = TRUE)


# 4.g Plot/Check delineated catchments
range(camels_shp_par$area, na.rm=T)
range(camels_shp_par$area_bb, na.rm=T)

# Map
map <- ggplot() + 
    geom_sf(data=border_shp, color="black") +
    geom_sf(data=camels_shp_par, aes(fill=nuts_lvl2), alpha=0.5) +
    geom_sf(data=camels_stations_delineated, color="darkblue", size=1, alpha=0.5)
#ggsave(file=paste0("../figures/map_n",n_delin,"_catchments_nuts_lvl2.png"), plot=map)
# Scatterplot or catchment areas
plt <- ggplot(camels_shp_par, aes(x=area, y=area_bb, color=border)) + geom_abline(slope=1, intercept=0) + 
    geom_point(alpha=0.5) 
plt <- plt + facet_wrap(~federal_state + nuts_lvl2, scales="free")
#ggsave(file=paste0("../figures/scatter_area_bb_n",n_delin,"_catchments.png"), plot=plt)


