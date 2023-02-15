#workflow
library(sf)
library(ggplot2)

# set working space ####
path_project <- getwd()
source("../R/_init.R") # path_gis to basin shapefile needs to be defined
shp_file <- paste0(path_gis,'/basisezg_bfg_d_v2.shp')


# 1. Load data ####

# 1.a basic basins shapefile ####
bb_readgermanyshape(file = shp_file, name = 'basins_germany.shp')
# Union/combine for borders
# borders.shp <- st_union(basins_germany.shp) # do not run! takes ages!
# borders.shp <- st_combine(basins_germany.shp) # does not merge overlapping polygons!
ggplot() + 
    geom_sf(data=borders.shp, color="black") + 
    geom_sf(data=basins_germany.shp[1:300,], color="blue")


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
examples <- c()
for (nut in unique(camels_stations$nuts_lvl2)){
    print(nut)
    examples <- c(examples, sample(which(camels_stations$nuts_lvl2==nut), 3))
}
ggplot(camels_stations[examples,]) + geom_sf() + geom_sf_text(aes(label=camels_id))
write.csv2(camels_stations$camels_id[examples],file="../example_stations.csv", row.names = F) # for reproducibility


# 2. Extract ids of basic basins for each station (i.e. bbasin of the catchment outlet)
xy_ids <- bb_stationid(camels_stations[examples,], germanyshape = basins_germany.shp, polygon_col = 'objectid', debug = TRUE)


# 3. Extract all basic basins ids of the catchment (i.e. upstream area of station)
catchment_ids <- bb_delineate(bbasin_ids=xy_ids, germanyshape=basins_germany.shp, remove_artifical = TRUE, 
                              station_ids=camels_stations$camels_id[examples], plot=TRUE, plot_dsn="../figures/bbasins/")

# 4. Calculate the area of the whole catchment
areas <- bb_area(catchment_ids, germanyshape = basins_germany.shp, polygon_col = 'objectid')


a <- cbind(camels, area_delineation = areas) %>% tibble() %>% 
    mutate(diff = area_delineation - area, ratio = diff / area_delineation)






# -----------------------------------------------------------------------------
# Trier
# -----------------------------------------------------------------------------

# Install the package from Github
# install.packages("devtools")
#devtools::install_github("modche/basinbindr")

library(basinbindr)

# Prepare BfG Basiseinzugsgebiete
shp_file <- '/Users/modchemba/Dropbox/IHF/CAMELS_DE/_waterbody_number/_gis_shapes/basisezg_bfg_d_v2.shp'
bb_readgermanyshape(file = shp_file, name = 'basins_germany.shp')


# Pegel X Y
trier <- read.table('/Users/modchemba/Desktop/trier/Stationen_Koordinaten_Q.csv', header = T, sep = ";")
trier4326 <- sf::st_as_sf(trier, coords = c('WGS84_Lon', 'WGS84_Lat'), crs = sf::st_crs(4326)) 
trier_xy <- data.frame(sf::st_coordinates(trier4326))
names(trier_xy) <- c('x','y')

# 2.
xy_ids <- bb_stationid(trier_xy, germanyshape = basins_germany.shp, polygon_col = 'objectid', debug = TRUE)

# 3. extract catchment ids now
catchment_ids <- bb_delineate(xy_ids, basins_germany.shp, remove_artifical = TRUE)

areas <- bb_area(catchment_ids, germanyshape = basins_germany.shp, polygon_col = 'objectid')

trier$areas <- areas

for (i in 1:nrow(trier)) {
    cat(i,"\n")
    shp <- basins_germany.shp %>% filter(objectid %in% catchment_ids[[i]])
    
    layer_string <- paste(trier$Messstelle[i],trier$Stationsname[i],trier$Gewaesser[i], sep ="_")
    
    sf::st_write(obj = shp, 
             dsn = '~/Desktop/shp/shapes_trier_v2.gpkg', # export ist im augenblick basisezgs (feature)
             layer = layer_string, append=TRUE)
    
}

 
    
    
    





extract_catchment <- function(x_lon, y_lat, shape,
                              plot = FALSE,
                              area_calc = FALSE,
                              write_shp = FALSE) {
    
    result <- 'no area/no plotting'
    
    # Pegelstandort als simple feature (sf) point aufbereiten
    point_xy <- st_sfc(st_point(c(x_lon, y_lat)), crs = 4326)
    sf_xy = st_sf(data.frame(id=1, geom=point_xy))
    
    #Erst alle passenden bboxes suchen, dann st_within (viel schneller!)
    #es kann mehr als 1 passende bbox für jeden Pegelort geben
    temp_shape <- shape %>% filter(x_lon > xmin, x_lon < xmax, y_lat > ymin, y_lat < ymax)
    selected_basis_catchment <- st_join(sf_xy, temp_shape, join = st_within)
    
    #Abfrage um Basis-EZG oberhalb des Pegel(standorts) zu extrahieren
    selected_shapes <- shape %>% filter(geb_kz_num <= selected_basis_catchment$geb_kz_num, # mit/one BAsis-Ezg des Pegels?
                                        gew_kz_num >= selected_basis_catchment$gew_kz_num) %>% 
        filter(abs(gew_kz_num-geb_kz_num) < 1e14) # 72 kanäle und überleitungen entfernen
    
    print(selected_basis_catchment$geb_kz_num)
    print(selected_basis_catchment$gew_kz_num)
    
    # export shapefile
    if(write_shp) {
        file_string <- paste0(selected_basis_catchment$objectid,'_',selected_basis_catchment$river1)
        st_write(obj = selected_shapes, 
                 dsn = '~/Desktop/shp/shapes_extract.gpkg',
                 layer = file_string, append=FALSE)
    }
    
    # ploting function
    if(plot) {
        g <- ggplot() +
            geom_sf(data = selected_shapes, aes(fill = factor(gew_kz_num)),
                    colour = "black", size = .1) +
            geom_sf(data = sf_xy, size = 3, colour = "black") +
            theme_minimal() +
            guides(fill = 'none')
        print(g)
    } #end plot
    
    # Fläche berechnen aus Summe Basiseinzugsgebiete
    if(area_calc) {
        
        result <-   selected_shapes %>%
            mutate(area = as.numeric(st_area(geometry))) %>%
            summarise(area = sum(area)/1e6) %>%
            pull(area)
    } #end area
    return(result)
}
