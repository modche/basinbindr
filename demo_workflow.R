#workflow

shp_file <- '/Users/modchemba/Dropbox/IHF/CAMELS_DE/_waterbody_number/_gis_shapes/basisezg_bfg_d_v2.shp'

# 1.
bb_readgermanyshape(file = shp_file, name = 'basins_germany')

dummy_station <- data.frame(x = c(7.9,11.1234), y = c(47.9,50.345676542))

# 2.
bb_stationid(dummy_station, germanyshape = basins_germany, polygon_col = 'objectid')
