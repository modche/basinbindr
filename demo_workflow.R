#workflow

shp_file <- '/Users/modchemba/Dropbox/IHF/CAMELS_DE/_waterbody_number/_gis_shapes/basisezg_bfg_d_v2.shp'

# 1a
bb_readgermanyshape(file = shp_file, name = 'basins_germany')

# 1b set a station (i.e. coordinates of gauging station)
#dummy_station <- data.frame(x = c(7.9,11.1234), y = c(47.9,50.345676542))
dreisam_station <- data.frame(x = c(7.898806, 9.1889), y = c(47.987748, 50.1988))
test_station <- data.frame(x = c(7.898806, 9.1889, 8.902567), y = c(47.987748, 50.1988, 50.131758))


# 2.
dreisam_id <- bb_stationid(test_station, germanyshape = basins_germany, polygon_col = 'objectid')

# 3. extract catchment ids now
bb_delineate(c(dreisam_id), basins_germany, remove_artifical = TRUE)


c2 <- bb_delineate(c(dreisam_id), basins_germany, remove_artifical = TRUE)

selected_catchment <- germanyshape[germanyshape$objectid %in% c2[[3]],] 
plot(selected_catchment["geb_kz_num"], key.pos = 1, border = 'white')


selected_catchment %>% 
    mutate(no = str_extract(geb_kz_num, '[1-9]*')) %>% 
    mutate(len = str_length(no)) %>% 
    mutate(cut = str_sub(no, min(len), min(len))) %>% 
    mutate(cut2 = str_sub(geb_kz_num, 1,5)) %>% 
    ggplot() +
        geom_sf(aes(fill = cut2 )) +
        theme_void() +
        scale_fill_viridis_d()
    