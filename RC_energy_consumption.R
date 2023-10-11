
# https://www.transpower.co.nz/hawkes-bay-and-gisborne-power-outage
# https://www.transpower.co.nz/news/transpower-and-powernet-seek-input-southlands-future-electricity-needs


suppressPackageStartupMessages(library("sf"))
suppressPackageStartupMessages(library("dplyr"))
suppressPackageStartupMessages(library("spgwr"))
suppressPackageStartupMessages(library("proj4"))
suppressPackageStartupMessages(library("ggplot2"))
suppressPackageStartupMessages(library("ggspatial",warn.conflicts=FALSE))
# Other useful packages not available: cartography for map display and spatstat for pts density analysis


# Load stations metadata location

proj4 <- "+proj=tmerc +lat_0=0 +lon_0=173 +k=0.9996 +x_0=1600000 +y_0=10000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs "

stations_metadata_locations_tmp<-read.csv("analysis/20230919_electricity_data_module/substations_location/stations_location_metadata_geocoded.csv")

stations_metadata_locations<-stations_metadata_locations_tmp %>% 
                            select(NSP,"POC.code","Network.participant","Description","NZTM.easting","NZTM.northing") %>% # Change to include or exlude geolocation .final
                            mutate(row=row_number())

# From the source data extract the x, y coordinates of the UTM

x_y <- stations_metadata_locations %>% 
   dplyr::select('NZTM.easting','NZTM.northing') # Change to include or exlude geolocation .final


# Transform the data

lon_lat <- proj4::project(x_y, proj4, inverse = TRUE) 


# Convert to a data frame

lon_lat2 <- data.frame(Longitude = lon_lat$x, Latitude = lon_lat$y) %>% 
   mutate(row=row_number())


# Add to the original file

stations_metadata_locations2 <- stations_metadata_locations %>%
   left_join(y=lon_lat2,by=c("row"="row")) 


# Convert location metadata data frame to a spatial data frame - removing missing coordinates because they don't allow the conversion

stations_metadata_locations3<-stations_metadata_locations2 %>% filter(!is.na(Longitude) & !is.na(Latitude)) 


# Load RC polygons shapefile


rc_polygons_tmp<- sf::read_sf("analysis/20230919_electricity_data_module/statsnz-regional-council-2023-clipped-generalised-SHP/regional-council-2023-clipped-generalised.shp")

rc_polygons<- rc_polygons_tmp%>% mutate(Area_Sub=gsub( " .*$", "",REGC2023_1),
                                        Area=case_when(Area_Sub=="West"~"West Coast",
                                                       Area_Sub=="Hawke's"~"Hawke's Bay",
                                                       Area_Sub=="Manawatu-Wanganui"~"Manawatū-Whanganui",
                                                       Area_Sub=="Bay" ~ "Bay of Plenty",
                                                       TRUE ~ Area_Sub)) %>% dplyr::select(-c("Area_Sub")) %>%
                                 dplyr::select(-c("REGC2023_1","REGC2023_2","LAND_AREA_","AREA_SQ_KM","Shape_Leng"))


# Calculate the land area in square meters to redistribute the consumption in missing regions

rc_polygons$land_m2 <- as.vector(sf::st_area(rc_polygons))


# Define the Stations as points(pts) to perform the Point in Polygon Analysis

pts <- sf::st_as_sf(stations_metadata_locations3, coords = c("Longitude", "Latitude"), crs = 4326) %>%
   left_join(stations_metadata_locations3[,c("row","Longitude","Latitude")],by="row")



# Define the RCs as polygons to perform the Point in Polygon Analysis

p <- sf::st_as_sf(rc_polygons)  

# Make sure polygons and location metadata have the same coordinate reference system

crs_poly<-sf::st_crs(p)
pts <- sf::st_transform(pts, crs = crs_poly)
p <- sf::st_transform(p, crs = crs_poly)

#  Perform the Point in Polygon analysis using the Intersect function

pts_index_rc <- sf::st_intersection(pts, p) %>% sf::st_drop_geometry()


nrow(stations_metadata_locations)
nrow(pts_index_rc)

# wrong_geolocation<-pts %>% filter(Latitude>-30) # Checking points outside the Nz boundaries
# write.csv(wrong_geolocation,"analysis/20230919_electricity_data_module/substations_location/check.csv",row.names=FALSE)


# Plotting substations


nz_all<-sf::st_union(rc_polygons)

map_substations <- ggplot() + geom_sf(data = nz_all ) + #  rc_polygons
   geom_sf(data = pts, color = "black",shape=16) +  # aes(color=`Network.participant`) # pts %>% filter(POC.code=="TUI1101")
   xlab(expression(paste("Longitude (", degree, "E)"))) +
   ylab(expression(paste("Latitude (", degree, "S)"))) +
   scale_x_continuous(breaks = seq(170, 184, by = 2)) +
   labs(title = "Substations location",
        subtitle = "EA",
        #subtitle="Colors based on Network Participant",
        caption = "Source: Markets") +
   theme(legend.position = "none") 

map_substations

#ggplot2::ggsave(map_substations, file = 'analysis/20230919_electricity_data_module/outputs/substations_location.jpg', width = 6.77, height = 7.19)


# Reading consumption data file generated by the Markets team

consumption_rds<-readRDS("analysis/20230919_electricity_data_module/markets_data/monthly_gxp_demand.rds") %>%
                  mutate(Year=as.numeric(format(TimePeriod, "%Y"))) %>%
                  dplyr::select(-c("Generation_Type","Unit","DaysInMonth"))


# Joining stations metadata location data to the consumption data



consumption_rc_allocated<-consumption_rds %>% 
                         left_join(pts_index_rc,by=c("POC"="POC.code"),relationship="many-to-many") %>%
                          select(-c("row")) %>%
                          mutate(REGC2023_V=case_when(POC=="TUI1101"~"05",
                                                      TRUE ~ REGC2023_V),
                                 Area=case_when(POC=="TUI1101"~"Gisborne",
                                                TRUE ~ Area),
                                 land_m2=case_when(POC=="TUI1101"~8385065178.,
                                                   TRUE ~ land_m2))


consumption_rc_aggregated<-consumption_rc_allocated %>% group_by(Year,Area) %>% 
                            summarize(csmp_rc_tot=sum(Value,na.rm=TRUE)/1e6) 


# Redistributing consumption between Gisborne and Hawkes' Bay 

# consumption_rc_aggregated_gisborne<-data.frame(Year=seq(from=1997,to=2023,by=1),Area=rep("Gisborne", 2023-1997+1),csmp_rc_tot=0)
# 
# consumption_rc_aggregated_added<- rbind(consumption_rc_aggregated,consumption_rc_aggregated_gisborne) %>% 
#    filter(!is.na(Area)) %>% arrange(Year) %>%
#    left_join(rc_polygons %>% sf::st_drop_geometry() %>% dplyr::select(Area,land_m2),by="Area",relationship="many-to-many") 
# 
# 
# consumption_rc_aggregated_fnl<-consumption_rc_aggregated_added %>% 
#    filter(Area %in% c("Gisborne","Hawke's Bay")) %>%
#    group_by(Year) %>%
#    mutate(csmp_rc_tot_fnl=sum(csmp_rc_tot)*(land_m2/sum(land_m2))) %>%
#    rbind(consumption_rc_aggregated_added %>% filter(!Area %in% c("Gisborne","Hawke's Bay")) %>% mutate(csmp_rc_tot_fnl=csmp_rc_tot)) %>%
#    arrange(Year)
# 



# Getting data ready to map

consumption_rc_map<-rc_polygons %>% left_join(consumption_rc_aggregated, by = "Area") %>% arrange("Area") %>% # consumption_rc_aggregated_fnl
   filter(!is.na(Year) & Year>=2019 & Year<=2022) 


# Mapping electricity consumption data 2019-2022

map_rc_consumption<-ggplot() + 
           geom_sf(data=nz_all,fill="white") + 
           geom_sf(data=consumption_rc_map,aes(fill = csmp_rc_tot)) + #  csmp_rc_tot_fnl
           facet_wrap(~ Year) + 
           scale_fill_continuous(labels=scales::comma,name = 'GWh',high = "#132B43", low = "#56B1F7",breaks = c(2.5e3,5e3,7.5e3,10e3)) + 
           scale_x_continuous(breaks = seq(170, 184, by = 3)) +
           scale_y_continuous(labels=scales::comma) +
           xlab(expression(paste("Longitude (", degree,"E)"))) +
           ylab(expression(paste("Latitude (", degree,"S)"))) +
           labs(title="Total Energy Consumption",
                subtitle="By Regional Council",
                caption="Source: Markets")

map_rc_consumption

ggplot2::ggsave(map_rc_consumption, file = 'analysis/20230919_electricity_data_module/outputs/RC_total_energyconsumption.jpg', width = 6.77, height = 7.19)



# Importing population

estpop<-mbieDBmisc::ImportREAR("Estimated resident population") %>% rename(Areatmp=Area)

estpoprc<- estpop %>% filter(AreaType=="Regional Council") %>% 
                      dplyr::select(AreaID,Areatmp,AreaType,Year,Value) %>% 
                      mutate(Area=case_when(Areatmp=="Manawatu-Wanganui"~"Manawat\u16b-Whanganui",
                                            TRUE ~ Areatmp)) %>% select(-c("Areatmp"))

estpoprc_aggregated<- estpoprc %>% 
                      group_by(Year,Area) %>% 
                      summarize(pop_rc_tot=sum(Value,na.rm=TRUE)) %>%
                      left_join(unique(estpoprc[,c("AreaID","Area")]),by="Area",relationship="many-to-many")
   

# Calculating consumption per capita


consumption_percapita<-consumption_rc_aggregated %>%
                        left_join(estpoprc_aggregated,by=c("Area","Year"),relationship="many-to-many")  %>%
                        mutate(csmp_rc_percapita=(csmp_rc_tot*1e6)/pop_rc_tot)
    

# Getting data ready to map

consumption_percapita_map<- rc_polygons %>% left_join(consumption_percapita, by = "Area") %>% arrange("Area") %>% 
                            filter(Year==2022) %>% 
                            mutate(group=case_when(Area=="Southland"~"Southland",
                                                   TRUE ~ "Excluding Southland"))


# Quantile of the consumption per capita

bk <- quantile(consumption_percapita_map$csmp_rc_percapita, seq(from = 0.25, to = 1, length.out = 5))


# Mapping electricity consumption per capita data for 2022

ns %>% filter(Area=="Southland") %>% sf::st_drop_geometry() %>% dplyr::select(csmp_rc_percapita)

map_rc_percapita<-ggplot() + 
           geom_sf(data=nz_all,fill="white") +
           geom_sf(data=consumption_percapita_map %>% filter(!Area=="Southland"),aes(fill = csmp_rc_percapita)) +
           scale_fill_continuous(labels=scales::comma, name = 'KWh per capita',high = "#132B43", low = "#56B1F7") +
           geom_sf(data=consumption_percapita_map %>% filter(Area=="Southland"),fill="black") +
           facet_wrap(~ Year) +
           scale_x_continuous(breaks = seq(170, 184, by = 3)) +
           scale_y_continuous(labels=scales::comma) +
           xlab(expression(paste("Longitude (", degree,"E)"))) +
           ylab(expression(paste("Latitude (", degree,"S)"))) +
           labs(title="Per capita Energy Consumption",
                subtitle="By Regional Council - Excluding Southland",
                caption="Source: Markets \n
                                 Southland  
                                 113,000 Kwh")
 

ggplot2::ggsave(map_rc_percapita, file = 'analysis/20230919_electricity_data_module/outputs/RC_percapita_energyconsumption.jpg', width = 6.77, height = 7.19)








