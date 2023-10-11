
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


# Calculate the land area in square meters to redistribute the generation missing regions

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

map_substations <- ggplot() + geom_sf(data = nz_all) +
   geom_sf(data = pts, color = "black",shape=16) +  #aes(color=`Network.participant`)
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


# Reading generation data file from the Markets team

generation_rds_tmp<-readRDS("analysis/20230919_electricity_data_module/markets_data/gen_data.rds")

generation_rds<- generation_rds_tmp %>%
                  group_by(POC_Code,Date) %>%
                  summarize(Value=sum(Value,na.rm=TRUE)) %>% 
                  mutate(Year=as.numeric(format(Date, "%Y")))


# Joining stations metadata location data to the generation data

generation_rc_allocated<-generation_rds %>% 
                         left_join(pts_index_rc,by=c("POC_Code"="POC.code"),relationship="many-to-many") %>%
                          select(-c("row")) %>%
                          mutate(REGC2023_V=case_when(POC_Code=="TUI1101"~"05",
                                                      TRUE ~ REGC2023_V),
                                 Area=case_when(POC_Code=="TUI1101"~"Gisborne",
                                                         TRUE ~ Area),
                                 land_m2=case_when(POC_Code=="TUI1101"~8385065178.,
                                                  TRUE ~ land_m2))


#View(generation_rc_allocated %>% filter(POC_Code=="STK0661")) # Nelson

generation_rc_aggregated_westcoast<-data.frame(Year=seq(from=1997,to=2023,by=1),Area=rep("West Coast", 2023-1997+1),gnrt_rc_tot=0)

generation_rc_aggregated_tasman<-data.frame(Year=seq(from=1997,to=2023,by=1),Area=rep("Tasman", 2023-1997+1),gnrt_rc_tot=0)

generation_rc_aggregated<-generation_rc_allocated %>% group_by(Year,Area) %>% 
                            summarize(gnrt_rc_tot=sum(Value,na.rm=TRUE)/1e6) %>%
                            rbind(generation_rc_aggregated_westcoast) %>%
                            rbind(generation_rc_aggregated_tasman) %>%
                            filter(!is.na(Area)) %>%
                            arrange(Year)

#write.csv(generation_rc_aggregated,"analysis/20230919_electricity_data_module/generation_rc_aggregated.csv")


# Redistributing generation between Gisborne and Hawkes' Bay 

# generation_rc_aggregated_gisborne<-data.frame(Year=seq(from=1997,to=2023,by=1),Area=rep("Gisborne", 2023-1997+1),gnrt_rc_tot=0)
# 
# generation_rc_aggregated_added<- rbind(generation_rc_aggregated,generation_rc_aggregated_gisborne) %>% 
#                                filter(!is.na(Area)) %>% arrange(Year) %>%
#                                left_join(rc_polygons %>% sf::st_drop_geometry() %>% dplyr::select(Area,land_m2),by="Area",relationship="many-to-many") 
# 
# 
# generation_rc_aggregated_fnl<-generation_rc_aggregated_added %>% 
#                                filter(Area %in% c("Gisborne","Hawke's Bay")) %>%
#                                group_by(Year) %>%
#                                mutate(gnrt_rc_tot_fnl=sum(gnrt_rc_tot)*(land_m2/sum(land_m2))) %>%
#                                rbind(generation_rc_aggregated_added %>% filter(!Area %in% c("Gisborne","Hawke's Bay")) %>% mutate(gnrt_rc_tot_fnl=gnrt_rc_tot)) %>%
#                                arrange(Year)
#    


# Getting data ready to map

generation_rc_map<-rc_polygons %>% left_join(generation_rc_aggregated, by = "Area") %>% arrange("Area") %>%  # generation_rc_aggregated_fnl
   filter(!is.na(Year) & Year>=2019 & Year<=2022) 


# Quantile of the generation

quantile(generation_rc_map$gnrt_rc_tot, seq(from = 0.25, to = 1, length.out = 5)) # gnrt_rc_tot_fnl


# Mapping electricity generation data 2019-2022

map_rc_generation<-ggplot() + 
   geom_sf(data=nz_all,fill="white") + 
   geom_sf(data=generation_rc_map %>% filter(!Area %in% c("West Coast","Tasman")),aes(fill = gnrt_rc_tot)) + #gnrt_rc_tot_fnl
   geom_sf(data=generation_rc_map %>% filter(Area %in% c("West Coast","Tasman")),fill="black") +
   facet_wrap(~ Year) + 
   scale_fill_continuous(labels=scales::comma,name = 'GWh',high = "#00994C", low = "#CCFFE5",breaks=c(4e3,8e3,12e3,16e3)) + 
   scale_x_continuous(breaks = seq(170, 184, by = 3)) +
   scale_y_continuous(labels=scales::comma) +
   xlab(expression(paste("Longitude (", degree,"E)"))) +
   ylab(expression(paste("Latitude (", degree,"S)"))) +
   labs(title="Total Energy Generation",
        subtitle="By Regional Council",
        caption="Source: Markets \n
                         Tasman and West Coast only power local lines networks (not connected to the grid)")

map_rc_generation

ggplot2::ggsave(map_rc_generation, file = 'analysis/20230919_electricity_data_module/outputs/RC_total_energygeneration.jpg', width = 6.77, height = 7.19)


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
   

# Calculating generation per capita


generation_percapita<-generation_rc_aggregated %>% # generation_rc_aggregated_fnl
                        left_join(estpoprc_aggregated,by=c("Area","Year"),relationship="many-to-many")  %>%
                        mutate(gnrt_rc_percapita=(gnrt_rc_tot/pop_rc_tot)*1e6) #gnrt_rc_tot_fnl
    

# Getting data ready to map

generation_percapita_map<- rc_polygons %>% left_join(generation_percapita, by = "Area") %>% arrange("Area") %>% 
                            filter(Year==2022) %>% 
                            mutate(group=case_when(Area=="Southland"~"Southland",
                                                   TRUE ~ "Excluding Southland"))


# Quantile of the generation per capita

quantile(generation_percapita_map$gnrt_rc_percapita, seq(from = 0.25, to = 1, length.out = 5))


# Mapping electricity generation per capita data for 2022

generation_percapita_map %>% filter(Area=="Southland") %>% sf::st_drop_geometry() %>% dplyr::select(gnrt_rc_percapita)

map_rc_generation_percapita<-ggplot() + 
           geom_sf(data=nz_all,fill="white") +
           geom_sf(data=generation_percapita_map %>% filter(!Area %in% c("West Coast","Tasman")),aes(fill = gnrt_rc_percapita)) + 
           geom_sf(data=generation_percapita_map %>% filter(Area %in% c("West Coast","Tasman")),fill="black") +
           scale_fill_continuous(labels=scales::comma, name = 'KWh per capita',high = "#00994C", low = "#CCFFE5") +
           facet_wrap(~ Year) +
           scale_x_continuous(breaks = seq(170, 184, by = 3)) +
           scale_y_continuous(labels=scales::comma) +
           xlab(expression(paste("Longitude (", degree,"E)"))) +
           ylab(expression(paste("Latitude (", degree,"S)"))) +
           labs(title="Per capita Energy Generation",
                subtitle="By Regional Council - All regions included",
                caption="Source: Markets \n
                                 Tasman and West Coast only power local lines networks (not connected to the grid)")

map_rc_generation_percapita

ggplot2::ggsave(map_rc_generation_percapita, file = 'analysis/20230919_electricity_data_module/outputs/RC_percapita_energygeneration.jpg', width = 6.77, height = 7.19)








