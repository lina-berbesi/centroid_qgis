
# https://www.transpower.co.nz/hawkes-bay-and-gisborne-power-outage
# https://www.transpower.co.nz/news/transpower-and-powernet-seek-input-southlands-future-electricity-needs

.libPaths(c("P:/R/libraries/4.0.5-20230724", "P:/R/libraries/4.0.5-20230724-extended"))

suppressPackageStartupMessages(library("sf"))
suppressPackageStartupMessages(library("dplyr"))
suppressPackageStartupMessages(library("spgwr"))
suppressPackageStartupMessages(library("proj4"))
suppressPackageStartupMessages(library("ggplot2"))
suppressPackageStartupMessages(library("ggspatial",warn.conflicts=FALSE))
# Other useful packages not available: cartography for map display and spatstat for pts density analysis


# Load stations metadata location

proj4 <- "+proj=tmerc +lat_0=0 +lon_0=173 +k=0.9996 +x_0=1600000 +y_0=10000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs "

stations_metadata_locations_tmp<-read.csv("analysis/20230919_electricity_data_module/substations_location/stations_location_metadata.csv") # stations_location_metadata_geocoded.csv

stations_metadata_locations<-stations_metadata_locations_tmp %>% 
                            select(NSP,"POC.code","Network.participant","Description","NZTM.easting","NZTM.northing","I.flow") %>% # Change to include or exlude geolocation .final
                            mutate(row=row_number(),
                                   category=case_when(`I.flow`==1~"In Point",
                                                      `I.flow`==0~"Out Point"))

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


# Adding centroids of the regions for the pie chart

nz_simple_poly <- sf::st_simplify(sf::st_geometry(rc_polygons), dTolerance = 10000) %>%
                  sf::st_sfc() %>%
                  sf::st_cast("POLYGON")

southland <- rc_polygons[rc_polygons$Area == "Southland", ]

southland_cent<- sf::st_centroid(sf::st_simplify(southland))

# Merge multipolygons in one before calculating centroids


rc_polygons_fnl<-rc_polygons %>%  group_by(REGC2023_V) %>%
                   summarise(geometry = sf::st_union(geometry)) %>%
                   ungroup()

centroids <- rc_polygons_fnl %>% sf::st_drop_geometry() %>%
             mutate(centroid=sf::st_point_on_surface(sf::st_simplify(rc_polygons_fnl$geometry,dTolerance=1000)),
                    easting=sf::st_coordinates(centroid)[,2],
                    northing=sf::st_coordinates(centroid)[,1]) %>%
             dplyr::select('easting','northing') 

rc_centroids<- as.data.frame(proj4::project(centroids, proj4, inverse = TRUE))
colnames(rc_centroids)<-c("longitude","latitude")

rc_polygons$longitude<-rc_centroids$longitude
rc_polygons$latitude<-rc_centroids$latitude


ggplot() + geom_sf(data = nz_all) +
   geom_point(data=rc_polygons %>% filter(!Area %in% c("Northland","Southland","Marlborough")),aes(longitude, latitude),color="red")


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


# Plotting substations


nz_all<-sf::st_union(rc_polygons)

map_substations <- ggplot() + geom_sf(data = nz_all) +
   geom_sf(data = pts, shape=16, aes(color=category)) + 
   scale_color_manual(values=c("#008000","#0000CD")) +
   xlab(expression(paste("Longitude (", degree, "E)"))) +
   ylab(expression(paste("Latitude (", degree, "S)"))) +
   scale_x_continuous(breaks = seq(170, 184, by = 2)) +
   labs(title = "Substations location",
        subtitle = "EA",
        caption = "Source: Markets") 

map_substations

ggplot2::ggsave(map_substations, file = 'analysis/20230919_electricity_data_module/outputs/substations_location.jpg', width = 6.77, height = 7.19)


# Reading generation data file from the Markets team

generation_rds<-readRDS("analysis/20230919_electricity_data_module/markets_data/gen_data.rds") %>%
                  mutate(Year=as.numeric(format(Date, "%Y"))) %>%
                  dplyr::select(-c("Unit"))


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


generation_rc_aggregated<-generation_rc_allocated %>% group_by(Year,Area,Fuel_Code) %>% 
                            summarize(gnrt_rc_tot=sum(Value,na.rm=TRUE)/1e6) %>%
                            filter(!is.na(Area)) %>%
                            arrange(Year)


generation_rc_aggregated_total<-generation_rc_allocated %>% group_by(Year,Area) %>% 
                                summarize(gnrt_rc_year=sum(Value,na.rm=TRUE)/1e6) %>%
                                arrange(Year)


generation_rc_aggregated_fnl<- generation_rc_aggregated %>% left_join(generation_rc_aggregated_total,by=c("Year","Area"),relationship="many-to-many") %>%
                                                            mutate(gnrt_rc_pcnt=round((gnrt_rc_tot/gnrt_rc_year)*100,digits=2))


# Getting data ready to map

generation_rc_map<-rc_polygons %>% left_join(generation_rc_aggregated_fnl, by = "Area") %>% arrange("Area") %>%  
   filter(Year==2022) 


# Quantile of the generation

quantile(generation_rc_map$gnrt_rc_tot, seq(from = 0.25, to = 1, length.out = 5)) 


# Mapping electricity generation by Fuel Code Year 2022

map_rc_generation<-ggplot() + 
   geom_sf(data=nz_all,fill="white") + 
   geom_sf(data=generation_rc_map,aes(fill = gnrt_rc_pcnt)) + 
   facet_wrap(~ Fuel_Code) + 
   scale_fill_continuous(high="#800080", low="#D8BFD8",name="Gwh") +
   scale_x_continuous(breaks = seq(170, 184, by = 3)) +
   scale_y_continuous(labels=scales::comma) +
   xlab(expression(paste("Longitude (", degree,"E)"))) +
   ylab(expression(paste("Latitude (", degree,"S)"))) +
   labs(title="Energy Generation by Fuel Type ",
        subtitle="Regional Councils",
        caption="Source: Markets \n
                         Tasman and West Coast only power local lines networks (not connected to the grid)")

map_rc_generation

ggplot2::ggsave(map_rc_generation, file = 'analysis/20230919_electricity_data_module/outputs/RC_total_energygeneration_bysource.jpg', width = 6.77, height = 7.19)


# Mapping electricity generation by Fuel Code Year 2022


geom_sf(data=nz_all,fill="white") + 
   geom_sf(data=generation_rc_map,aes(fill = gnrt_rc_pcnt)) + 
   facet_wrap(~ Fuel_Code) 







