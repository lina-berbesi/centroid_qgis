

suppressPackageStartupMessages(library("tidyverse"))
suppressPackageStartupMessages(library("tidygeocoder"))
suppressPackageStartupMessages(library("dplyr"))


# Tidy geocoder api reference

tidygeocoder::api_info_reference


# Reading initial file provided by the Markets team

stations_metadata_locations_markets<-read.csv("analysis/20230919_electricity_data_module/substations_location/stations_location_metadata.csv") 

# e.g. duplicated  substation in the original data example: stations_metadata_locations_markets %>% filter(NSP=="NUL0011NZALEN")

stations_metadata_locations_tmp<- stations_metadata_locations_markets %>% 
   select(NSP,"POC.code","Network.participant","NZTM.easting","NZTM.northing","Description") %>%
   filter(!duplicated(NSP))

nrow(stations_metadata_locations_markets)
nrow(stations_metadata_locations_tmp)

# Subset metadata stations

stations_metadata_locations<-stations_metadata_locations_tmp %>% filter (!(is.na(NZTM.easting) | is.na(NZTM.northing)))

# Find duplicated in the stations metadata location

stations_metadata_locations %>% 
   filter(duplicated(paste0('NZTM.easting','NZTM.northing')) | duplicated(paste0('NZTM.easting','NZTM.northing'),fromLast=TRUE)) %>% 
   summarize(duplicated_stations_location=n())


# Geocode stations with missing easting and northing

stations_metadata_locations_missing_tmp<- stations_metadata_locations_tmp %>% 
   filter(is.na(NZTM.easting) | is.na(NZTM.northing))  %>% 
   select(-c("NZTM.easting","NZTM.northing"))  %>%
   tidygeocoder::geocode(address=Description,method="osm")

# Tidy geocoder doesn't work with Building Names It has to have a street number 

stations_metadata_locations_missing<- stations_metadata_locations_missing_tmp %>%
                                      filter(!(is.na(lat)|is.na(long))) %>% 
                                      mutate(row=row_number())

cord_deck1 <- sp::SpatialPoints(cbind(stations_metadata_locations_missing$long, stations_metadata_locations_missing$lat), 
                           proj4string = CRS("+proj=longlat"))

cord_UTM1 <- sp::spTransform(cord_deck1, CRS("+init=epsg:2193")) #https://epsg.io/?q=New%20Zealand
# It seems the original file easting and northing are in NZGD2000 / New Zealand Transverse Mercator 2000 - New Zealand Transverse Mercator (NZTM)

cord_missing<-as.data.frame(cord_UTM1)  %>% mutate(row=row_number())
colnames(cord_missing)<-c("NZTM.easting.geocoded","NZTM.northing.geocoded","row")


stations_metadata_locations_geocoded<-stations_metadata_locations_missing %>% 
                                         left_join(cord_missing,by="row")

# Reading manual file with the ones could not be geocoded

stations_metadata_locations_manual<-read.xlsx2("analysis/20230919_electricity_data_module/substations_location/stations_location_metadata_manual.xlsx",sheetIndex=1) %>%
                                    mutate(row=row_number(),
                                           lat=as.numeric(lat),
                                           long=as.numeric(long))

cord_deck2 <- sp::SpatialPoints(cbind(stations_metadata_locations_manual$long, stations_metadata_locations_manual$lat), 
                               proj4string = CRS("+proj=longlat"))

cord_UTM2 <- sp::spTransform(cord_deck2, CRS("+init=epsg:2193")) 

cord_manual<-as.data.frame(cord_UTM2)  %>% mutate(row=row_number())
colnames(cord_manual)<-c("NZTM.easting.manual","NZTM.northing.manual","row")

stations_metadata_locations_manual<-stations_metadata_locations_manual %>% 
                                      left_join(cord_manual,by="row")


# Binding the missing geocoding stations with the geocoded stations

stations_metadata_locations_write<-stations_metadata_locations_tmp %>%
                                 left_join(stations_metadata_locations_geocoded[,c("NSP","NZTM.easting.geocoded","NZTM.northing.geocoded")],by="NSP") %>%
                                 left_join(stations_metadata_locations_manual[,c("NSP","NZTM.easting.manual","NZTM.northing.manual")],by="NSP") %>%
                                 mutate(`NZTM.easting.final`=case_when(!is.na(`NZTM.easting.manual`) ~ `NZTM.easting.manual`,
                                                                       !is.na(`NZTM.easting.geocoded`) & is.na(`NZTM.easting.manual`) ~ `NZTM.easting.geocoded`,
                                                                       TRUE ~ `NZTM.easting`),
                                        `NZTM.northing.final`=case_when(!is.na(`NZTM.northing.manual`) ~ `NZTM.northing.manual`,
                                                                        !is.na(`NZTM.northing.geocoded`) & is.na(`NZTM.northing.manual`) ~ `NZTM.northing.geocoded`,
                                                                        TRUE ~ `NZTM.northing`))

nrow(stations_metadata_locations_write)
View(stations_metadata_locations_write)

write.csv(stations_metadata_locations_write,"analysis/20230919_electricity_data_module/substations_location/stations_location_metadata_geocoded.csv",row.names=FALSE)





# Checking % of consumption coming from no UTM stations

consumption_rds<-readRDS("analysis/20230919_electricity_data_module/markets_data/monthly_gxp_demand.rds") %>%
   mutate(Year=as.numeric(format(TimePeriod, "%Y")))

stations_metadata_locations_fnl<-stations_metadata_locations_markets %>%
   mutate(flag=ifelse(is.na(`NZTM.easting`),"noUTM","UTM"))

test_empty_substations<-consumption_rds %>% 
   left_join(stations_metadata_locations_fnl,by=c("POC"="POC.code"),relationship="many-to-many")


test_empty_substations %>% filter(is.na(flag)) %>% select(POC) %>% unique()
test_empty_substations %>% filter(is.na(flag)) %>% select(Year) %>% unique()
test_empty_substations %>% filter(is.na(flag)) %>% summarize(demand=sum(Value))

test_empty_substations %>% filter(flag=="noUTM") %>% select(POC) %>% unique()
test_empty_substations %>% filter(flag=="noUTM") %>% select(Year) %>% unique()
test_empty_substations %>% filter(flag=="noUTM") %>% summarize(demand=sum(Value))

consumption_rds %>% filter(Year>=2020 & Year<=2023) %>% group_by(Year) %>% summarize(demand=sum(Value))



