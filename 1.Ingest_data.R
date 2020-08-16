library('tidyverse')
library('googleway')

new <-read_csv('./input_data/Street_Lighting__sample_data_.csv')


map_key <- "SECRET_KEY"


get_bus_number <- function(lat,lon) {
  length(google_places(location = c(lat,lon),
  radius = 200,
  place_type="transit_station",
  key = map_key)[["results"]][["types"]])
}

get_tourist_number <- function(lat,lon) {
  length(google_places(location = c(lat,lon),
                       radius = 200,
                       place_type="tourist_attraction",
                       key = map_key)[["results"]][["types"]])
}


get_park_number <- function(lat,lon) {
  length(google_places(location = c(lat,lon),
                       radius = 200,
                       place_type="park",
                       key = map_key)[["results"]][["types"]])
}

buses_parks_tourists <- new %>% 
  mutate(bus_stops_nearby = get_bus_number(Geolocation_Latitude,Geolocation_Longitude),
         park_nearby = get_park_number(Geolocation_Latitude,Geolocation_Longitude),
         tourist_nearby = get_tourist_number(Geolocation_Latitude,Geolocation_Longitude),
         )

write.csv(buses_parks_tourists,'./output_data/lights_gmaps.csv')


#Clean data

final <- buses_parks_tourists %>% filter(!grepl("CAMERA", Other_Asset, fixed = TRUE),
                                         No_Lights_on_Pole > 0) %>% 
                                  mutate(wattage = sub(" *\\(.*", "", Light_Power_Rating),
                                         wattage = str_replace(wattage,'HPS',''),
                                         wattage = as.numeric(replace_na(wattage,250))
                                         )


write.csv(final,'./output_data/combined_data.csv')

