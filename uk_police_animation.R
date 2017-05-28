### downloading and unzipping Essex Police data

#devtools::install_github("dkahle/ggmap")
#devtools::install_github("hadley/ggplot2")
#install.packages("ggplot2")
#install.packages("ggmap", type = "source")

library(dplyr)
library(lubridate)
library(stringr)
library(ggmap)
library(ggplot2)




#files <- dir(recursive = T, pattern = "*.csv", full.names=TRUE) # searching for ALL csv files
london_files <- dir(recursive = T, pattern = "*metropolitan-street.csv", full.names=TRUE)

london_police_data <- do.call(rbind,lapply(london_files, read.csv))
str(london_police_data)

sort(table(london_police_data$Crime.type))

weapon_map_data <- london_police_data %>%
  filter(Crime.type == "Possession of weapons") %>% 
  #na.omit() %>%
  select(Month, Longitude, Latitude, Crime.type)

summary(weapon_map_data)
weapon_map_data <- weapon_map_data[complete.cases(weapon_map_data),]


### creating a map ####

#### using averaged coordinates for centering the map - not used in this particular example

#london_lon = c(min(map_data$Longitude, na.rm = TRUE), max(map_data$Longitude, na.rm = TRUE)) 
#london_lat = c(min(map_data$Latitude, na.rm = TRUE), max(map_data$Latitude, na.rm = TRUE)) 


### creating London map
london_map = get_map(location = "London",
                       maptype="toner",  zoom = 10)

ggmap(london_map)


#### plotting points / creating heatmap ####

#### weapon possession
## heat map

weapon_london_heat_map<- ggmap(london_map, extent = "device") +
  stat_density_2d(aes(x = Longitude, y = Latitude, fill = ..level.., alpha=1),
                  data=weapon_map_data, geom = "polygon") +
  scale_fill_gradient(low = "blue", high = "red") +
  scale_alpha(range = c(0.00, 0.5), guide = FALSE) 
  #scale_x_continuous(limits = essex_lon) +
  #scale_y_continuous(limits = essex_lat) 

weapon_london_heat_map


#### weapon possession
## heat map

str(london_police_data)

jan2016_map_data <- london_police_data %>% 
  filter(Month == '2016-01') %>% 
  select(Month, Longitude, Latitude, Crime.type)

jan2016_london_heat_map<- ggmap(london_map, extent = "device") +
  stat_density_2d(aes(x = Longitude, y = Latitude, fill = ..level.., alpha=1),
                  data=jan2016_map_data, geom = "polygon") +
  scale_fill_gradient(low = "blue", high = "red") +
  scale_alpha(range = c(0.00, 0.5), guide = FALSE) +
  facet_wrap(~ Crime.type, nrow = 3) 
#scale_x_continuous(limits = essex_lon) +
#scale_y_continuous(limits = essex_lat) 

jan2016_london_heat_map




#### animated maps - gganimate alone ####
devtools::install_github("dgrtwo/gganimate")

library(gganimate)


### weapon crimes only
map_anime<- ggmap(london_map, extent = "device") +
  stat_density_2d(aes(x = Longitude, y = Latitude, frame = Month, 
                      fill = ..level.., alpha=1),
                  data=weapon_map_data, geom = "polygon") +
  scale_fill_gradient(low = "blue", high = "red") +
  scale_alpha(range = c(0.00, 0.5), guide = FALSE) 

gganimate(map_anime, "weapons_london.gif")




#### all crimes 

all_map_data <- london_police_data %>% 
  select(Month, Longitude, Latitude, Crime.type)

all_london_heat_map<- ggmap(london_map, extent = "device") +
  stat_density_2d(aes(x = Longitude, y = Latitude, frame = Month, 
                      fill = ..level.., alpha=1),
                  data=all_map_data, geom = "polygon") +
  scale_fill_gradient(low = "blue", high = "red") +
  scale_alpha(range = c(0.00, 0.5), guide = FALSE) +
  facet_wrap(~ Crime.type, nrow = 3) 
#scale_x_continuous(limits = essex_lon) +
#scale_y_continuous(limits = essex_lat) 

gganimate(all_london_heat_map, "all_london.gif")





#### map animations = tweenr pkg ####
#devtools::install_github("thomasp85/tweenr")
library(tweenr)
library(tidyr)

str(map_data)
summary(map_data)
table(map_data$Month)

is.na(map_data) %>% table()
map_data <- droplevels(map_data)

london_maps_n <- map_data %>%
  group_by(Month) %>%
  nest() 

str(london_maps_n$data)
summary(london_maps_n$data)

### error: cannot send dataframes of varying lengths to tween_states()
tween_london_maps <- tween_states(london_maps_n$data, tweenlength = 1,
                                  statelength = 0.5, ease = "sine-out", nframe = 200) %>%
  as.tibble()













































































































































































