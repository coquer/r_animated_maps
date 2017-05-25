### downloading and unzipping Essex Police data
#devtools::install_github("dkahle/ggmap")
devtools::install_github("hadley/ggplot2")

install.packages("ggplot2")
install.packages("ggmap", type = "source")

library(dplyr)
library(lubridate)
library(stringr)
library(ggmap)
library(ggplot2)

??ggproto

#files <- list.files(recursive = T, pattern = "*.csv")
files <- dir(recursive = T, pattern = "*.csv", full.names=TRUE)
london_files <- dir(recursive = T, pattern = "*metropolitan-street.csv", full.names=TRUE)

london_police_data <- do.call(rbind,lapply(london_files, read.csv))
str(london_police_data)

sort(table(london_police_data$Crime.type))

map_data <- london_police_data %>%
  filter(Crime.type == "Possession of weapons") %>% 
  #na.omit() %>%
  select(Month, Longitude, Latitude, Crime.type)

summary(map_data)

map_data[is.na(map_data), ] <- NULL

### creating a map ####
london_lon = c(min(map_data$Longitude, na.rm = TRUE), max(map_data$Longitude, na.rm = TRUE)) 
london_lat = c(min(map_data$Latitude, na.rm = TRUE), max(map_data$Latitude, na.rm = TRUE)) 

?get_map

london_map = get_map(location = "London",
                       maptype="toner",  zoom = 10)

ggmap(london_map)


#### plotting points / creating heatmap ####

#### weapon possession
??panel.spacing

london_heat_map<- ggmap(london_map, extent = "device") +
  stat_density_2d(aes(x = Longitude, y = Latitude, fill = ..level.., alpha=1),
                  data=map_data, geom = "polygon") +
  scale_fill_gradient(low = "blue", high = "red") +
  scale_alpha(range = c(0.00, 0.5), guide = FALSE) 
  #scale_x_continuous(limits = essex_lon) +
  #scale_y_continuous(limits = essex_lat) 

london_heat_map


#### animated maps 

devtools::install_github("thomasp85/tweenr")
library(tweenr)

weapons_1 <- map_data %>% filter(Month == '2016-01')
weapons_2 <- map_data %>% filter(Month == '2016-02')
weapons_3 <- map_data %>% filter(Month == '2016-03')
weapons_4 <- map_data %>% filter(Month == '2016-04')
weapons_5 <- map_data %>% filter(Month == '2016-05')
weapons_6 <- map_data %>% filter(Month == '2016-06')
weapons_7<- map_data %>% filter(Month == '2016-07')
weapons_8 <- map_data %>% filter(Month == '2016-08')
weapons_9 <- map_data %>% filter(Month == '2016-09')
weapons_10 <- map_data %>% filter(Month == '2016-10')
weapons_11 <- map_data %>% filter(Month == '2016-11')
weapons_12 <- map_data %>% filter(Month == '2016-12')
weapons_13 <- map_data %>% filter(Month == '2017-01')
weapons_14 <- map_data %>% filter(Month == '2017-02')
weapons_15 <- map_data %>% filter(Month == '2017-03')

?tween_states

data <- tween_states(list(paste0("weapons_",1:15)), 3, 1, 'cubic-in-out', 100)


##### example 
library(tweenr)

gapminder_edit <- gapminder %>%
  arrange(country, year) %>%
  select(gdpPercap,lifeExp,year,country, continent, pop) %>%
  rename(x=gdpPercap,y=lifeExp,time=year,id=country) %>%
  mutate(ease="linear")

gapminder_tween <- tween_elements(gapminder_edit,
                                  "time", "id", "ease", nframes = 300) %>%
  mutate(year = round(time), country = .group) %>%
  left_join(gapminder, by=c("country","year","continent")) %>%
  rename(population = pop.x)

p2 <- ggplot(gapminder_tween,
             aes(x=x, y=y, frame = .frame)) +
  geom_point(aes(size=population, color=continent),alpha=0.8) +
  xlab("GDP per capita") +
  ylab("Life expectancy at birth") +
  scale_x_log10(labels=comma)

gganimate(p2, filename="gapminder-tween.gif", title_frame = FALSE, interval = 0.05)
For another nice example using the tweenr package, check out this blog post by Jake 






























































































































































