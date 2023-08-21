library(tidyverse)
library(rnaturalearthdata)
library(sf)

world_sf <- countries110 %>% 
  st_as_sf()

ggplot() +
  geom_sf(data = world_sf,
          size = 0.1)

ggplot() +
  geom_sf(data = world_sf,
          fill = "green",
          size = 0.4)