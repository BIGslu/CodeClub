library(tidytuesdayR)
library(tidyverse)

dat <- tidytuesdayR::tt_load("2022-03-01")$stations

#Washington
wa <- dat %>% filter(STATE == "WA" & LONGITUDE > -150)

#Base map
map_wash <- map_data("state",region = "washington")

ggplot(map_wash, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "white")

#Add stations
ggplot() +
  geom_polygon(data=map_wash,
               aes(x = long, y = lat, group = group),
               fill="lightgray", colour = "white") +
  geom_point(data=wa, aes(x=LONGITUDE, y=LATITUDE,
                          color=FUEL_TYPE_CODE))

#Density
##By color
ggplot() +
  geom_polygon(data=map_wash,
               aes(x = long, y = lat, group = group),
               fill="lightgray", colour = "white") +
  geom_bin2d(data=wa, aes(x=LONGITUDE, y=LATITUDE),
             bins=100) +
  scale_fill_continuous(type="viridis")

##As density
ggplot(wa) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE,
                 color=FUEL_TYPE_CODE), size=0.5) +
  geom_density_2d_filled(aes(x=LONGITUDE, y=LATITUDE),
                         alpha=0.5) +
geom_polygon(data=map_wash,
             aes(x = long, y = lat, group = group),
             fill=NA, colour = "black")

##Mask outside
# outline <- map_wash %>% select(long, lat)
# rec_box <- data.frame(long=c(-125,-125,-117,-117,-125), lat=c(45,49,49,45,45))
# 
# mask <- rbind(rec_box,map_wash)
# 
# ggplot() + 
#   geom_sf(data = usa_sf()) +
#   geom_density_2d_filled(aes(x = LONGITUDE, y = LATITUDE),
#                          data = top_50,
#                          alpha = .5) +
#   xlim(-125,-66.5) +
#   ylim(20, 50) +
#   geom_polygon(data=mask,
#                aes(x=x,y=y),color="white",fill="white") +
#   theme_map() +
#   theme(legend.position = "none")

# Animate
library(gganimate)
wa_year <- wa %>% 
  mutate(YEAR = lubridate::year(OPEN_DATE))
  
aa <- ggplot(wa_year) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE,
                 color=FUEL_TYPE_CODE), size=0.5) +
  # geom_density_2d_filled(aes(x=LONGITUDE, y=LATITUDE),
  #                        alpha=0.5) +
  geom_polygon(data=map_wash,
               aes(x = long, y = lat, group = group),
               fill=NA, colour = "black") +
  transition_states(YEAR) 
#Explore?
  # transition_reveal(YEAR) #+
  # shadow_mark()
 # shadow_wake()

aa
anim_save("wa.gif", aa)
