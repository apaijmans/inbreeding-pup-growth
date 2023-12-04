# -----------------------------------------------------------
# Author: A.J. Paijmans with help of Ane Liv Berthelsen and
# Rebecca Nagel for the code to make the map of Bird Island
#
# Script Name: 5_seasonal_figures
#
# Purpose: This script is used to create figures showing
# seasonal variation in number of female breeders, female pup birth mass
# and foraging trip duration, as well as a figure with the map of
# South Georgia and Bird Island.
#
# Date: 2023-12-01
# -----------------------------------------------------------


library(here)
library(readxl)
library(tidyverse)
library(cowplot)

# The following libraries are necessary for making the map
library(sf)
library(chron)
library(ggspatial)
library(ggsn)



## ---- fig_map_seasons --------

#~~~~~~~~~~~~~~~~#
#  Load data  ####
#~~~~~~~~~~~~~~~~#

seasonal_data <- read.table(here("Data", "Raw", "seasonal_data.txt"), sep = "\t", stringsAsFactors = F, header = T)



#~~~~~~~~~~~~~~~~~~~~#
#  Seasonal data  ####
#~~~~~~~~~~~~~~~~~~~~#

source(here("Rcode", "anneke_theme.R"))

#~~ Make a list for the theme so it is the same for all figures
gglayer_theme <- list(
  scale_x_discrete(labels = c(`2017-2018` = "2018", `2018-2019` = "2019", `2019-2020` = "2020", `2020-2021` = "2021")),
  theme_anneke(),
  theme(axis.line.x = element_line(colour = 'black', linetype='solid'),
        axis.line.y = element_line(colour = 'black', linetype='solid'),
        plot.title = element_text(size = rel(1)))
)

#~~ Make sub plots
# Plot a is blank canvas + title where later the map gets added
p_a <- ggplot(seasonal_data %>% filter(variable=="SSB ESTIMATED NUMBER OF FEMALE BREEDERS"), 
              aes(x = season, y = mean)) + 
  #geom_pointrange(aes(ymin = CI95_low, ymax = CI95_high)) +
  #geom_point(shape = 22, size = 4, fill = "#eb7f86") +
  labs(title="(a) Map of Bird Island", x= "", y="") +
  gglayer_theme +
  theme(axis.line.x = element_blank(),
        axis.line.y = element_line(colour = 'white', linetype='solid'), 
        axis.text = element_text(colour = "white"),
        panel.grid=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

# Breeding females
p_breeders <-  ggplot(seasonal_data%>% filter(variable=="SSB ESTIMATED NUMBER OF FEMALE BREEDERS"), 
                      aes(x = season, y = mean)) + 
  geom_pointrange(aes(ymin = CI95_low, ymax = CI95_high)) +
  geom_point(shape = 22, size = 4, fill = "#ea4f88") +
  labs(title="(b) Female breeders", x= "Year", y="No. of breeders") +
  gglayer_theme

# Female pup birth mass
p_bm <-  ggplot(seasonal_data%>% filter(variable=="SSB FEMALE PUP BIRTH MASS (kg)"), 
                aes(x = season, y = mean)) + 
  geom_pointrange(aes(ymin = CI95_low, ymax = CI95_high)) +
  geom_point(shape = 22, size = 4, fill = "#4b2991") +
  labs(title="(c) Female pup birth mass", x= "Year", y="Birth mass (kg)") +
  gglayer_theme

# Female foraging trip duration
p_foraging <-  ggplot(seasonal_data %>% filter(variable=="FWB FEMALE FORAGING TRIP DURATION (days)"), 
                      aes(x = season, y = mean)) + 
  geom_pointrange(aes(ymin = CI95_low, ymax = CI95_high)) +
  geom_point(shape = 22, size = 4, fill = "#f6a97a") +
  labs(title="(d) Female foraging trip duration", x="Year", y="Time at sea (days)") +
  gglayer_theme



#~~~~~~~~~~~~~~~~~~~~~~#
#  Bird Island map  ####
#~~~~~~~~~~~~~~~~~~~~~~#

#~~ Bird island maps
bi_coast <- st_read(here("Rcode", "Bird Island Map", "Map_Old", "BI_Coast_Projected_new.shp"), quiet = TRUE)
# bi_r <- st_read(here("Rcode", "Bird Island Map", "rivers_lines", "sg_bird_rivers.shp"), quiet = TRUE) #rivers
# bi_c <- st_read(here("Rcode", "Bird Island Map", "contours", "sg_bird_contours.shp"), quiet = TRUE) # contours
# bi <- st_read(here("Rcode", "Bird Island Map", "coastline", "sg_bird_coast.shp"), quiet = TRUE) #surface

#~~ Outline of SSB and FWB, made in Google Earth
ssb <- st_read(here("Rcode", "Bird Island Map", "beachs", "SSB.kml", "doc.kml"), quiet = TRUE)
fwb <- st_read(here("Rcode", "Bird Island Map", "beachs", "FWB.kml", "doc.kml"), quiet = TRUE)


#### mapping bird island ----
plot.bi.color <- ggplot() + 
  geom_sf(data = bi_coast, fill = "#ADADAD") + 
  #geom_sf(data = bi_r, color = "blue") + 
  #geom_sf(data = bi_c) + 
  geom_sf(data=ssb, fill = "#872ca2") + #ssb
  geom_sf(data=fwb, fill = "#fa7876") + #fwb
  theme(legend.position="none") +
  theme_void()

#### mapping study colonies ----
# Adds box around study colonies on bird island map
plot.bi.color. <- plot.bi.color +
  annotate(geom = "rect", 
           xmin = -38.060, 
           xmax = -38.045, 
           ymin = -54.014, 
           ymax = -54.0065, 
           fill = NA, # transparent bg
           color = "black" ) 

# Adds beach location in color
plot.bi.beaches.color <- ggplot() + 
  geom_sf(data = bi_coast, fill = "NA") + 
  geom_sf(data = bi_coast, fill = "#D4CEC2") +  # "#eaeaea"
  geom_sf(data = fwb, fill = "#fa7876") + 
  geom_sf(data = ssb, fill = "#872ca2") +
  theme(legend.position = "none") +
  theme_void()

# Add text
plot.bi.beaches.color <- plot.bi.beaches.color + 
  coord_sf(xlim = c(-38.060, -38.045),
           ylim = c(-54.014, -54.0065),
           expand = FALSE) +
  annotation_scale(aes(location="br", style = "ticks")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=1)) +
  annotate(geom = "text",
           x = -38.05,
           y = -54.0092,
           label = "FWB", 
           color = "#fa7876",
           fontface = "bold")  +
  annotate(geom = "text",
           x = -38.05,
           y = -54.011,
           label = "SSB", 
           color = "#872ca2",
           fontface = "bold")  #+

# Combine
map <- ggdraw(p_a) + # empty canvas with title to match other plots
  draw_plot(plot.bi.beaches.color, x= 0.07, scale = .8) + # study colonies
  draw_plot(plot.bi.color., 0.07, .48, .5, .5, scale = 1.3) # Bird Iland

#map



#~~~~~~~~~~~~~~~~~#
#  Final plot  ####
#~~~~~~~~~~~~~~~~~#

P_seasonal <- plot_grid(map, p_breeders, p_bm, p_foraging)

P_seasonal

##---- chunk_end

ggsave("Figs/F1_seasonal_data_map.jpg", P_seasonal, width = 7, height = 6)


