## Replication data for "Limiting aggressive policing can reduce police and 
## civilian violence"
## 
## This file: 
## 1) Cleans and links the fogo cruzado data to the police precinct shapefiles 
## 2) Reproduces the graphics shown in Figure 1
## 
library(stargazer)
library(ggplot2)
library(readr)
library(sf)
library(ggmap)
library(tidyverse)
library(lubridate)
library(gridExtra)

load("fogo_rd.RData")

#### LINK FOGO CRUZADO DATA TO GEOLOCATED POLICE PRECINCTS ####

delegacias <- st_read("shape limites seg/lm_dp_2019.shp")
## Subset them by police presence or not 
fogo_dp_cops <- subset(fogo_dp_cops, data_ocorrencia < as.Date("2020-01-01"))
fogo_dp_nocops <- subset(fogo_dp_nocops, data_ocorrencia < as.Date("2020-01-01"))
fogocops <- fogo_dp_cops %>%
  group_by(dp) %>%
  summarise(n_cops = sum(n),
            n_deaths_cops = sum(n_deaths))
fogonocops <- fogo_dp_nocops %>%
  group_by(dp) %>%
  summarise(n_nocops = sum(n),
            n_deaths_nocops = sum(n_deaths))

delegacias_full <- left_join(delegacias, fogocops, by="dp")
delegacias_full <- left_join(delegacias_full, fogonocops, by = "dp")
delegacias_full <- na.omit(delegacias_full)

#Make plots with google maps API
register_google(key = "[INSERT YOUR OWN GOOGLE MAPS API KEY HERE]")
rio_basemap <- get_map(location=c(left = -44, right = -42.3, top = -22.25, bottom = - 23.2),
                       maptype = 'terrain-lines', source = 'stamen')

#### MAP MAKING ####

## "Shootings in 2019 involving a police officer (Total = 2247)"
m1 <- ggmap(rio_basemap) + geom_sf(data = delegacias_full, aes(fill = n_cops), inherit.aes = F) +
  theme_bw() + scale_fill_gradient2(low = "white", mid = "orange", high = "red", limits = c(0, 520), midpoint = 90, name = "Shootings") +
  labs(x = NULL, y = NULL, title = NULL) + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),
                                                 legend.position = "bottom")
## Shootings not involving a police officer (n = 5,121)
m2 <- ggmap(rio_basemap) + geom_sf(data = delegacias_full, aes(fill = n_nocops), inherit.aes = F) +
  theme_bw() + scale_fill_gradient2(low = "white", mid = "orange", high = "red", limits = c(0, 520), midpoint = 90, name = "Shootings") +
  labs(x = NULL, y = NULL,  title = NULL) + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),
                                                  legend.position = "bottom")
## title = "Deaths resulting from shootings involving a police officer (Total = 908)") 
m3 <- ggmap(rio_basemap) + geom_sf(data = delegacias_full, aes(fill = n_deaths_cops), inherit.aes = F) +
  theme_bw()  + scale_fill_gradient2(low = "white", mid = "orange", high = "red", limits = c(0, 66), midpoint = 30, name = "Deaths") +
  labs(x = NULL, y = NULL, title = NULL) + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),
                                                 legend.position = "bottom")
## title = "Deaths resulting from shootings not involving a police officer (Total = 540)")
m4 <- ggmap(rio_basemap) + geom_sf(data = delegacias_full, aes(fill = n_deaths_nocops), inherit.aes = F) + theme_bw()  +
  scale_fill_gradient2(low = "white", mid = "orange", high = "red", limits = c(0, 66), midpoint = 30, name = "Deaths") +
  labs(x = NULL, y = NULL, title = NULL) + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),
                                                 legend.position = "bottom")


pdf("m1.pdf")
m1
dev.off()

pdf("m2.pdf")
m2
dev.off()

pdf("m3.pdf")
m3
dev.off()

pdf("m4.pdf")
m4
dev.off()