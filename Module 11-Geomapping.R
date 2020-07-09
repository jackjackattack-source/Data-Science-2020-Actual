#clear environment
rm(list=ls(all=TRUE))
# install these normally if you don't have them
library(rio)
library(tidyverse)
library(googlesheets4)
library(labelled)
library(data.table)
library(varhandle)
library(ggrepel)
library(geosphere)
library(rgeos)
library(viridis)
library(mapview)
library(rnaturalearth)
library(rnaturalearthdata)
library(devtools)
library(remotes)
library(raster)
library(sp)
library(sf)
library(Imap)
# need to install via github with devtoolds and remotes
library(rnaturalearthhires)# devtools::install_github("ropensci/rnaturalearthhires")
library(ggsflabel)# devtools::install_github("yutannihilation/ggsflabel")
#read in file
setwd("C:/Users/Jack/Documents/Data Science 2020")
world_borders <-st_read("C:/Users/Jack/Documents/Data Science 2020/World_Borders.shp")
#ensure its in WGS34 Format
borders <-st_transform(world_borders, "+proj=longlat +ellps=WGS84 +datum=WGS84")
rm(world_borders)
#import dta file
orig_nr = import("C:/Users/Jack/Documents/Data Science 2020/v11almost_newer_natural_resources.dta")
#big file
dim(orig_nr)
# create fake variable to summarize over
orig_nr$lat_long = orig_nr$latitude+orig_nr$longitude
# collapse
nr = orig_nr %>%
  group_by(country, resource, latitude, longitude, continent, region_wb) %>%
    summarize(lat_long =mean(lat_long, na.rm=TRUE))%>%
  dplyr::select(-c(lat_long))# drop the fake variable
#distinct function does the same thing
nr <- orig_nr%>%
  distinct(country, resource, latitude, longitude, continent, region_wb)
#transform to sf format
# let's drop the NAs just in case
nr <-na.omit(nr, select=c("latitude","longitude","resource", "country","continent", "region_wb"))
#convert to sf format
nr_sf <-st_as_sf(nr, coords =c("longitude", "latitude"),
                 crs = 4326,agr = "constant")#these two don't change for WGS84 format
head(nr_sf)#lat_long now is geometry !!! ALWAYS DO LONG FIRST-ASSOCIATED WITH X var
#rename resource
setnames(nr_sf, "resource", "Resource")
#change all the resources other than the ones we want to other-this will take it out of sf format
final =transform(nr_sf,Resource =factor(replace(as.character(Resource),
                                                list =!Resource%in% c("oil","gold", "diamond"),values = "other")))
#make it sf again
final =st_sf(final)
# getting world map from natural earth packages
world <-ne_countries(scale = "large", returnclass = "sf")
# World map (basic)
world_basic =ggplot()+
  geom_sf(data = world)+
  geom_sf(data = final)
print(world_basic)
#save the basic world map
ggsave(world_basic, filename = "world_map.png", width = 6.5, height = 6)
# World map (with legend, theme change, and shapes for resources)
world_all =ggplot()+
  geom_sf(data = world)+
  geom_sf(data = final,aes(shape=Resource))+
  theme_void()+
  scale_shape_manual(values=c("gold" = 11,
                              "diamond" = 18,
                              "oil" = 10,"other" = 20))+
  theme(legend.position = "right")
print(world_all)
#africa
africa <-ne_countries(continent ='africa',
                      scale = "large",returnclass = "sf")
# subset to only get African natural resource data
africa_data =subset(final, continent=="africa")
# make the map
africa_map =ggplot()+
  geom_sf(data = africa)+
  geom_sf(data = africa_data,aes(shape=Resource))+
  theme_void()+
  scale_shape_manual(values=c("gold" = 11,
                              "diamond" = 5,
                              "oil" = 10,"other" = 20))+
  theme(legend.position = "right")
print(africa_map)
#columbia, and minerals in different colors
colombia <-ne_countries(country ='colombia', scale = "large", returnclass = "sf")
#include emeralds, exclude diamonds
# perform the transformation
final2 =transform(nr_sf,Resource =factor(replace(as.character(Resource),
                                                 list =!Resource%in% c("oil","gold", "emerald"),values = "other")))
# put everything back in sf form (the thing I forgot in the video)
final2=st_sf(final2)
#subset to columbia 
colombia_data <-subset(final2, country=="colombia")

#mapping polygon data via merging and subsetting
orig_nr$country =str_to_title(orig_nr$country)
#proper capitalization
table(orig_nr$country)
#make the fixes
orig_nr$country[orig_nr$country=="Democratic Republic Of Congo"] = "Democratic Republic of Congo"
orig_nr$country[orig_nr$country=="Republic Of Congo"] = "Republic of Congo"
orig_nr$country[orig_nr$country=="Myanmar (Burma)"] = "Myanmar"
orig_nr$country[orig_nr$country=="Swaziland (Eswatini)"] = "Swaziland"
#get iso3 country code package
library(countrycode)
orig_nr$ISO3 =countrycode(sourcevar = orig_nr$country,origin = "country.name",
                          destination = "iso3c",warn = TRUE)
#kaz didn't go through
orig_nr$ISO3[orig_nr$country=="Kazahkstan"] = "KAZ"
#remove missings
orig_nr_small <-na.omit(subset(orig_nr,select=c("latitude","longitude",
                                                "resource", "country","continent",
                                                "log_wb_val", "ISO3")))
#logwbval is the log transformed resources value
merged_data =left_join(borders, orig_nr, by=c("ISO3"))
#join data 
merged_data =left_join(borders, orig_nr, by=c("ISO3"))
#south america map
south_america<- ne_countries(continent =  "south america", 
                             scale = "medium",
                             returnclass = "sf")
sa_nr <-subset(merged_data,
               country=="Chile"|country=="Peru"|country=="Argentina"|country=="Bolivia"|country=="Brazil"|country=="Suriname"|country=="Guyana"|country=="Ecuador"|country=="Venezuela"|country=="Paraguay"|country=="Uruguay"|country=="Colombia" )
#collapse the data
sa_nr_collapsed = sa_nr%>%
  group_by(country)%>%
  summarize(log_wb_val =sum(log_wb_val, na.rm=TRUE))
#rename the natural resources var
setnames(sa_nr_collapsed, "log_wb_val", "Log Value")
#map
sa_map =ggplot()+
  geom_sf(data = south_america)+
  geom_sf(data = sa_nr_collapsed,aes(fill=`Log Value`))+
  scale_fill_viridis(option = "viridis")+
  ggtitle("Natural Resource Wealth in South America (World Prices), 1994-2014")+
  theme(plot.title =element_text(hjust = 0.5))+
  theme_void()
print(sa_map)
#save
ggsave(sa_map, filename = "south_america_map.png", width = 6.5, height = 6)