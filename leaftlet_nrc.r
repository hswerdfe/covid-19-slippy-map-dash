library(tidyverse)
library(leaflet)
library(viridis)
G_PROJ4DEF <- '+proj=lcc +lat_1=49 +lat_2=77 +lat_0=49 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'
G_CRS_CODE <- 'EPSG:3978'



#####################3
#
# This is supposed to let me color the points on the leaflet map but it is not working 
#
pal <- leaflet::colorNumeric( viridis::viridis_pal(option = "C")(50), domain = 1:300, na.color = "#0F0F0F", alpha = TRUE)

pal(300)

#####################################333
#
# Aggregate points and resize for the leaflet plot
#
aggregate_points <- function(data_points, round_digit = 0, size_adjust = 10000){
  data_points %>% 
    select(Long, Lat, City) %>% 
    mutate_if(is.numeric, round, round_digit) %>% 
    group_by(Long, Lat) %>% 
    summarise(n = n(), City = paste0(unique(City), collapse = "; ")) %>% 
    #count(Long, Lat, sort = T) %>% 
    mutate(Size = sqrt(n)*size_adjust) %>%
    mutate(Popup = paste0(City, "\nN=",n)) 
}



get_prov_shp <- function(){
  canada_HR_shp <- read_sf(file.path("canada_pt_sim.geojson")) %>% 
    st_transform(canada_shp, crs = G_PROJ4DEF)
}

get_hr_shp <- function(){
  shp <- read_sf(file.path("HR000b11a_e_Oct2013_simp.geojson")) %>% 
    st_transform(shp, crs = G_PROJ4DEF)
}
get_pr_shp <- function(){
  shp <- read_sf(file.path("canada_pt_sim.geojson")) %>% 
    st_transform(shp, crs = G_PROJ4DEF)
}

get_gg_map_HR <- function(){
  get_hr_shp() %>% 
    ggplot() +
    geom_sf() +
    theme_map()
}

get_gg_map_prov <- function(){
  get_prov_shp() %>% 
    ggplot() +
    geom_sf() +
    theme_map()
}






##########################333
#
# Get a BASE leaflet map
#
get_leaflet_map <- function(){

  bnds =  c(-7786476.885838887,
            -5153821.09213678,
            7148753.233541353,
            7928343.534071138
  )
  
  
  res <- c(
    38364.660062653464, 
    22489.62831258996, 
    13229.193125052918,
    7937.5158750317505,
    4630.2175937685215,
    2645.8386250105837,
    1587.5031750063501,
    926.0435187537042,
    529.1677250021168,
    317.50063500127004,
    185.20870375074085,
    111.12522225044451,
    66.1459656252646,
    38.36466006265346,
    22.48962831258996,
    13.229193125052918,
    7.9375158750317505,
    4.6302175937685215,
    2.6458386250105836,
    1.5875031750063502,
    0.92604351875370428,
    0.52916772500211673,
    0.31750063500127002,
    0.18520870375074083,
    0.11112522225044451,
    0.066145965625264591
  )
  
  orgn <- c(-34655800, 39310000)

  
  urlTemplate = "https://geoappext.nrcan.gc.ca/arcgis/rest/services/BaseMaps/CBMT3978/MapServer/tile/{z}/{y}/{x}?m4h=t"
  
  tile_attrib <- "NRCAN"
  
  epsg3978 <- leafletCRS(
    crsClass = "L.Proj.CRS", 
    code = G_CRS_CODE,
    proj4def = G_PROJ4DEF,
    origin = orgn,
    bounds =  bnds,
    resolutions = res
  )
  
  
  m <- leaflet(options = leafletOptions(worldCopyJump = F, 
                                        crs = epsg3978,
                                        minZoom = 2, maxZoom = 17)
  ) %>%
    addTiles(urlTemplate = urlTemplate,#x
             attribution = tile_attrib,
             options = tileOptions(continuousWorld = F)
    ) %>% 
    # addMarkers(lng = -75.705793, 
    #            lat = 45.345134,
    #            popup = "My office."
    # ) %>%
    setView(lng = -1*(96+(40/60)+(35/3600)), 
            lat = 62+(24/60),
            zoom = 3
    ) 
  
  
  m
}