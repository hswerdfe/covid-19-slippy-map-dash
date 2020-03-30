
library(waffle)
library(cowplot)
library(xml2)
library(forcats)
library(viridis)
library(tmaptools)
library(sf)
library(tidyverse)
#library(geom_raster)
library(sf)
library(ggthemes)
library(feather)
G_PROJ4DEF <- '+proj=lcc +lat_1=49 +lat_2=77 +lat_0=49 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'
G_CRS_CODE <- 'EPSG:3978'


#crs_canada <- "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
G_CRS_WGS <- "+proj=longlat +datum=WGS84"

#getwd()


##################################333
# Geocode with Open Streeat maps
# but only if not in spreadsheet
geocode_OSM_with_cache <- function(vec, file_loc = file.path("OSM_geocoded_strings.tsv")){
  cache <- read_tsv(file_loc)
  
  to_look_up <- vec[!vec %in% cache$query]
  
  new_loc <- tryCatch({
    geocode_OSM(to_look_up, as.data.frame = T)
  }, error = function(e) {
    cache %>% filter( 1 == 0 )
  }, finally = {
    print("Done")
  })
  
  new_loc <- 
    new_loc %>% 
    mutate(human_check = "none")
  
  if (!is.null(new_loc))
    cache <- bind_rows(cache, new_loc) %>% distinct()
  
  cache %>% write_tsv(file_loc)
  cache %>% filter(query %in% vec)
}


cache_viri_health_data <- function(){
  tlbList <- download_viri_health_data()
  
  for (nm in names(tlbList)){
    df <- tlbList[[nm]]
    print(nm)
    #print(df)
    write_feather(x = df, path = paste0("viri_health_", nm, ".feather"))
  }
}



########################################3
#
#
#
Get_viri_health_data <- function(type= "cases", how_old = Sys.Date())
{
  fn <- paste0("viri_health_", type, ".feather")
  if (how_old - as.Date(file.info("covid_19_UT_data.feather")$ctime ) >= 1){
    cache_viri_health_data()
  }
  read_covid_19_UT_data()
}



########################################3
#
#
#
read_viri_health_data <- function(type= "cases"){
  fn <- paste0("viri_health_", type, ".feather")
  read_feather(fn)
}


########################################3
#
#
#
get_viri_health_age_hist <- function(){
  cases <- read_viri_health_data("cases")
  deaths <- read_viri_health_data("deaths")
  
  
  d <- deaths %>% dplyr::count(Age) %>% mutate(type = "deaths")
  c <-cases %>% dplyr::count(Age) %>% mutate(type = "Infections")
  
  
  df <- 
    bind_rows(d, c) %>% 
    group_by(type) %>% 
    mutate(base_num = sum(n))
  
  max_case_dt <- lubridate::parse_date_time(paste0(cases$Date, "-2020"), orders = "d-b-Y", locale = "us") %>% max()
  max_death_dt <- lubridate::parse_date_time(paste0(deaths$Announced, "-2020"), orders = "d-b-Y", locale = "us") %>% max()
  max_dt <- as.character(  max(max_death_dt, max_case_dt) )
  
  n_deaths <- nrow(deaths)
  n_cases <- nrow(cases)
  df_l <- 
    df %>% 
    group_by(type) %>%
    mutate(base_num = sum(n)) %>% 
    filter(!is.na(Age)) %>% 
    ungroup() %>% 
    group_by(type, base_num) %>%
    summarise( vert = max(n)*4/5, hor = 1 ) %>% 
    mutate(lbl = paste0(type," " ,base_num))
    
  p <- 
    df %>% 
    filter(!is.na(Age)) %>% 
    #mutate(age_missing = is.na(Age)) %>% 
    ggplot(aes(y = n, x = Age, fill = type, label = n)) + 
    geom_col( ) + 
    geom_label(fill = "white") +
    geom_text(data = df_l, mapping = aes(x = hor, y = vert, label = lbl) , fill = "white", size = 7, color = "grey", hjust  = "left") +
    facet_grid(rows = vars(type), scales = "free_y") + 
    scale_fill_manual(values = c("#000000", "#006400")) + 
    labs (title = "Deaths and infections by age in Canada.", subtitle = paste0("Source Virihealth.com, " , max_dt) ) +
    theme(legend.position = "none")
  #, cols = vars(age_missing))# +
    #theme_bw()
  p

  w_df <-
    df %>% 
    mutate(age_missing = is.na(Age)) %>% 
    group_by(age_missing, type) %>% 
    summarise(n = sum(n)) %>% 
    mutate(age_missing_lbl = ifelse(age_missing, "Age not reported", "Age is known")) %>% 
    mutate(Category = paste0(type, "- ", age_missing_lbl)) %>% 
    ungroup() %>%
    mutate(f = 3*100*n/sum(n)) %>% 
    arrange(desc(n))
  
  parts <- w_df %>% pull(f) %>% round()
  names(parts) <- w_df %>%  pull(Category)
  q <- waffle(parts, title = "Covid-19 Deaths and infections\nwhen Age is reported in Canada.", size = 1, colors = c("#0d98ba", "#006400", "#808080", "#000000"), rows = 20)
  
  q
  plot_grid(p,q, labels = NULL ) 
  
    #filter(is.na(Age)) %>% 
  
}



########################################3
#
#
#
get_tests_plot <- function(){
  df <- read_viri_health_data("tests")
  df %>% 
    clean_names() %>% 
    mutate(tests_1m = as.integer(sub(",", "",tests_1m)), tests = as.integer(sub(",", "",tests)),  positive= as.integer(sub(",", "",positive))) %>%
    mutate(negative = tests - positive) %>%
    select(prov, negative, positive, tests, tests_1m ) %>%
    mutate(per = 100*positive/tests) %>%
    filter(!prov %in% c("TOT","RC") ) %>%
    pivot_longer(cols = c("negative", "positive")) %>%
    mutate(prov = paste0(prov, "\n tests =", tests,"\nper million =",tests_1m, "\npositive=", round(per, 1), "%")) %>%
    mutate(prov = fct_reorder(prov, tests_1m)) %>% 
    mutate(wdth = tests_1m) %>% #sqrt(Tests)) %>% 
    mutate(percent = ifelse(name == "negative", 100 - per, per)) %>% 
    ggplot(aes(x=wdth/2, y = percent, fill = name, width = wdth)) +
    geom_bar(position="fill", stat="identity") +
    scale_fill_manual(values = c("#d3d3d3", "#006400")) +
    facet_wrap(vars(prov)) +
    #facet_grid(side1 ~ side2) +
    coord_polar("y") +
    labs(title = "COVID-19 tests performed by provinces.", fill = "COVOD-19 test results." ) +
    theme_void() +
    theme(
      panel.border = element_rect(linetype = "dashed", fill = NA), 
      plot.title = element_text(size = 20), 
      strip.text.x = element_text(size = 12)
    )
}


#####################################33
#
#
#
get_viri_health_cases_circles <- function(crs_out = G_PROJ4DEF , ...){#G_CRS_WGS){
  df <- read_viri_health_data("cases")
  df <- 
  df %>% 
    mutate(for_geo = trimws(tolower(paste0(City, ", ", Prov, ", ", "Canada")))) 
  
  geo <- 
  df %>% distinct(for_geo) %>% 
    pull() %>%
    geocode_OSM_with_cache() %>% 
    mutate(for_geo := query)
  
  
  df_pnts <- 
    df %>% left_join(geo, by = "for_geo") %>% 
     replace_na(list(lat = 75, lon = -40)) %>% 
     mutate(for_lbl = paste0(City, "," , Prov))
  
  agg_circle_df <- df_pnts %>%  agg_pnts(for_lbl = "for_lbl", ...)
  
  return(agg_circle_df)
  #   #dplyr::count(for_geo, lat, lon, sort = T) %>% view()
  #    st_as_sf(coords = c("lon", "lat"))
  # 
  # # , crs = G_CRS_WGS) %>%  
  # #    st_transform(crs = crs_out) %>% 
  # #   aggregate_points()

}
agg_pnts <- function(df, grp = c("lon", "lat"), for_lbl = "for_geo", round_digit = 0, size_adjust = 3000){
  df %>% 
    group_by_at(grp) %>% 
    summarise(N = n(), 
              lbl = trimws(first(!!sym(for_lbl)))) %>% 
    ungroup()%>% 
    mutate_if(is.numeric, round, round_digit) %>% 
    group_by_at(grp) %>% 
    summarise(N = sum(N), 
              lbl = paste0(lbl, collapse = "; ")) %>% 
    mutate(Size = sqrt(N) * size_adjust, 
           Popup = paste0(lbl, " N = ", N)) %>% arrange(desc(Size))
}




#########################################
#
# Read data from VIRI Health websit
#
download_viri_health_data <- function(webpage_url = "https://virihealth.com/", crs_out = G_CRS_WGS){
  #########################
  # Dont do this all the time
  #webpage <- xml2::read_html(webpage_url)
  
  library(curl)
  #req <- curl_fetch_memory("https://virihealth.com/")
  webpage <- xml2::read_html("C:/Users/hswerdfe/Documents/Projects/covid_19/dashboard/slippy_map/virihealth.html")
  #req <- curl_fetch_memory("https://eu.httpbin.org/get?foo=123")
  #str(req)
  
  ret_val <- list()
  ret_val[["totals"]] <- rvest::html_table(webpage)[[1]] %>% 
    tibble::as_tibble(.name_repair = "unique") # repair the repeated columns
  
  
  ret_val[["prov"]] <- rvest::html_table(webpage)[[2]] %>% 
    tibble::as_tibble(.name_repair = "unique") # repair the repeated columns
  
  ret_val[["tests"]] <- rvest::html_table(webpage)[[3]] %>% 
    tibble::as_tibble(.name_repair = "unique") # repair the repeated columns
  
  
  ret_val[["sex"]] <- rvest::html_table(webpage)[[4]] %>% 
    tibble::as_tibble(.name_repair = "unique") # repair the repeated columns
  
  
  ret_val[["Health Region"]] <- rvest::html_table(webpage)[[5]] %>% 
    tibble::as_tibble(.name_repair = "unique") # repair the repeated columns
  
  
  ret_val[["Percent_recovered"]] <- rvest::html_table(webpage)[[6]] %>% 
    tibble::as_tibble(.name_repair = "unique")# repair the repeated columns
  
  
  ret_val[["deaths"]] <- rvest::html_table(webpage)[[7]] %>% 
    tibble::as_tibble(.name_repair = "unique") 
  
  
  ret_val[["cases"]] <- rvest::html_table(webpage)[[8]] %>% 
    tibble::as_tibble(.name_repair = "unique") 
  
  # ret_val[["cases"]] <- rvest::html_table(webpage)[[9]] %>% 
  #   tibble::as_tibble(.name_repair = "unique") 
  # 
  # 
  # ret_val[["cases"]] <- ret_val[["cases"]] %>% 
  #   mutate(to_geo = tolower(paste(City, Prov, "Canada",sep = ", ")) )
  # 
  # for_geo <- ret_val[["cases"]] %>% pull(to_geo) %>% unique()
  # cases_cols <- ret_val[["cases"]] %>% colnames()
  # 
  # 
  # 
  # geo_loc <- geocode_OSM_with_cache(for_geo)
  # 
  # ret_val[["cases"]] <- left_join(ret_val[["cases"]], geo_loc, by = c("to_geo" = "query"))
  # 
  # 
  # 
  # ret_val[["cases"]] <- 
  #   ret_val[["cases"]] %>% 
  #   mutate(to_geo = if_else(is.na(lat),    tolower(paste( Prov, "Canada",sep = ", ")), to_geo ))
  # 
  # ctry2 <- ret_val[["cases"]] %>% filter(is.na(lat)) 
  # ctry1 <- ret_val[["cases"]] %>% filter(!is.na(lat)) 
  # geo_loc <- geocode_OSM_with_cache(ctry2$to_geo %>% unique())
  # 
  # 
  # ctry2 <- ctry2 %>% select(cases_cols)
  # ctry2 <- left_join(ctry2, geo_loc, by = c("to_geo" = "query"))
  # 
  # ret_val[["cases"]] <- bind_rows(ctry2, ctry1)
  # 
  # ret_val[["cases"]] <- 
  #   ret_val[["cases"]] %>% 
  #   replace_na(list(lat = 75, lon = -40)) %>% 
  #   st_as_sf(coords = c("lon", "lat"), crs = G_CRS_WGS) %>%  
  #   st_transform(crs = crs_out)
  # 
  # 
  return(ret_val)
}





############################33
# TODO ::
Plot_provincial_choropleth <- function(data){

  
}

 ############3 Some bugs below ##################
# data <- NULL
# data <- read_viri_health_data()
# 
# 
# 
# canada_HR_shp <- read_sf(file.path("HR000b11a_e_Oct2013_simp.geojson")) %>% 
#   st_transform(canada_shp, crs = G_PROJ4DEF)
# 
# canada_HR_shp %>% 
#   ggplot() +
#   geom_sf() +
#   theme_map()
# 
# 
# 
# canada_prov_shp <- read_sf(file.path("canada_pt_sim.geojson")) %>% 
#   st_transform(canada_shp, crs = G_PROJ4DEF)
# 
# 
# canada_prov_shp %>% 
#   ggplot() +
#   geom_sf(alpha = 0, size = 1.25, color = "black") +
#   theme_map()
# 
# 
# data$prov
# 
# points<- 
# data$cases %>% 
#   st_transform(canada_shp, crs = G_PROJ4DEF)
#   # st_coordinates(data$cases) %>% 
#   # as_tibble() %>% 
#   # bind_cols(., data$cases)
#   #   
# 
# 
# data$cases %>% group_by(Y) %>% summarise(n = n()) %>% view()
#   
# p <- 
#   canada_shp %>% 
#   ggplot() +
#   geom_sf() +
#   theme_map()
# 
# p + 
#   geom_sf(data = points, mapping = aes()) +
#   scale_color_viridis() +
#   scale_fill_viridis()
# 
# data$cases %>% view()
# fn <- paste0("virihealth_cases_", Sys.Date(), ".tsv")
# ret_val[["cases"]] %>% write_tsv(fn)
# 
# 
# fn <- paste0("virihealth_by_city_", Sys.Date(), ".tsv")
# ret_val[["cases"]] %>% group_by(City, Prov, lat, lon,  lat_min, lat_max, lon_min, lon_max) %>% summarise(N = n()) %>% 
#   write_tsv(fn)
# 
# 
# fn <- paste0("virihealth_by_prov_", Sys.Date(), ".tsv")
# ret_val[["cases"]] %>% group_by(Prov) %>% summarise(N = n()) %>% 
#   write_tsv(fn)
# 
# 
# lat_lng <- st_coordinates(data$cases)
# rbind(lat_lng, data$cases)
# 
# 
# data$tests %>% 
#   mutate(Tests = as.integer(sub(",", "",Tests)),  Positive= as.integer(sub(",", "",Positive))) %>%
#   mutate(Negative = Tests - Positive) %>%
#   select(Prov, Negative, Positive, Tests ) %>% 
#   mutate(Per = 100*Positive/Tests) %>%
#   filter(!Prov %in% c("TOT","RC") ) %>% 
#   pivot_longer(cols = c("Negative", "Positive")) %>%
#   mutate(Prov = paste0(Prov, "\n Tests =", Tests, "\nPositive=", round(Per, 1), "%")) %>%
#   mutate(Prov = fct_reorder(Prov, 1/Tests)) %>%
#   ggplot(aes(x=sqrt(Tests)/2, y = value, fill = name, width = sqrt(Tests))) +
#   geom_bar(position="fill", stat="identity") + 
#   facet_wrap(vars(Prov)) +
#   #facet_grid(side1 ~ side2) + 
#   coord_polar("y") + 
#   labs(title = "COVID-19 tests performed by provinces.", fill = "COVOD-19 test results." ) + 
#   theme_void() +
#   theme(
#         panel.border = element_rect(linetype = "dashed", fill = NA)
#         ) 
# 
#   
#   
#   
