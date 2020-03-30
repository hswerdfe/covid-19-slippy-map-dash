library(tidyverse)
library(janitor)
library(viridis)
library(ggrepel)
library(gganimate)
library(gifski)
library(wppExplorer)
library(wbstats)
#G_base_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-"
G_base_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_"
MIN_COUNT_START <- 75
NUM_COUNTRIES_LABEL <- 10
Highlight_country <- "Canada"
country_2_remove <- c("China","Cruise Ship") 
#country_2_remove <- c("Cruise Ship") 
#country_2_remove <- c("BLA BLA BLA") 

#################################
#
# get CSV File from URL link
#
covid_19_get_file <- function(type = "Confirmed", base_url = G_base_url){
  full_url  <- paste0(base_url, tolower(type), "_global", ".csv")
  df_raw <- read_csv(full_url)
  df_raw$type <- type
  return(df_raw)
}  



#################################
#
#  Get JHU DATa
#
covid_19_get_data <- function(base_url = G_base_url, types = c("Confirmed", "Deaths","Recovered")){
  
  a <- map(.x = types, .f = covid_19_get_file,  base_url = base_url)
  a <- a %>% map_df(.f = rbind)
  a <- a %>% clean_names()
  
  a %>% pivot_longer(cols = starts_with(match = "x"), names_to = "date") %>% 
    mutate(date = as.Date(date, "x%m_%d_%y")) %>%
    mutate(full_geo = paste0(province_state, "_", country_region))
}  

########################################
#
get_covid_19_International_compare <- function(){
  
  df_raw <- NULL
  df_raw <- covid_19_get_data()
  df<- df_raw 
  
  #####################3
  # what is the latest date
  MAX_DATE <- df$date %>% max()
  df %>% filter(country_region == Highlight_country) %>% pull(date) %>% max()
  #df %>% group_by(country_region) %>% slice(which.max(date)) %>% View()
  
  top_countries <-
    df %>% 
    filter(type == "Confirmed") %>%
    group_by(country_region) %>%
    summarise(value = max(value)) %>% 
    arrange(desc(value)) %>% head(16) %>% 
    pull(country_region)
  
  
  df %>% 
    group_by(country_region, type, date) %>% 
    summarise(value = sum(value)) %>% 
    pivot_wider(names_from = type, values_from = value) %>% 
    mutate(currently_sick = Confirmed - Deaths - Recovered) %>% 
    pivot_longer(cols = c("currently_sick" ,"Confirmed" ,"Deaths" , "Recovered"), names_to = "type") %>% 
    filter(type != "Confirmed" ) %>% 
    filter(country_region %in% top_countries) %>%
    group_by(country_region) %>%
    mutate(value_max = max(value)) %>% 
    ungroup(country_region) %>%
    mutate(country_region = paste0(country_region, "\nMax infected (", value_max, ")")) %>% 
    mutate(country_region = fct_reorder(country_region, 1/value_max)) %>% 
    ggplot(aes(x = date, y = value, color = type, fill= type)) + 
    geom_point() +
    geom_line() + 
    scale_y_log10() +
    facet_wrap(vars(country_region)) +
    theme_bw() +
    labs(title = "COVID-19 most infections in country", y = "Counts", X = "Dates")
  
  
}
# 
# getwd()
# get_covid_19_growth_compare_save(TYPE_CASE = "Confirmed" , fn_ext = ".svg")

##############################3
#
get_covid_19_growth_compare_save <- function(fn_base = "covid-19-growth",fn_ext = ".svg",  ...){
  l = list(...)
  inputs <- paste0(l, collapse = "_")
  fn = paste0(fn_base, inputs, as.character(Sys.Date()), fn_ext, collapse = "_")
  p <- get_covid_19_growth_compare(...)
  ggsave(file=fn, plot=p, width=10, height=8)
}



################################
get_covid_19_new_vs_total_infections(TYPE_CASE = c("Confirmed"), 
                                     lag_count = 7,
                                     Highlight_country = "Canada",
                                     country_2_remove = c("Diamond Princess") ,
                                     MIN_COUNT_START = 100, 
                                     NUM_COUNTRIES_LABEL = 20){
  
  
  df_raw <- NULL
  df_raw <- covid_19_get_data(types = TYPE_CASE)
  df<- df_raw 
  
  df_p <- 
    df %>% 
    group_by(country_region, date) %>% 
    summarise(value = sum(value)) %>% 
    group_by(country_region) %>% 
    mutate(value_before = dplyr::lag(x = value, n = lag_count, order_by = date)) %>% #pull(value_before) %>% 
    mutate(new_cases = value - value_before) %>% 
    filter(value >= MIN_COUNT_START) %>% 
    filter(!country_region %in% country_2_remove)
  
  df_p_dt %>% distinct(new_cases)
  df_p_dt <-
  df_p %>% 
    ungroup() %>% 
    mutate(value = MIN_COUNT_START*2, 
              new_cases = max(new_cases , na.rm = T)/2) %>% 
    group_by(date) %>% 
    summarise(value = MIN_COUNT_START*2, 
              new_cases = max(new_cases, na.rm = T)/2,) %>% 
    mutate(country_region := date) #%>% view()
  
   df_p_pt <- 
    df %>% filter(country_region == Highlight_country) %>% 
    group_by(full_geo, date) %>% 
    summarise(value = sum(value)) %>% 
    group_by(full_geo) %>% 
    mutate(value_before = dplyr::lag(x = value, n = lag_count, order_by = date)) %>% #pull(value_before) %>% 
    mutate(new_cases = value - value_before) %>% 
    filter(value >= MIN_COUNT_START) %>% 
    filter(!full_geo %in% country_2_remove) %>% 
    rename(country_region := full_geo)
  
  
  
  
  df_p_h <- df_p %>% filter(country_region == Highlight_country) 
  
  countries_to_label <- 
    df_p %>% dplyr::count(country_region, sort = T) %>% head(NUM_COUNTRIES_LABEL) %>% pull(country_region)
  df_p_l <- 
    df_p %>% filter(country_region %in% countries_to_label)
  
  p<-
  df_p %>% 
    filter(country_region != Highlight_country) %>% 
    ggplot(aes(x = value, y = new_cases, group = country_region, label = country_region))+
             geom_point(color = "grey") +
             geom_line(color = "grey") +
    scale_y_log10() +
    scale_x_log10() +
    #scale_color_viridis() +
    #facet_grid(cols = vars(type), scales = "free") +
    annotation_logticks(sides = "lb") +
    geom_text(data =df_p_l ) + 
    #geom_text(data = df_p_dt, color = "grey", size = 3) +
    guides(color = FALSE, fill = FALSE) +
    theme_bw() +
    labs(title = paste0("Global Covid-19 \n data date = ", max(df_p$date)), 
         x = "Cummulitive Cases", y = "New Cases")
  p<-
  p + geom_point(data = df_p_h,   color = "red") +
    geom_line(data = df_p_h, color = "red", size = 2) +
    geom_label(data = df_p_h,fill = "red")
    
  
  
  p<-
    p + geom_point(data = df_p_pt,   aes(color = country_region)) +
    geom_line(data = df_p_pt, aes(color = country_region)) +
    geom_label(data = df_p_pt , aes(fill = country_region))
  
  
  
  
      
    anim <- p +
    transition_reveal(date)

  animate(anim, nframes = 200, renderer = gifski_renderer("gganim.gif", width = 2024, height = 768), end_pause = 100)
}
  
######################################
#
#
#get_covid_19_growth_compare(TYPE_CASE = "Confirmed", NUM_COUNTRIES_LABEL = 10, MIN_COUNT_START = 100, country_2_remove = c("Cruise ship") )
#get_covid_19_growth_compare(TYPE_CASE = "Deaths", country_2_remove = c("Cruise ship") , NUM_COUNTRIES_LABEL = 12, MIN_COUNT_START = 20)
#
get_covid_19_growth_compare <- function(TYPE_CASE = c("Deaths", "Confirmed") , MIN_COUNT_START = 100, 
                                        NUM_COUNTRIES_LABEL = 15,
                                        Highlight_country = "Canada",
                                        country_2_remove = c("China","Diamond Princess") 
                                        ){
  
  df_raw <- NULL
  df_raw <- covid_19_get_data(types = TYPE_CASE)
  df<- df_raw 
  
  #####################3
  # what is the latest date
  MAX_DATE <- df$date %>% max()
  df %>% filter(country_region == Highlight_country) %>% pull(date) %>% max()
  #df %>% group_by(country_region) %>% slice(which.max(date)) %>% View()
    
  # df %>% count(full_geo)           
  # df %>% count(country_region)     
  # df %>% count(province_state, sort = T)     
  
  ###################################
  #Mutate the dataframe for plotting
  df_p <-   
    df %>% # filter(country_region == "Canada") %>% 
    arrange(date, full_geo) %>% 
    #group_by(full_geo, lat, long, type, country_region, province_state) %>% 
    
    group_by( type, country_region, date) %>% 
    #group_by( type, province_state, date) %>% 
    summarise(value = sum(value)) %>% 
    filter(value > MIN_COUNT_START) %>% ungroup() %>% 
    group_by( type, country_region) %>% 
    #group_by( type, province_state) %>% 
    
    mutate(date_min = min(date)) %>% 
    ungroup() %>% 
    mutate(date_since = date - date_min) %>% 
    mutate(delta_dates = date_since - lag(date_since, n = 1), delta_value = value - lag(value, n = 1)) %>% 
    mutate(const = (value / lag(value, n = 1))/as.integer(delta_dates)) %>% 
    filter(!(country_region %in% country_2_remove)) %>%
    filter(type %in% TYPE_CASE) %>% 
    drop_na()
  
  
  df_p_pt <- 
    df %>% 
    filter(country_region == Highlight_country) %>% 
    select(-country_region) %>% 
    rename(country_region := province_state) %>% 
    arrange(date, country_region) %>% 
    #group_by(full_geo, lat, long, type, country_region, province_state) %>% 
    
    group_by( type, country_region, date) %>% 
    #group_by( type, province_state, date) %>% 
    summarise(value = sum(value)) %>% 
    filter(value > MIN_COUNT_START) %>% ungroup() %>% 
    group_by( type, country_region) %>% 
    #group_by( type, province_state) %>% 
    
    mutate(date_min = min(date)) %>% 
    ungroup() %>% 
    mutate(date_since = date - date_min) %>% 
    mutate(delta_dates = date_since - lag(date_since, n = 1), delta_value = value - lag(value, n = 1)) %>% 
    mutate(const = (value / lag(value, n = 1))/as.integer(delta_dates)) %>% 
    filter(!(country_region %in% country_2_remove)) %>%
    filter(type %in% TYPE_CASE) %>% 
    drop_na()


  df_p_pt_l <- 
    df_p_pt %>% 
    group_by(country_region) %>% 
    slice(which.max(date)) %>% 
    mutate(lbl = paste0(country_region, "\n", value))
  
  
  longest_countries <-
  df_p %>% #pull(province_state) %>% unique()
    group_by(country_region) %>% 
    mutate(N = n()) %>% 
    slice(which.max(date)) %>% 
    ungroup() %>% 
    arrange(desc(N)) %>% 
    mutate(., rank = 1:nrow(.)) %>% 
    filter(rank < NUM_COUNTRIES_LABEL | country_region == Highlight_country) %>% 
    pull(country_region) %>% 
    unique() 
  

  ##############################
  # Mutate the dataframe so we have only the ones we will label
  df_p_l <- 
    df_p %>% 
    #filter(rank < NUM_COUNTRIES_LABEL | country_region == Highlight_country) %>% 
    filter(country_region %in% longest_countries | country_region == Highlight_country) %>% 
    #mutate(lbl = paste(country_region, "\ncases=", value,"\ndays later=", date_since)) %>% 
    mutate(lbl_short = paste(country_region, "\n", value)) %>% 
    #mutate(lbl_short = paste(province_state, "\n", value)) %>% 
    group_by(country_region, type) %>% 
    #group_by(province_state) %>% 
    slice(which.max(date)) %>% 
    ungroup()
  
  
  df_dt <- 
    df_p %>% 
    group_by(type) %>% 
    mutate(x = max(date_since) *0.05,
           y = 0.9 * max(value), 
           lbl = as.character(date)) %>% 
    slice(which.max(date))
  
  
  # %>% slice(which.max(date))
  # 
  # arrange(date)
  # df_dt$lbl = as.character(df_dt$date) 
  # df_dt$x = 7
  # df_dt$y = max(df_p$value) * 0.9
  # df_dt<-df_dt %>% slice(which.max(date))

  #######################################
  # Plot the points and lines
  p <-
    df_p %>%
    ggplot(aes(x = date_since, y = value, color = country_region), color = "grey", alpha = 0.5) + 
    #ggplot(aes(x = date_since, y = value, color = province_state), color = "grey", alpha = 0.5) + 
    geom_point(color = "grey") + 
    geom_line(aes(group = country_region), color = "grey") + 
    #geom_line(aes(group = province_state)) + 
    geom_text(data = df_dt, aes(x= x, y= y, label = lbl), size=8, color = "grey", hjust = 0) +
    #geom_smooth(aes(group = country_region), method = "lm", se = F) + 
    #geom_smooth(inherit.aes = F, mapping = aes(x = date_since, y = value), size = 2, color = "black") +
    scale_y_log10() +
    #scale_color_viridis() +
    facet_grid(cols = vars(type), scales = "free") +
    annotation_logticks(sides = "l") +
    theme_bw() +
    theme(legend.position = "none")
  
  
  #p
  ############## xcvxcvxcv########################
  # Add the points and lines for Canada 
  p<-  
    p+
    geom_point(data = df_p%>% filter(country_region == Highlight_country), color = "red", size = 2) + 
    geom_line(data = df_p%>% filter(country_region == Highlight_country), color = "red", size = 2) 
    #geom_smooth(data = df_p%>% filter(country_region == Highlight_country), method = "lm", color = "black", size = 1, fullrange=TRUE, linetype = "dashed") + 
  
  p<-
    p + 
    geom_point(data = df_p_pt, size = 1.5) + 
    geom_line(data = df_p_pt , size = 1.5) +
    geom_label_repel(data = df_p_pt_l , mapping = aes(label = lbl, color = country_region))#, color = "black")
  
  p <- p +  
    labs(caption = paste0("data = ", "John Hopkins University"),
         x = paste0("Days Since ",MIN_COUNT_START , " counts"), y = "Count",#paste0("Number Of ", TYPE_CASE), 
         title = paste0("COVID-19 Growth By Country\nlast day of data = ", MAX_DATE)) 
  #p
  
  
  ########################################
  # Add labels for some of the lines
  p <- p +  geom_label_repel(data = df_p_l %>% filter(country_region != Highlight_country) , mapping = aes(label = lbl_short), alpha = 0.75, seed = 123)
  #p <- p +  geom_label(data = df_p_l , mapping = aes(label = lbl_short), alpha = 0.75, seed = 123)
  
  p <- p +  geom_label(data = df_p_l %>% filter(country_region == Highlight_country) , mapping = aes(label = lbl_short), alpha = 0.75, seed = 123, color = "red")
  p

  # 
  # anim <- p + 
  #   transition_reveal(date)
  # 
  # animate(anim, nframes = 200, renderer = gifski_renderer("gganim.gif", width = 1024, height = 768), end_pause = 100)
  # 
  # 
  # 

}

# country_list_raw <- wbstats::wb(indicator = "SP.POP.TOTL", startdate = 2018, enddate = 2018) %>% as_tibble()
# country_list <- country_list_raw
# country_list <- 
#   country_list %>% 
#   select(value, country) %>%
#   rename(pop := value)
# df_raw <- covid_19_get_data(types = TYPE_CASE)
# 
# a <- df_raw %>% left_join(country_list, by = c("country_region" = "country"))
# 
# a %>% filter(is.na(pop)) %>% distinct(country_region) %>% view()
# 
# b <- a %>% mutate(value = 10^6 *value / pop)
# 
# 
# df_raw <- b
# a %>% count(date)
# df_raw <- NULL
# df_raw <- covid_19_get_data(types = TYPE_CASE)
# df <- df_raw
# df %>% 
#   group_by(country_region, type, date) %>% 
#   summarise(value = sum(value)) %>% 
#   pivot_wider(names_from = type, values_from = value) %>% 
#   mutate(death_rate = Deaths/Confirmed) %>% 
#   filter(Deaths >= 10 & Confirmed >= 100) %>% 
#   ggplot(aes(x = death_rate)) +
#   #geom_density() +
#   geom_histogram(bins = 15, alpha = 0.5, color = "black") +
#   geom_point(y=0) +
#   #scale_x_log10() +
#   #annotation_logticks(sides = "b") 
#   theme_bw()
#   
#   
# 
# df %>% 
#   group_by(country_region, type, date) %>% 
#   summarise(value = sum(value)) %>% 
#   pivot_wider(names_from = type, values_from = value) %>% 
#   mutate(death_rate = Deaths/Confirmed) %>% 
#   filter(Deaths >= 10 & Confirmed >= 100) %>% 
#   filter(Deaths >= 10 & Confirmed >= 100) %>% 
#   group_by(country_region) %>% 
#   slice(which.max(date)) %>%
#   ggplot(aes(x = Deaths, y = Confirmed)) + 
#   geom_point(aes(color = death_rate)) +
#   geom_text_repel(aes(label = country_region)) +
#   #geom_path(aes(group =  country_region) )+
#   scale_y_log10() +
#   scale_x_log10() +
#   annotation_logticks(sides = "lb") +
#   #theme(legend.position = "none") + 
#   geom_abline()




# p<-
# df  %>% filter(country_region == "US") %>% 
#   ggplot(aes(x = date, y = value, color = full_geo, group = full_geo)) + 
#   geom_line() +
#   geom_point() +
#   #geom_line(data = df %>% filter(country_region == "Canada"), color = "black" , size = 3, stroke = 3) +
#   #viridis::scale_color_viridis() + 
#   scale_y_log10() + 
#   facet_grid(rows = vars(type)) +
#   theme(legend.position = "none") + 
#   annotation_logticks(sides = "l") 
# 
# 
# ggplotly(p)


#   df %>% 
#   arrange(date, full_geo) %>% 
#   group_by(full_geo, lat, long, type, country_region, province_state) %>% 
#   mutate(delta_date = date - lag(date, n = 5), delta_value = value - lag(value, n = 5), const = (value / lag(value, n = 7))/7) %>% 
#   #mutate(slope = lag_value / as.integer(lag_date)) %>% 
#   #mutate(up_rate = slope / value) %>% # View()
#   mutate(grp = paste(full_geo, type, sep = "_")) %>% 
#   ungroup() %>% 
#   filter(type == "Confirmed") %>% 
#   filter(value > 75) %>% 
#   arrange(full_geo, date) %>% 
#   ggplot(aes(x = date, y = const, color = country_region)) + 
#   geom_point() + 
#   #geom_line(aes(group = full_geo)) + 
#   #geom_smooth(aes(group = full_geo), method = "gam", se = F) +
#   scale_y_log10() +
#   #theme(legend.position = "none") + 
#   facet_grid(rows = vars(type))
#   
#   
#   
#   
# ggplotly(p)
#   mutate(delta_date = date - lag(date, n = 5), delta_value = value - lag(value, n = 5), const = (value / lag(value, n = 7))/7) %>% 
#     #mutate(slope = lag_value / as.integer(lag_date)) %>% 
#     #mutate(up_rate = slope / value) %>% # View()
#     mutate(grp = paste(full_geo, type, sep = "_")) %>% 
#     ungroup() %>% 
#     filter(type == "Confirmed") %>% 
#     
#     filter(country_region != "China") %>% 
#     arrange(full_geo, date) %>% 
#   
# ggplotly(p)
# 
# df %>% count(country_region) %>% view()
# df %>% filter(country_region == "Canada" & type == "Confirmed")  %>% arrange(desc(date))
# 
# library(plotly)  
#          
#          
#   group_by(date) %>% 
#   summarise(value = sum(value)) %>%
#   arrange(desc(date)) %>%
# 
