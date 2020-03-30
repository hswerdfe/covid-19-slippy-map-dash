library(tidyverse)
library(janitor)
library(viridis)
library(ggrepel)
library(gganimate)
library(gifski)
library(wppExplorer)
library(wbstats)
library(readxl)

library(feather)
library(readxl)
library(httr)


getwd()

##############################3
#
#
# cache_covid_19_gov_interventions()
cache_covid_19_gov_interventions <- function(data_lk = "https://www.bsg.ox.ac.uk/sites/default/files/OxCGRT_Download_latest_data.xlsx",
                                            fn = "OxCGRT_Download_latest_data.feather"  ){
  
  GET(data_lk, write_disk(tf <- tempfile(fileext = ".xlsx")))
  df_raw <- read_excel(tf)
  
  
  df <- 
    df_raw %>% 
    clean_names() %>% # mutate(date = as.character(date)) %>% distinct(date)
    mutate(date = as.Date(as.character(date) , format = "%Y%m%d")) %>%
    select(-x35)
    
  
  df_s <-
  df %>% 
    select(-matches("_notes")) %>% 
    select(-matches("confirmed_")) %>% 
    select(-stringency_index) %>% 
    pivot_longer(. ,cols = 4:ncol(.), names_to = "s_type", values_to = "s_value") %>%
    replace_na(list(s_value = 0)) %>%
    filter(s_value > 0) %>% 
    separate(col = s_type, into = c("s_number", "s_name"), extra = "merge", remove = FALSE) %>% 
    distinct()
  
  
  
  
  to_remove <- unique(df_s$s_type)
  
  df_s <- df_s %>% select(-s_type)
  
  
  df_f <- 
  df %>% 
    select(-matches("_notes")) %>% 
    select(-one_of(to_remove)) %>% 
    pivot_longer(. ,cols = 4:ncol(.), names_to = "final_type", values_to = "final_value") %>%
    replace_na(list(final_value = 0)) %>%
    filter(final_value > 0) %>% 
    distinct()
  
  to_remove <- c(unique(df_f$final_type), to_remove)
  
  
  df_nts  <-
  df %>% 
    select(-one_of(to_remove)) %>%
    pivot_longer(. ,cols = matches("_notes"), names_to = "s_type", values_to = "note_txt") %>% 
    replace_na(list(note_txt = "")) %>% 
    filter(nchar(note_txt)>0) %>% 
    separate(col = s_type, into = c("s_number", "s_name"), extra = "merge") %>% 
    select(-s_name) %>% 
    distinct()

  
  
  df_details <- full_join(df_s, df_nts,  by = c("country_name", "country_code", "date", "s_number")) 
    
  df_all <-full_join(df_f, df_details, by = c("country_name", "country_code", "date")) 
  #df_nts %>% write_feather("gov_intervention_notes.feather")
  #df_dbl %>% write_feather("gov_intervention_values.feather")
  df_all %>% write_feather("gov_intervention_full.feather")
  
  
  return(TRUE)
}




########################################3
#
#
#
get_covid_19_gov_interventions <- function(how_old = Sys.Date()-1){
  if (as.Date(file.info("gov_intervention_full.feather")$ctime )-how_old >= 1){
    cache_covid_19_gov_interventions()
  }
  read_covid_19_gov_interventions()  
}



########################################3
#
#
#
read_covid_19_gov_interventions <-function(){
  read_feather("gov_intervention_full.feather")
}

df <- read_covid_19_gov_interventions()

countries_of_interest <- df %>% 
  dplyr::count(country_name, country_code, date, s_number, sort = TRUE) %>% 
  dplyr::count(country_name, country_code, sort = TRUE) %>% 
  pull(country_code)%>%  head(10) %>% c("CAN", "USA") %>% unique()
  


countries_of_interest <- df$country_code %>% unique() %>% sample(10)%>% c(countries_of_interest) %>% unique()
df %>% 
  filter(country_code %in% countries_of_interest) %>%
  filter(final_type == "stringency_index") %>% 
  ggplot(aes(x = date, y = final_value, color = country_name)) + 
  geom_point() +
  geom_line() +
  facet_wrap(vars(country_name)) +
  labs(title = "Covid-19 country response Stringency Index", y = "Stringency Index")



df %>% 
  group_by(s_name) %>% 
  #filter(s_number == "s5")
  mutate(s_range_norm = (s_value - min(s_value))/ (max(s_value) - min(s_value))) %>% 
  ungroup() %>%          
  filter(country_code %in% countries_of_interest) %>%
  filter(s_name != "is_general") %>%
  distinct() %>% 
  group_by(country_name, s_name) %>% 
  slice(which.max(date)) %>% 
  ungroup() %>% 
  mutate(s_name = gsub(pattern = "_", replacement = " ", x = s_name, ignore.case = TRUE)) %>% 
  ggplot(aes(x = s_name, y = country_name, color = s_range_norm, size = s_range_norm, label = s_range_norm))+
  geom_point() +
  scale_color_viridis(direction = -1) +
  labs(title = "Covid-19 interventions by country") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  guides(size = FALSE , color = FALSE) 

  
df %>% arrange(desc(s_value))       
  
df$country_code %>% unique() %>% sort()
df$s_name %>% unique() %>% sort()
countries <- c()


  select(-contains("notes")) %>% 
    select(-contains("general")) %>%
    select( country_name, country_code ,date ,confirmed_cases,confirmed_deaths, stringency_index) %>% 
    pivot_longer(. ,cols = 4:ncol(.)) %>% 
    replace_na(list(value = 0)) %>% 
    filter(value > 0) %>% 
    pivot_wider(values_from = value, names_from = name) %>% 
    replace_na(list(confirmed_cases = 0,confirmed_deaths = 0, stringency_index = 0)) %>% 
      #mutate(grp = paste0(name, "_", country_code)) %>% 
      ggplot(aes(y = confirmed_cases, x = stringency_index, color = country_name)) +
      geom_point() + 
      geom_line() +
      guides(color = FALSE) +
      scale_y_log10()
      
      
        geom_point() + guides(color = FALSE)+
        geom_line()
      

