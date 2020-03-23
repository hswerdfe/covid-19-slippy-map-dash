
#Public_COVID-19_Canada
#library(googlesheets)
library(gsheet)

library(janitor)
library(viridis)
library(ggrepel)
library(gganimate)
library(feather)
library(sqldf)
library(gifski)
library(scales)
library(cowplot)
library(tidyverse)
#gs_auth(new_user = TRUE)




###############################333
# Split U of T Age Column up into min max
split_age <- function(df){
  df <- 
    df %>% separate("age", into = c( "min_age", "max_age"), remove = FALSE) %>% 
    mutate(min_age = as.integer(min_age),
           max_age = as.integer(max_age)) %>% 
    mutate(min_age = ifelse(is.na(min_age), 0, min_age)) %>% 
    mutate(max_age = ifelse(is.na(max_age), ifelse(as.numeric(age), age, NA), max_age)) %>% 
    mutate(max_age = as.integer(max_age)) %>% 
    mutate(age = fct_reorder(age, min_age))  #%>% df %>% count(age, min_age, max_age)
  
  df
  #df %>% count(age, min_age, max_age)
}

############################################
# Gte the recommended citation
get_covid_19_UT_citation <- function(dt = Sys.Date()){
  recomended_citation = paste0("COVID-19 Canada Open Data Working Group. Epidemiological Data from the COVID-19 Outbreak in Canada. https://github.com/ishaberry/Covid19Canada. (" ,
                               as.character(dt), ").")
  return(recomended_citation)
}


###################################33
# Download the spreadshee and format it a bit
download_covid_19_UT_data_download <- function(){
  #df_raw <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1D6okqtBS3S2NRC7GFVHzaZ67DuTw7LX49-fqSLwJyeo/htmlview?sle=true#')
  df_raw <-  gsheet2tbl("https://docs.google.com/spreadsheets/d/1D6okqtBS3S2NRC7GFVHzaZ67DuTw7LX49-fqSLwJyeo/htmlview?sle=true#gid=942958991")
  df_names <- df_raw %>% slice(2) %>% unlist(., use.names=FALSE)

  df <- df_raw[3:nrow(df_raw),]
  
  colnames(df) <- df_names
  df <- df %>% clean_names()
  
  df <- df %>% split_age()
  df <- 
    df %>% 
    mutate(date_report = as.Date(date_report, format = "%d-%m-%Y")) %>% 
    mutate(report_week = as.Date(report_week, format = "%d-%m-%Y")) 
  return(df)
}


############################
# cace the spreadsheet 
cache_covid_19_UT_data <- function(){
  df <- NULL
  df <- download_covid_19_UT_data_download()
  df %>% write_feather("covid_19_UT_data.feather")
}

##############################
# read from file
read_covid_19_UT_data <- function(){
  df <- read_feather("covid_19_UT_data.feather")
  df
}

get_covid_19_UT_data <- function(how_old = Sys.Date())
{
  if (how_old - as.Date(file.info("covid_19_UT_data.feather")$ctime ) >= 1){
    cache_covid_19_UT_data()
  }
  read_covid_19_UT_data()
}

#############################
# Make an age histogram
get_age_hist <- function()
{
  df <- get_covid_19_UT_data()
  df$fake_age <- mapply(function(x, y) sample(seq(x, y), 1), 
                             df$min_age, ifelse(is.na(df$max_age), 0 ,df$max_age))
  
  df$fake_age <- ifelse(df$age == "Not Reported", NA, df$fake_age)
  df$ALL_TRUE <- 1
  df <- df %>% mutate(Age_is_reported = ifelse(is.na(fake_age),"Not Reported","Repoted"))
  df %>% dplyr::count(age)
  bins <- tibble(min_bin = seq(0,120, 10), max_bin = seq(10,130, 10))
  
   
  
  df_p<- sqldf ("
SELECT b.* , df.*
FROM bins AS b
LEFT JOIN df  ON
    b.min_bin <= df.fake_age AND
    b.max_bin > df.fake_age 
") %>% 
    mutate(bin_lbl = paste0(min_bin, "-", max_bin)) %>% 
    mutate(bin_lbl = fct_reorder(bin_lbl, min_bin)) %>% 
    filter(!is.na(ALL_TRUE)) %>% 
    as_tibble()
  
    max_date <- df_p$date_report %>% max() %>% as.character()
    p <- df_p %>% ggplot(aes(x = bin_lbl, fill = sex)) + 
      geom_histogram(alpha = 0.5, stat="count", color = "black") + 
      coord_flip() + 
      facet_grid(cols = vars(sex)) +
      #scale_color_manual(values=c("#ffc0cb", "#add8e6", "#808080")) + 
      scale_fill_manual(values=c("#ffc0cb", "#add8e6", "#808080")) 
    p <-
    p + labs(title = paste0("Covid-19 age histogram for Canada" , "\nLast date of data = ", max_date), 
             x = "age bin", 
             y = "count", 
             caption = get_covid_19_UT_citation()) + 
      theme_bw()
    p
    
}


##############################
# TODO :Get a Pie of the not reported Ages
get_age_reported_pie <- function(){
  df <- get_covid_19_UT_data()
  df$fake_age <- mapply(function(x, y) sample(seq(x, y), 1), 
                        df$min_age, ifelse(is.na(df$max_age), 0 ,df$max_age))
  
  df$fake_age <- ifelse(df$age == "Not Reported", NA, df$fake_age)
  df$ALL_TRUE <- 1
  df <- df %>% mutate(Age_is_reported = ifelse(is.na(fake_age),"Not Reported","Repoted"))  
  #ggplot(aes(x=Tests/2, y = value, fill = name, width = Tests)) +  
  p <- 
    df %>% 
    count(Age_is_reported, sex) %>% 
    group_by(sex) %>% mutate(N_sex = sum(n)) %>% ungroup() %>% 
    ggplot(aes(x=N_sex/2, y=n, fill=Age_is_reported, color=Age_is_reported )) + 
      geom_col() + coord_polar("y", start=0) + theme_void() +
      geom_text(aes(y = n/3 + c(0, cumsum(n)[-length(n)]), 
                    label = n), size=5) + 
      facet_grid(cols = vars(sex))
      labs(title = paste0("Reporting of age for Covid-19 in Canada","\nLast date of data = ", max_date), 
           fill = "", 
           caption = get_covid_19_UT_citation()) +
      theme(legend.position="bottom")
    p
}

#TODO Make the pie and the hisogram go togeather
#plot_grid(get_age_reported_pie() + labs(title = ""), get_age_hist(), labels = c("Age of Covid-19 victims in Canada, ", ""))
    # anim <- p + 
    #   transition_reveal(date_repor)
    # 
    # animate(anim, nframes = 200, renderer = gifski_renderer("covid_19_age_hist.gif", width = 1024, height = 768), end_pause = 100)
    # 
