rm(list = ls()); gc(); gc()
options(bitmapType='cairo')
options(scipen = 999)

library(ggplot2)
library(rvest)
library(dplyr)
#library(tidyr)

# Define your workspace: "X:/xxx/"
wd <- path.expand("~/Documents/github/olympia")
setwd(wd)

# pull country medals data for a specified Olympic event (1 = 1896 Athens)
f.gen.olympic.data <- function(event = "1"){
  country.medals.url.base <- "http://www.theolympicdatabase.nl/countries.php?reset=1&event_id="
  country.medals.url <- paste0(country.medals.url.base, event)
  
  medals.web <- read_html(country.medals.url)
  
  # get full event name
  event <- medals.web %>% 
    html_nodes("h3 b") %>% 
    html_text()
  
  # get year of event: remove everything not a digit
  year <- gsub("[^[:digit:]]", "", event)
  
  # summer or wintergames
  type <- gsub(".* ([a-zA-Z]+games).*", "\\1", event)
  
  dta <- medals.web %>% 
    # class="datagrid_header_table"
    html_nodes("table .datagrid_header_table") %>% 
    # only one table, so could also unlist()
    .[[1]] %>% 
    html_table(header = TRUE) %>% 
    # make safe R column names: medal columns start with a space... ' Gold' :(
    setNames(gsub("X.", "", make.names(names(.), unique = TRUE))) %>% 
    select(Country, Gold, Silver, Bronze) %>% 
    mutate(
      year = year,
      type = type, 
      event = event
    )
  
  return(dta)  
}


# get medals data
medals <- as.data.frame(NULL)


# 19 Summergames London 1948

# no data for 15-16-17
# 17 1944 Summer London
# 16 1940 Summer Tokyo
# 15 1940 Winter Garmisch-Partenkirchen

# 14 1936 Summer Berlin
# 13 1936 Winter Garmisch-Partenkirchen
games <- seq.int(49)
games <- games[!games %in% c(15, 16, 17)]

# run loop
for(i in games){
  medals <- bind_rows(medals, f.gen.olympic.data(i))
}








