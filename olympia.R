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

country.medals.url <- "http://www.theolympicdatabase.nl/countries.php?reset=1&event_id=1"

medals.web <- read_html(country.medals.url)


dta <- medals.web %>% 
  # class="datagrid_header_table"
  html_nodes("table .datagrid_header_table") %>% 
  # only one table, so could also unlist()
  .[[1]] %>% 
  html_table(header = TRUE) %>% 
  # make safe R column names: medal columns start with a space... ' Gold' :(
  setNames(gsub("X.", "", make.names(names(.), unique = TRUE))) %>% 
  select(Country, Gold, Silver, Bronze)
  
  





