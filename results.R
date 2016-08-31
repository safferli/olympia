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


bbc.rio2016 <- "http://www.bbc.com/sport/olympics/36373149"

events <- read_html(bbc.rio2016) %>% 
  html_nodes("a") %>% 
  html_attr("href") %>% 
  # get only links to the rio2016 results pages
  .[grepl(x = ., pattern = "/sport/olympics/rio-2016/results/sports")] %>%
  # turn into full bbc link
  paste0("http://www.bbc.com", .) %>% 
  # turn into proper dataframe
  as.data.frame() %>% 
  setNames("link") %>% 
  # get sport and event explicitly
  mutate(
    sport = sub(x = link, ".*results/sports/(.+)/(.+)", "\\1"),
    event = sub(x = link, ".*results/sports/(.+)/(.+)", "\\2")
  )

# 31 events
unique(events$sport)



sw <- "http://www.bbc.com/sport/olympics/rio-2016/results/sports/swimming/womens-800m-freestyle"
sw.html <- read_html(sw)

dta <- sw.html %>%
  # get first <div> after <h3>Final</h3>
  html_node(xpath = "//h3[contains(., 'Final')]/following-sibling::div") %>% 
  # get table from div
  html_node("table") %>% 
  # convert into dataframe
  html_table()


