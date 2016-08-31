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
  as.data.frame(stringsAsFactors = FALSE) %>% 
  setNames("link") %>% 
  # get sport and event explicitly
  mutate(
    sport = sub(x = link, ".*results/sports/(.+)/(.+)", "\\1"),
    event = sub(x = link, ".*results/sports/(.+)/(.+)", "\\2")
  )

# 31 events
unique(events$sport)

knockout.results <- c("archery", "badminton", "basketball", 
                      "boxing", "fencing", "handball", "hockey", 
                      "judo", "rugby-sevens", "table-tennis", "taekwondo", 
                      "tennis", "volleyball", "water-polo", "wrestling")






#sw <- "http://www.bbc.com/sport/olympics/rio-2016/results/sports/swimming/womens-800m-freestyle"

f.get.bbc.nonknockout.results <- function(link){
  dta <- read_html(link) %>%
    # get first <div> after <h3>Final</h3>
    html_node(xpath = "//h3[contains(., 'Final')]/following-sibling::div") %>% 
    # get table from div
    html_node("table") %>% 
    # convert into dataframe
    html_table() %>% 
    mutate(
      sport = events[events$link == link, "sport"],
      event = events[events$link == link, "event"]
    )
}


dta <- list()

for(i in events %>% filter(!sport %in% knockout.results) %>% .$link){
  dta[[i]] <- f.get.bbc.nonknockout.results(i)
}







