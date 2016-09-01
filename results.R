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



f.get.bbc.nonknockout.results <- function(link){
  dta <- read_html(link) %>%
    # get first <div> after <h3>Final</h3>
    #html_node(xpath = "//h3[contains(., 'Final') or contains(., 'Results')]/following-sibling::div") %>% 
    # http://stackoverflow.com/questions/8808921/selecting-a-css-class-with-xpath
    # http://dubinko.info/blog/2007/10/01/simple-parsing-of-space-seprated-attributes-in-xpathxslt/
    # first h3 element of class = heading, pick the div that follows
    #html_node(xpath = "(//h3[contains(concat(' ', normalize-space(@class), ' '), ' heading ')])[1]/following-sibling::div") %>% 
    html_node(xpath = "(//div[contains(concat(' ', normalize-space(@class), ' '), ' table-container ')])[1]") %>% 
    # get table from div
    html_node("table") %>% 
    # convert into dataframe, header = FALSE because of mens-synchronised-3m-springboard :(
    # fill = TRUE because of synchronised-swimming/duets
    html_table(header = FALSE, fill = TRUE) %>% 
    mutate(
      sport = events[events$link == link, "sport"],
      event = events[events$link == link, "event"]
    )
}



# non-knockout events (easier to calculate ranks)
nonknockouts <- events %>% 
  # manual list of knockout round sports
  filter(!sport %in% knockout.results) %>% 
  # these seem to have have knockout rounds
  filter(!(sport == "cycling" & event %in% c("mens-sprint", "womens-sprint", "mens-team-pursuit", "womens-team-pursuit"))) %>% 
  # these tables are empty :(
  filter(!sport == "football") 

dta.ll <- list()

for(i in seq.int(nrow(nonknockouts))){
  dta.ll[[i]] <- f.get.bbc.nonknockout.results(nonknockouts[i, "link"])
}

save(dta.ll, file = "bbc-nonknockouts.Rdata")

dta.raw <- lapply(dta.ll, function(df){
  # get first row as header names
  header <- df[1,]
  # rename last two columns to "sport", "event"
  l <- length(header)
  header[(l-1):l] <- c("sport", "event")
  # make R-safe column names, remove first row
  setNames(df[-1,], make.names(header))
})

# (roughly) get all versions of each column name (eg "Rank", "Ranking", "Position")
# nn <- bind_rows(lapply(dta.raw, function(x){as.data.frame(t(names(x)))}))
# apply(nn, 2, unique)

# check lapply(dta.raw, names) for name overview... 
# http://stackoverflow.com/questions/34275576/avoiding-error-when-using-rename-in-dplyr-and-column-doesnt-exist
namekey <- c(
  Rank = "rank", Ranking = "rank", Position = "rank",
  Country = "country", Nation = "country",
  Name = "names", Names = "names", Athlete = "names", Athletes = "names", 
  # many, many different result variables... :( 
  Result = "result", Results = "result", Total = "result",
  Time = "result", Points = "result", Distance..m. = "result", Height = "result", Distance = "result", 
  Height..m. = "result", Throw..m. = "result", Score = "result", Pts = "result", 
  Final.score = "result", Net.points = "result", Final.points = "result", Overall.time = "result",
  # phew...
  sport = "sport", event = "event"
)

dta.nk <- lapply(dta.raw, function(df){
  names(df) <- namekey[names(df)]
  return(df)
})
