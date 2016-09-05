rm(list = ls()); gc(); gc()
options(bitmapType='cairo')
options(scipen = 999)

#library(ggplot2)
library(rvest)
library(dplyr)
library(tidyr)

# Define your workspace: "X:/xxx/"
wd <- path.expand("~/Documents/github/olympia")
setwd(wd)


if(!file.exists("bbc-nonknockouts.Rdata")){
  # base BBC sports Rio 2016 page
  bbc.rio2016 <- "http://www.bbc.com/sport/olympics/36373149"
  
  # get DF of event links
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
  #unique(events$sport)
  
  knockout.results <- c("archery", "badminton", "basketball", 
                        "boxing", "fencing", "handball", "hockey", 
                        "judo", "rugby-sevens", "table-tennis", "taekwondo", 
                        "tennis", "volleyball", "water-polo", "wrestling")
  
  # non-knockout events (easier to calculate ranks)
  nonknockouts <- events %>% 
    # manual list of knockout round sports
    filter(!sport %in% knockout.results) %>% 
    # these seem to have have knockout rounds
    filter(!(sport == "cycling" & event %in% c("mens-sprint", "womens-sprint", "mens-team-pursuit", "womens-team-pursuit"))) %>% 
    # these tables are empty :(
    filter(!sport == "football") 
  
  # the non-nonknockouts are thus the knockouts :) 
  knockouts <- dplyr::setdiff(events, nonknockouts)

  
  ##
  ## download non-knockout results
  ## 
  
  f.get.bbc.nonknockout.data <- function(link){
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
  
  ## actual download loop here
  dta.nk.ll <- list()
  for(i in seq.int(nrow(nonknockouts))){
    dta.nk.ll[[i]] <- f.get.bbc.nonknockout.data(nonknockouts[i, "link"])
  }
  
  ##
  ## download knockout results
  ##
  
  # get knockout results - top 8
  f.get.bbc.knockout.data <- function(link){
    tdta <- read_html(link) %>%
      #html_node(xpath = "//*[contains(concat(' ', normalize-space(@class), ' '), ' layout__ghost-column '))]") %>% 
      html_table(fill = TRUE) 
    # make R-safe column names (Name+Country headers are duplicated)
    tdta <- lapply(tdta, function(df){
      setNames(df, make.names(names(df), unique = TRUE))
    })
    # get finals, bronze medal match, semi- and quarter-finals (top 8 players)  
    bind_rows(tdta[1:4]) %>% 
      mutate(
        sport = events[events$link == link, "sport"],
        event = events[events$link == link, "event"]
      )
  }
  
  ## actual knockout data loop
  dta.ko.ll <- list()
  for(i in seq.int(nrow(knockouts))){
    dta.ko.ll[[i]] <- f.get.bbc.knockout.data(knockouts[i, "link"])
  }
  
  
  
  
  
  # save data to avoid re-pulling
  save(events, dta.nk.ll, dta.ko.ll, knockouts, nonknockouts, file = "bbc-olympics data.Rdata")
} else {
  load("bbc-olympics data.Rdata")
}

##
## clean non-knockout data
##

dta.nk.raw <- lapply(dta.nk.ll, function(df){
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

dta.nk <- lapply(dta.nk.raw, function(df){
  # rename columns according to namekey
  names(df) <- namekey[names(df)]
  # some NA column names left, clean these up
  df <- setNames(df, make.names(names(df), unique = TRUE))
  # bind_rows chokes on chr and num later... with these columns; make them chr now
  for(i in paste0("NA..", 1:6)){
    if(any(grepl(i, names(df)))){
      df[, i] <- as.character(df[, i])
    }
  }
  # make first column (rank) always chr; some have "Gold" as rank 1
  df[, 1] <- as.character(df[, 1])
  return(df)
})

##
## big list of manual checking/fixing
## 

# 86-91 (horses)
# 105, 125, 160, 187
# + find out which DF does not have a "result" variable: 
# dta.nk[which(
#   sapply(dta.nk, function(df){
#     !any(grepl("result", names(df)))
#   })
# )]

# athletics/womens-hammer-throw
dta.nk[[45]] <- setNames(dta.nk[[45]], c("rank", "country", "names", "result", "sport", "event"))
# diving/mens-synchronised-3m-springboard
dta.nk[[80]] <- setNames(dta.nk[[80]], c("rank", "country", "names", paste0("V", 1:6), "result", "sport", "event"))
# equestrian/eventing-team
dta.nk[[86]] <- setNames(dta.nk[[86]], c("rank", "country", "V1", "V2", "result", "sport", "event"))
# equestrian/jumping-team
dta.nk[[90]] <- setNames(dta.nk[[90]], c("rank", "country", "result", "sport", "event"))
# gymnastics/womens-team lost it's first row because the table has no headers
dta.nk[[105]] <- setNames(dta.nk[[105]], c("rank", "country", "result", "sport", "event"))
dta.nk[[105]] <- bind_rows(
  # manually create first row again
  data.frame(rank = "1", country = "United States", result = "184.897", sport = "gymnastics", event = "womens-team", stringsAsFactors = FALSE),
  dta.nk[[105]]
)
# rowing/womens-quadruple-sculls
dta.nk[[125]] <- setNames(dta.nk[[105]], c("rank", "country", "result", "sport", "event"))
# swimming/womens-200m-breaststroke
dta.nk[[160]] <- setNames(dta.nk[[160]], c("rank", "names", "country", "result", "sport", "event"))
# synchronised-swimming/duets
dta.nk[[187]] <- setNames(dta.nk[[187]], c("rank", "country", "names", "V1", "V2", "result", "points.behind", "sport", "event"))

#test <- dta.nk[[112]]

##
## manual check and clean-up finished
##



##
## clean knockout data
##

# returns the index of the larger element of a result of the format "a-b"
# e.g. "2" if the result is "1-2"
f.get.winner.index <- function(result){
  # gsub("(\\d)\\-(\\d)", paste0("\\", num), x)
  which.max(unlist(strsplit(result, split = "-")))
}

# build clean ranking of the ko-results (places 5-8 are tied to 5)
f.clean.knockout.results <- function(df){
  df <- df %>% 
    #tidyr::separate(Result, c("left.result", "right.result"), sep = "-", remove = FALSE) %>% 
    mutate(
      left.name = paste(Name, Country, sep = ", "),
      right.name = paste(Name.1, Country.1, sep = ", ")
    )  
  
  # finals
  winners <- c(df[1, "left.name"], df[1, "right.name"])
  # if right.name won, switch places in winners vector 
  if(f.get.winner.index(df[1, "Result"]) == 2){
    winners[c(1, 2)] <- winners[c(2, 1)]
  }
  
  # "loser's final"; match for 3rd place
  winners <- c(winners, c(df[2, "left.name"], df[2, "right.name"]))
  # if right.name won, switch places
  if(f.get.winner.index(df[2, "Result"]) == 2){
    winners[c(3, 4)] <- winners[c(4, 3)]
  }
  
  # places 5-8: all 8, minus the winners
  participants <- unique(c(df$left.name, df$right.name))
  losers <- participants[!participants %in% winners]
  
  ko <- data.frame(
    participants = c(winners, losers), 
    rank = c(1:4, rep(5, 4)), 
    sport = df[1, "sport"], 
    event = df[1, "event"],
    stringsAsFactors = FALSE
  ) %>% 
    tidyr::separate(participants, into = c("names", "country"), sep = ", ")
  
  return(ko)
}


## 
## combine to get full data
## 

# non-knockout round data finally! :) 
dta <- bind_rows(dta.nk) %>% 
  select(
    sport, event, rank, country, result, names, everything()
  ) %>% 
  mutate(
    # TODO: for some reason tons of ws left
    rank = if_else(trimws(rank) == "Gold", "1", trimws(rank)), 
    rank = if_else(rank == "Silver", "2", rank),
    rank = if_else(rank == "Bronze", "3", rank)
  )




