library(tidyverse)
library(rvest)
library(httr)

get_info_player <- function(url, player_name, year) {
  url_redirect <- GET(url)$url
  
  url_parse <- read_html(paste0(substr(url_redirect, 1, nchar(url_redirect)-8), "/gamelogs?season=", year))
    
  table_stats <- url_parse %>% 
    html_nodes("table") %>% 
    html_table()
  
  ## Get Regular Season Table
  print(player_name)
  reg_stats <- table_stats[[2]]
  
  ## Reove Meta Data
  reg_stats <- reg_stats[-1, ]
  
  ## Remove lines with NA or blank space
  reg_stats <- reg_stats[!(is.na(reg_stats$X1) | reg_stats$X1==""), ]
  
  ## Change names of columns
  colnames(reg_stats) = reg_stats[1, ] 
  reg_stats = reg_stats[-1, ]
  
  ## Add player's name and season
  reg_stats$player = player_name
  reg_stats$year = year
  
  return(reg_stats)
}

get_all_players_season <- function(url, year) {
  players_page <- read_html(url)
  
  ## Get urls of player's profile
  players_url <- players_page %>% 
    html_nodes("td") %>% 
    html_nodes("a") %>% 
    html_attr("href") %>% 
    matrix(ncol = 2, byrow = TRUE) %>% 
    as.data.frame() %>% 
    select("url" = V1)
  
  ## Get info about player's season
  players_name <- players_page %>% 
    html_nodes("table") %>% 
    html_table()
  
  players_name <- players_name[[1]]
  
  ## Player info season
  players_info <- players_name %>% 
    mutate(Yds = as.numeric(gsub(",", "", Yds))) %>% 
    cbind(players_url) %>% 
    mutate(year = year) %>% 
    select(-Rk)
  
  return(players_info)
}

## Get data of every player in a list of players (player name, player url)
get_data_week_by_week <- function(players_list, year) {
  list_url <- player_list$url
  list_player <- player_list$Player
  
  data_wbw <- get_info_player(paste0("http://www.nfl.com", list_url[1]), list_player[1], year)
  
  for (i in 2:length(list_url)) {
    player_info <- get_info_player(paste0("http://www.nfl.com", list_url[i]), list_player[i], year)
    
    data_wbw <- data_wbw %>% 
      rbind(player_info)
  }
  
  return(data_wbw)
}

## List of players with passing stats according with NFL
list_qbs1 <- "http://www.nfl.com/stats/categorystats?tabSeq=0&season=2017&seasonType=REG&Submit=Go&experience=&archive=true&d-447263-p=1&statisticCategory=PASSING&conference=null&qualified=false"
list_qbs2 <- "http://www.nfl.com/stats/categorystats?tabSeq=0&season=2017&seasonType=REG&experience=&Submit=Go&archive=true&conference=null&statisticCategory=PASSING&d-447263-p=2&qualified=false"

players_list_page1 <- get_all_players_season(list_qbs1, 2017)
players_list_page2 <- get_all_players_season(list_qbs2, 2017)

# Only QB will be searched
players_list <- players_list_page1 %>% 
  rbind(players_list_page2) %>% 
  filter(Pos == "QB")

# Get data week by week (only regular season)
players_wbw <- get_data_week_by_week(players_list %>% select(Player, url), 2017)

## Write csvs
write.csv(players_list, "player_list.csv", row.names = FALSE)
write.csv(players_wbw, "players_wbw.csv", row.names = FALSE)
