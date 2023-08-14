library(tidyverse)
library(rvest)
library(magrittr)
library(stringr)
library(ggplot2)
library(lubridate)
library(mlbplotR)
library(stringdist)


nl_west_teams <- c('Arizona Diamondbacks', 'Colorado Rockies', 'San Diego Padres', 'San Fransisco Giants', 'Los Angeles Dodgers')
nl_central_teams <- c('Chicago Cubs', 'Cincinnati Reds', 'Milwaukee Brewers', 'Pittsburgh Pirates', 'St. Louis Cardinals')
nl_east_teams <- c('Atlanta Braves', 'Miami Marlins', 'Philadelphia Phillies', 'New York Mets', 'Washington Nationals')
al_west_teams <- c('Houston Astros', 'Los Angeles Angels', 'Oakland Athletics', 'Seattle Mariners', 'Texas Rangers')
al_central_teams <- c('Chicago White Sox', 'Cleveland Guardians', 'Detroit Tigers', 'Kansas City Royals', 'Minnesota Twins')
al_east_teams <- c('Baltimore Orioles', 'Boston Red Sox', 'New York Yankees', 'Tampa Bay Rays', 'Toronto Blue Jays')

all_teams <- c(nl_west_teams, nl_central_teams, nl_east_teams, al_west_teams, al_central_teams, al_east_teams)

nl_west_abbs <- c('ARI', 'COL', 'SD', 'SF', 'LAD')
nl_central_abbs <- c('CHC', 'CIN', 'MIL', 'PIT', 'STL')
nl_east_abbs <- c('ATL', 'MIA', 'PHI', 'NYM', 'WAS')
al_west_abbs <- c('HOU', 'LAA', 'OAK', 'SEA', 'TEX')
al_central_abbs <- c('CHW', 'CLE', 'DET', 'KC', 'MIN')
al_east_abbs <- c('BAL', 'BOS', 'NYY', 'TB', 'TOR')

nl_abbs <- c(nl_west_abbs, nl_central_abbs, nl_east_abbs)
al_abbs <- c(al_west_abbs, al_central_abbs, nl_east_abbs)

all_abbs <- c(nl_west_abbs, nl_central_abbs, nl_east_abbs, al_west_abbs, al_central_abbs, al_east_abbs)



make_url <- function(team){
  team_name <- team %>%  tolower() %>% str_replace_all("[[:punct:]]", "")
  team_name <- gsub(" ", "-", team_name)
  team_abb <- all_abbs[match(team, all_teams)] %>% tolower() 
  
  url1 <- paste0('https://www.espn.com/mlb/team/schedule/_/name/', team_abb, '/seasontype/2/half/1')
  url2 <- paste0('https://www.espn.com/mlb/team/schedule/_/name/', team_abb, '/seasontype/2/half/2')
  return(c(url1, url2, team_abb))
}

get_schedule <- function(team){
  url_abb <- make_url(team)
  link1 <- url_abb[1] #link to first half
  link2 <- url_abb[2] #link to second half
  abb <- toupper(url_abb[3])
  schedule1 <- (read_html(link1) %>% html_nodes('table') %>% html_table(header = TRUE))[[1]]
  schedule2 <- (read_html(link2) %>% html_nodes('table') %>% html_table(header = TRUE))[[1]]
  schedule <- rbind(schedule1, schedule2)
  schedule %<>% mutate(TEAM = abb, .after = 'DATE')
  return(schedule)
}

schedule <- get_schedule('Chicago Cubs')


schedule


drop_future_games <- function(sched) {
  #sched <- sched[[1]] #this grabs the schedule from the team (two level list)
  bad_row <- which(sched$DATE == 'DATE' & sched$OPPONENT == 'OPPONENT' & sched$RESULT == 'TIME') #this row indicates the following games havennt occured
  if(bad_row %>% length > 0){
    good_sched <- sched[-(bad_row:nrow(sched)),] #drops relabeling row and following games
  }
  else{
    good_sched <- sched
  }
  ppd_rows <- which(good_sched$RESULT == 'Postponed')
  if(ppd_rows %>% length > 0){
    good_sched <- good_sched[-ppd_rows,]
  }
  return(good_sched)
}
#lapply(all_schedules, drop_future_games) #WORKS


###home and away more readable
who_where <- function(sched){
  #sched <- sched[[1]]
  sched %<>% add_column(WHERE = NA, .before = 'OPPONENT') #empty column to assign 'Home' and 'Away' to
  #populating where column
  away_idxs <- which(sched$OPPONENT %>% substr(1, 1) == '@')
  sched$WHERE[away_idxs] <- 'Away'
  home_idxs <- which(sched$OPPONENT %>% substr(1, 1) == 'v')
  sched$WHERE[home_idxs] <- 'Home' 
  sched[75:76,]$WHERE <- 'Away' #london games
  
  #removing these indicators from opponent names
  sched$OPPONENT %<>% str_replace_all(c('@' = '', 'vs' = '', ' \\*' = ''))
  good_sched <- sched
  return(good_sched)
}
#lapply(all_schedules, who_where) #WORKS



##need to separate scores
score_clean <- function(sched){
  #sched <- sched[[1]]
  good_sched <- separate(sched, RESULT, c('RESULT', 'SCORE'), sep = cumsum(c(1, 10)))
  good_sched %<>% separate(SCORE, c('WINNINGSCORE', 'LOSINGSCORE'), sep = '-') #splits scores into two columns
  good_sched %<>% add_column(EXTRAS = NA, .after = 'LOSINGSCORE')
  extra_idxs <- good_sched$LOSINGSCORE %>% grepl(' F/', .) %>% which() #finds index of rows with extra innings
  good_sched$EXTRAS[-extra_idxs] <- 'N' #extras col full of N for games which finished in 9
  where_fs <- good_sched$LOSINGSCORE %>% gregexpr(' F/', .) %>% unlist() #indexes in score strings where F/XX appears
  good_sched$EXTRAS[extra_idxs] <- good_sched$LOSINGSCORE[extra_idxs] %>% substr(where_fs[extra_idxs] + 1, str_length(.))
  good_sched$LOSINGSCORE[extra_idxs] %<>% substr(1, where_fs[extra_idxs] - 1)
  
  #swapping scores for losses so we can track team score vs opp score rather than high score and low score
  win_scores <- good_sched[good_sched$RESULT == 'L',]$WINNINGSCORE
  good_sched[good_sched$RESULT == 'L',]$WINNINGSCORE <- good_sched[good_sched$RESULT == 'L',]$LOSINGSCORE
  good_sched[good_sched$RESULT == 'L',]$LOSINGSCORE <- win_scores
  
  good_sched %<>% rename(TEAMSCORE = WINNINGSCORE) ### RENAMING ###
  good_sched %<>% rename(OPPSCORE = LOSINGSCORE)   ###   COLUMNS ####
  
  #making wins and loss columns separate and adding pct column
  good_sched %<>% separate(`W-L`, c('WINS', 'LOSSES'), sep = '-') #separates win/losses into their own columns
  good_sched$WINS %<>% as.integer
  good_sched$LOSSES %<>% as.integer
  good_sched %<>% mutate(PCT = WINS / (WINS + LOSSES), .before = 'WIN')
  
  good_sched$TEAMSCORE %<>% as.integer()
  good_sched$OPPSCORE %<>% as.integer()
  good_sched$ATT %<>% gsub(',', '', .) %<>% as.integer()
  return(good_sched)
}




#lapply(all_schedules, score_clean) #WORKS


get_clean_schedules <- function(schedule_ls){
  played_games <- lapply(all_schedules, drop_future_games)
  with_where <- lapply(played_games, who_where)
  clean <- lapply(with_where, score_clean)
  return(clean)
}



schedule %<>% drop_future_games()
schedule %<>% who_where()
schedule %<>% score_clean()
schedule$DATE %<>% as.Date("%a, %b %d")
schedule$OPPONENT[12:14] <- 'Los Angeles Dodgers'
schedule$OPPONENT[15:17] <- 'Oakland Athletics'
schedule$OPPONENT[18:21] <- 'Los Angeles Dodgers'
schedule$OPPONENT[28:31] <- 'Washington Nationals'
schedule$OPPONENT[35:37] <- 'St Louis Cardinals'
schedule$OPPONENT[44:46] <- 'Philadelphia Phillies'
schedule$OPPONENT[47:49] <- 'New York Mets'
schedule$OPPONENT[60:62] <- 'Los Angeles Angels'
schedule$OPPONENT[75:76] <- 'St Louis Cardinals'
schedule$OPPONENT[77:79] <- 'Philadelphia Phillies'
schedule$OPPONENT[87:89] <- 'New York Yankees'
schedule$OPPONENT[93:95] <- 'Washington Nationals'
schedule$OPPONENT[96:99] <- 'St Louis Cardinals'
schedule$OPPONENT[100:101] <- 'Chicago White Sox'
schedule$OPPONENT[102:105] <- 'St Louis Cardinals'




schedule$OPPONENT <- all_abbs[amatch(schedule$OPPONENT, all_teams, maxDist = Inf)]
schedule$OPPONENT

#makingurls to get the box scores
years <- schedule$DATE %>% strftime('%Y')
months <- schedule$DATE %>% strftime('%m')
days <- schedule$DATE %>% strftime('%d') %>% as.character()


wheregames <- case_when(schedule$WHERE == 'Away' ~ paste0(schedule$TEAM, '@', schedule$OPPONENT),
                        schedule$WHERE == 'Home' ~ paste0(schedule$OPPONENT, '@', schedule$TEAM))

box_urls <- paste0('https://www.cbssports.com/mlb/gametracker/preview/MLB_', years, months, days, '_', wheregames, '/')

#test <- read_html(box_urls[1]) %>% html_nodes('table') %>% html_table() 

batting_ls <- list()
pitcher_ls <- list()
for(i in 1:length(box_urls)){
  boxes <- read_html(box_urls[i]) %>% html_nodes('table') %>% html_table() 
  if(schedule$WHERE[i] == 'Away'){
    cubs_bats <- boxes[[2]] %>% mutate(DATE = schedule$DATE[i], .before = HITTERS)
    opp_pitchers <- boxes[[8]]  %>% mutate(DATE = schedule$DATE[i], .before = PITCHERS)
  }
  else{
    cubs_bats <- boxes[[4]]  %>% mutate(DATE = schedule$DATE[i], .before = HITTERS)
    opp_pitchers <- boxes[[6]] %>% mutate(DATE = schedule$DATE[i], .before = PITCHERS)
  }
  cubs_bats$HR %<>% as.integer()
  batting_ls[[i]] <- cubs_bats
  pitcher_ls[[i]] <- opp_pitchers
  print(i)
}



names(batting_ls) <- schedule$DATE
names(pitcher_ls) <- schedule$DATE

batting_ls[[1:19]]$HR %>% typeof()

pitcher_ls$`2023-07-26` %>% mutate(date = '2023-07-26')

box_urls[75]

schedule[75:76,] <- 'Away'
for(i in 1:length(batting_ls)){
  
  batting_ls[[i]]$FPTS %<>% as.integer()
}

starting_pitchers <- bind_rows(pitcher_ls) %>% group_by(DATE) %>% slice(1)
batters <- bind_rows(batting_ls)
#drop rows of non starters

starters <- batters[-which(batters$HITTERS %>% substr(2,2) == '-'),] #this drops all subs who had plate appearance
starters <- starters[!(starters$AB == 0),] #drops defensive subs with no at bat
starters$HITTERS %<>% sub("^(\\S*\\s+\\S+).*", "\\1", .)  

wide_hits <- (starters[, c('DATE', 'HITTERS', 'H')] %>% group_by(DATE) %>% group_by(HITTERS) %>% filter(H > 0)) %>% pivot_wider(names_from = HITTERS, values_from = H)

wide_hits %>% cor()

(wide_hits[,2:ncol(wide_hits)] %>% cor()) %>% min()
wide_hits[is.na(wide_hits)] <- 0
wide_hits
