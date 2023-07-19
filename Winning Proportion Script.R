library(tidyverse)
library(rvest)
library(magrittr)
library(stringr)
library(ggplot2)
library(lubridate)
library(mlbplotR)

###defining all teams and their abbreviations
make_teams_ls <- function(){
  nl_west_teams <- c('Arizona Diamondbacks', 'Colorado Rockies', 'San Diego Padres', 'San Fransisco Giants', 'Los Angeles Dodgers')
  nl_central_teams <- c('Chicago Cubs', 'Cincinnati Reds', 'Milwaukee Brewers', 'Pittsburgh Pirates', 'St. Louis Cardinals')
  nl_east_teams <- c('Atlanta Braves', 'Miami Marlins', 'Philadelphia Phillies', 'New York Mets', 'Washington Nationals')
  al_west_teams <- c('Houston Astros', 'Los Angeles Angels', 'Oakland Athletics', 'Seattle Mariners', 'Texas Rangers')
  al_central_teams <- c('Chicago White Sox', 'Cleveland Guardians', 'Detroit Tigers', 'Kansas City Royals', 'Minnesota Twins')
  al_east_teams <- c('Baltimore Orioles', 'Boston Red Sox', 'New York Yankees', 'Tampa Bay Rays', 'Toronto Blue Jays')

  all_teams <- c(nl_west_teams, nl_central_teams, nl_east_teams, al_west_teams, al_central_teams, al_east_teams)
  return(all_teams)
}

make_abbs_ls <- function(){
  nl_west_abbs <- c('ARI', 'COL', 'SD', 'SF', 'LAD')
  nl_central_abbs <- c('CHC', 'CIN', 'MIL', 'PIT', 'STL')
  nl_east_abbs <- c('ATL', 'MIA', 'PHI', 'NYM', 'WSH')
  al_west_abbs <- c('HOU', 'LAA', 'OAK', 'SEA', 'TEX')
  al_central_abbs <- c('CHW', 'CLE', 'DET', 'KC', 'MIN')
  al_east_abbs <- c('BAL', 'BOS', 'NYY', 'TB', 'TOR')
  
  all_abbs <- c(nl_west_abbs, nl_central_abbs, nl_east_abbs, al_west_abbs, al_central_abbs, al_east_abbs)
  return(all_abbs)
}



###defining function to create link to schedule on espn from team name
make_url <- function(team){
  team_name <- team %>%  tolower() %>% str_replace_all("[[:punct:]]", "")
  team_name <- gsub(" ", "-", team_name)
  team_abb <- all_abbs[match(team, all_teams)] %>% tolower() 
  
  url1 <- paste0('https://www.espn.com/mlb/team/schedule/_/name/', team_abb, '/seasontype/2/half/1')
  url2 <- paste0('https://www.espn.com/mlb/team/schedule/_/name/', team_abb, '/seasontype/2/half/2')
  return(c(url1, url2, team_abb))
}


###defining function to get schedules as tables from urls
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

#making all of the urls and storing in a list
#url_ls <- lapply(all_teams, make_url) %>% unlist()
#url_ls



#######need to clean up a little bit########

###first, we need to get rid of games that haven't happened yet and those which have been postponed

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

loc_wins <- function(sched){
  team <- sched$TEAM[1]
  games_played <- nrow(sched)
  wins <- (sched$RESULT == 'W') %>% sum()
  losses <- (sched$RESULT == 'L') %>% sum()
  home_games <- (sched$WHERE == 'Home') %>% sum()
  home_wins <- (sched[sched$WHERE == 'Home', ]$RESULT == 'W') %>% sum()
  home_losses <- (sched[sched$WHERE == 'Home', ]$RESULT == 'L') %>% sum()
  away_games <- (sched$WHERE == 'Away') %>% sum()
  away_wins <- (sched[sched$WHERE == 'Away', ]$RESULT == 'W') %>% sum()
  away_losses <- (sched[sched$WHERE == 'Away', ]$RESULT == 'L') %>% sum()
  run_diff <- sched$TEAMSCORE %>% sum() - sched$OPPSCORE %>% sum()
  home_run_diff <- (sched[sched$WHERE == 'Home', ]$TEAMSCORE %>% sum()) - (sched[sched$WHERE == 'Home', ]$OPPSCORE %>% sum())
  away_run_diff <- (sched[sched$WHERE == 'Away', ]$TEAMSCORE %>% sum()) - (sched[sched$WHERE == 'Away', ]$OPPSCORE %>% sum())
  
  summary_vec <- c(team, games_played, wins, losses, home_games, home_wins,
                   home_losses, away_games, away_wins, away_losses, run_diff, home_run_diff, away_run_diff)
  return(summary_vec)
  
}
#loc_wins(exp_ls$CHC) #WORKS

plottable_data <- function(){
  all_teams <- make_teams_ls()
  all_abbs <- make_abbs_ls()
  ###list of every schedule. named with team name
  all_schedules <- lapply(all_teams, get_schedule)
  names(all_schedules) <- all_abbs
  
  ##list with clean schedules for every team
  clean_schedules <- get_clean_schedules(all_schedules) ###WORKS
  
  ##data into vector to be transformed into dataframe
  vector_for_df <- sapply(clean_schedules, loc_wins)
  
  #dataframe from vector
  dat <- (vector_for_df %>% t()) %>% as_tibble()
  colnames(dat) <- c('Team', 'GP', 'W', 'L', 'HGP', 'HW', 'HL', 'AGP', 'AW', 'AL', 'RD', 'HRD', 'ARD')
  dat$GP %<>% as.integer
  dat$W %<>% as.integer
  dat$L %<>% as.integer
  dat$HGP %<>% as.integer
  dat$HW %<>% as.integer
  dat$HL %<>% as.integer
  dat$AGP %<>% as.integer
  dat$AW %<>% as.integer
  dat$AL %<>% as.integer
  dat$RD %<>% as.integer
  dat$HRD %<>% as.integer
  dat$ARD %<>% as.integer
  
  
  dat %<>% mutate(HomeWP = HW / HGP, .before = AGP)
  dat %<>% mutate(AwayWP = AW / AGP)
  dat %<>% mutate(Diff = HomeWP / AwayWP)
  return(dat)
}

dat <- plottable_data()

dat

updated_since <- paste0(((today() - 1) %>% month(label = TRUE, abbr = FALSE)) %>% as.character(), ' ', ((today() -1) %>% day()) %>% as.character)
jpeg(file = paste0('homevsaway', today(), '.jpeg'))

plot <- ggplot(dat, aes(y = W, x = Diff)) + geom_mlb_logos(aes(team_abbr = Team), width = .03) +  theme_light() + 
  geom_vline(xintercept = 1, linetype = 'dotted', size = 1, color = 'red', alpha = .6) + labs(y = 'Wins', x = 'Home Winning % / Away Winning %', title = 'Proportion of Winning Percentage at Home to Winning Percentage on the Road Among MLB Teams', subtitle = paste0('Updated ', updated_since)) + 
  theme(plot.title = element_text(hjust = .5), plot.subtitle = element_text(hjust = .5), text = element_text(size = 16, family = 'Optima')) + scale_x_continuous(n.breaks = 10) + scale_y_continuous(n.breaks = 8)

plot
dev.off()




long_rd_dat <- dat[, c('Team', 'HRD', 'ARD')] %>% pivot_longer(cols = c(HRD, ARD), names_to = 'Where', values_to = 'RD')
long_p2 <- dat[, c('Team', 'HomeWP', 'AwayWP')] %>% pivot_longer(cols = c(HomeWP, AwayWP), names_to = 'Where', values_to = 'WP')
long_dat <- cbind(long_rd_dat, long_p2[,'WP'])
long_dat <- long_dat[order(long_dat$Team, long_dat$Where), ]




primary_colors <- c('#A71930','#CE1141', '#DF4601', '#BD3039', '#0E3386', '#27251F', '#C6011F', '#00385D', '#333366', '#0C2340',
                    '#002D62', '#004687', '#003263', '#005A9C', '#00A3E0', '#12284B', '#002B5C', '#002D72', '#003087',
                    '#003831', '#E81828', '#27251F', '#2F241D', '#0C2C56', '#FD5A1E', '#C41E3A', '#092C5C', '#003278', '#134A8E', '#AB0003')
secondary_colors <- c('#E3D4AD', '#13274F', '#000000', '#0C2340', '#CC3433', '#C4CED4', '#000000', '#E50022', '#C4CED4', '#FA4616',
                      '#EB6E1F', '#BD9B60', '#BA0021', '#EF3E42', '#EF3340', '#FFC52F', '#D31145', '#FF5910', '#E4002C',
                      '#EFB21E', '#002D72', '#FDB827', '#FFC425', '#005C5C', '#27251F', '#0C2340', '#8FBCE6', '#C0111F', '#1D2D5C', '#14225A')

team_colors <- c(rbind(primary_colors, secondary_colors))


ggplot(long_dat, aes(y = RD, x = Team, label = Where %>% substr(1, 1))) + geom_text(aes(size = WP), color = team_colors) + scale_size_continuous(range = c(4, 12)) +
  geom_line(aes(group = Team), linetype = 'dotted', alpha = .7) + geom_hline(yintercept = 0, linetype = 'dashed') + theme_minimal() + theme(plot.title = element_text(face = 'bold')) +
  labs(y = 'Run Differential', x = 'Team', title = 'Home and Away Run Differentials for MLB Teams' , subtitle = paste0('Updated ', updated_since), size = 'Relative\nWinning\nPercentage') + 
  theme(plot.title = element_text(hjust = .5), plot.subtitle = element_text(hjust = .5), text = element_text(size = 14, family = 'Optima')) + scale_y_continuous(n.breaks = 11)

ggplot(long_dat, aes(y = RD, x = Team, label = Where %>% substr(1, 1))) + geom_text(aes(size = WP), color = team_colors) + scale_size_continuous(range = c(4, 12)) +
  geom_line(aes(group = Team), linetype = 'dotted', alpha = .7) + geom_hline(yintercept = 0, linetype = 'dashed') + theme_minimal() + theme(plot.title = element_text(face = 'bold')) +
  labs(y = 'Run Differential', x = 'Team', title = 'Home and Away Run Differentials for MLB Teams' , subtitle = paste0('Updated ', updated_since), size = 'Relative\nWinning\nPercentage') + 
  theme(plot.title = element_text(hjust = .5), plot.subtitle = element_text(hjust = .5), text = element_text(size = 14, family = 'Optima')) + scale_y_continuous(n.breaks = 11)

ggplot(long_dat[long_dat$Team %in% team_abbrs,], aes(y = RD, x = Team, label = Where %>% substr(1, 1))) + geom_text(aes(size = WP), color = team_colors) + theme(axis.title.x = element_mlb_logo())

team_abbrs <- valid_team_names()
team_abbrs <- team_abbrs[!team_abbrs %in% c('NL', 'AL', 'MLB')]
team_abbrs == sort(all_abbs)
all_abbs[all_abbs %in% team_abbrs]
long_dat[long_dat$Team %in% team_abbrs,]

team_abbrs
all_abbs %>% sort

ggplot(dat[dat$Team %in% team_abbrs,], aes(y = W, x = Team)) + geom_col(aes(color = Team, fill = Team), width = 0.5) +
  scale_color_mlb(type = "secondary") +
  scale_fill_mlb(alpha = 0.4) +
  theme_minimal() + theme(axis.title.x = element_mlb_logo())


