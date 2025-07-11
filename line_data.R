library(tidyverse)
library(glue)

group_shifts <- function(pbp){
  pbp <- pbp %>%
    filter(period < 5)
  
  pbp$home_on_1 <- ifelse(is.na(pbp$home_on_1), "MISSING", pbp$home_on_1)
  pbp$home_on_2 <- ifelse(is.na(pbp$home_on_2), "MISSING", pbp$home_on_2)
  pbp$home_on_3 <- ifelse(is.na(pbp$home_on_3), "MISSING", pbp$home_on_3)
  pbp$home_on_4 <- ifelse(is.na(pbp$home_on_4), "MISSING", pbp$home_on_4)
  pbp$home_on_5 <- ifelse(is.na(pbp$home_on_5), "MISSING", pbp$home_on_5)
  pbp$home_on_6 <- ifelse(is.na(pbp$home_on_6), "MISSING", pbp$home_on_6)
  pbp$home_on_7 <- ifelse(is.na(pbp$home_on_7), "MISSING", pbp$home_on_7)
  
  pbp$away_on_1 <- ifelse(is.na(pbp$away_on_1), "MISSING", pbp$away_on_1)
  pbp$away_on_2 <- ifelse(is.na(pbp$away_on_2), "MISSING", pbp$away_on_2)
  pbp$away_on_3 <- ifelse(is.na(pbp$away_on_3), "MISSING", pbp$away_on_3)
  pbp$away_on_4 <- ifelse(is.na(pbp$away_on_4), "MISSING", pbp$away_on_4)
  pbp$away_on_5 <- ifelse(is.na(pbp$away_on_5), "MISSING", pbp$away_on_5)
  pbp$away_on_6 <- ifelse(is.na(pbp$away_on_6), "MISSING", pbp$away_on_6)
  pbp$away_on_7 <- ifelse(is.na(pbp$away_on_7), "MISSING", pbp$away_on_7)
  
  pbp$shift_change <- ifelse(lag(pbp$home_on_1)==pbp$home_on_1 & lag(pbp$home_on_2)==pbp$home_on_2 & lag(pbp$home_on_3)==pbp$home_on_3 &
                               lag(pbp$home_on_4)==pbp$home_on_4 & lag(pbp$home_on_5)==pbp$home_on_5 & lag(pbp$home_on_6)==pbp$home_on_6 &
                               lag(pbp$away_on_1)==pbp$away_on_1 & lag(pbp$away_on_2)==pbp$away_on_2 & lag(pbp$away_on_3)==pbp$away_on_3 &
                               lag(pbp$away_on_4)==pbp$away_on_4 & lag(pbp$away_on_5)==pbp$away_on_5 & lag(pbp$away_on_6)==pbp$away_on_6 &
                               lag(pbp$home_score)==pbp$home_score & lag(pbp$away_score)==pbp$away_score & lag(pbp$period)==pbp$period &  
                               lag(pbp$game_id)==pbp$game_id, 0, 1)
  
  pbp$shift_change <- ifelse(is.na(pbp$shift_change), 0, pbp$shift_change)
  
  pbp$shift_change_index <- cumsum(pbp$shift_change)
  
  pbp <- pbp %>% mutate(game_score_state = glue("{home_score}v{away_score}"),
                        event_length = game_seconds - lag(game_seconds))
  pbp$home_xGF <- ifelse(pbp$event_team_name==pbp$home_team & !is.na(pbp$xg), pbp$xg, 0)
  pbp$away_xGF <- ifelse(pbp$event_team_name!=pbp$home_team & !is.na(pbp$xg), pbp$xg, 0)
  pbp$home_GF <- ifelse(pbp$event_type=="GOAL" & pbp$event_team_name==pbp$home_team, 1, 0)
  pbp$away_GF <- ifelse(pbp$event_type=="GOAL" & pbp$event_team_name==pbp$away_team, 1, 0)
  pbp$home_CF <- ifelse(pbp$event_type %in% c("GOAL","SHOT_ON_GOAL","MISSED_SHOT","BLOCKED_SHOT") & pbp$event_team_name==pbp$home_team, 1, 0)
  pbp$away_CF <- ifelse(pbp$event_type %in% c("GOAL","SHOT_ON_GOAL","MISSED_SHOT","BLOCKED_SHOT") & pbp$event_team_name==pbp$away_team, 1, 0)
  pbp$home_FF <- ifelse(pbp$event_type %in% c("GOAL","SHOT_ON_GOAL","MISSED_SHOT") & pbp$event_team_name==pbp$home_team, 1, 0)
  pbp$away_FF <- ifelse(pbp$event_type %in% c("GOAL","SHOT_ON_GOAL","MISSED_SHOT") & pbp$event_team_name==pbp$away_team, 1, 0)
  
  
  grouped_shifts <- pbp %>%
    group_by(game_id, shift_change_index, period, game_score_state, home_on_1, home_on_2, home_on_3, home_on_4, home_on_5, home_on_6, home_on_7,
             away_on_1, away_on_2, away_on_3, away_on_4, away_on_5, away_on_6, away_on_7) %>%
    summarise(shift_length = sum(event_length), homexGF = sum(home_xGF), awayxGF = sum(away_xGF), homeGF = sum(home_GF), awayGF = sum(away_GF),
              homeCF = sum(home_CF), awayCF = sum(away_CF), homeFF = sum(home_FF), awayFF = sum(away_FF)) %>%
    filter(shift_length > 0)
  
  home <- grouped_shifts %>%
    rename(offense_1 = home_on_1, offense_2 = home_on_2, offense_3 = home_on_3, offense_4 = home_on_4, offense_5 = home_on_5, offense_6 = home_on_6, offense_7 = home_on_7,
           defense_1 = away_on_1, defense_2 = away_on_2, defense_3 = away_on_3, defense_4 = away_on_4, defense_5 = away_on_5, defense_6 = away_on_6, defense_7 = away_on_7,
           xGF = homexGF, xGA = awayxGF, GF = homeGF, GA = awayGF, CF = homeCF, CA = awayCF, FF = homeFF, FA = awayFF) %>%
    select(game_id, shift_change_index, period, game_score_state,
           offense_1, offense_2, offense_3, offense_4, offense_5, offense_6, offense_7,
           defense_1, defense_2, defense_3, defense_4, defense_5, defense_6, defense_7,
           xGF, GF, CF, FF, xGA, GA, CA, FA, shift_length, shift_change_index)
  
  away <- grouped_shifts %>%
    rename(offense_1 = away_on_1, offense_2 = away_on_2, offense_3 = away_on_3, offense_4 = away_on_4, offense_5 = away_on_5, offense_6 = away_on_6, offense_7 = away_on_7,
           defense_1 = home_on_1, defense_2 = home_on_2, defense_3 = home_on_3, defense_4 = home_on_4, defense_5 = home_on_5, defense_6 = home_on_6, defense_7 = home_on_7,
           xGF = awayxGF, xGA = homexGF, GF = awayGF, GA = homeGF, CF = awayCF, CA = homeCF, FF = awayFF, FA = homeFF) %>%
    select(game_id, shift_change_index, period, game_score_state,
           offense_1, offense_2, offense_3, offense_4, offense_5, offense_6, offense_7,
           defense_1, defense_2, defense_3, defense_4, defense_5, defense_6, defense_7,
           xGF, GF, CF, FF, xGA, GA, CA, FA, shift_length, shift_change_index)
  
  shifts_combined <- full_join(home, away) %>% arrange(shift_change_index) %>%
    mutate(offense_players = paste(offense_1,offense_2,offense_3,offense_4,offense_5,offense_6,offense_7),
           defense_players = paste(defense_1,defense_2,defense_3,defense_4,defense_5,defense_6,defense_7))
  
  shifts_combined$offense_players <- gsub(" MISSING","",shifts_combined$offense_players)
  shifts_combined$defense_players <- gsub(" MISSING","",shifts_combined$defense_players)
  shifts_combined$strength_state <- paste(str_count(shifts_combined$offense_players," ") + 1,str_count(shifts_combined$defense_players," ") + 1,sep = "v")
  
  return(shifts_combined)
}
pairs <- function(shifts){
  players <- str_split(shifts["offense_players"]," ",simplify = TRUE)
  groups <- data.frame(t(data.frame(combn(players,2,simplify = FALSE))),shifts["shift_change_index"])
  return(groups)
}
trios <- function(shifts){
  players <- str_split(shifts["offense_players"]," ",simplify = TRUE)
  groups <- data.frame(t(data.frame(combn(players,3,simplify = FALSE))),shifts["shift_change_index"])
  return(groups)
}
QOT <- function(shifts){
  pairings <- data.frame(crossing(c(str_split(shifts["offense_players"]," ",simplify = TRUE)),c(str_split(shifts["offense_players"]," ",simplify = TRUE))),shifts["shift_change_index"])
  return(pairings)
}
QOC <- function(shifts){
  pairings <- data.frame(crossing(c(str_split(shifts["offense_players"]," ",simplify = TRUE)),c(str_split(shifts["defense_players"]," ",simplify = TRUE))),shifts["shift_change_index"])
  return(pairings)
}
player_toi <- function(pbp){
  power_play <- c("toi_5v4","toi_5v3","toi_4v3","toi_6v4","toi_6v3","toi_7v5")
  penalty_kill <- c("toi_4v5","toi_3v5","toi_3v4","toi_4v6","toi_3v6","toi_5v7")
  even <- c("toi_5v5","toi_4v4","toi_3v3","toi_6v6","toi_6v5","toi_5v6")
  all <- c(power_play,penalty_kill,even)
  
  grouped_shifts <- group_shifts(pbp)
  
  toi <- grouped_shifts %>%
    ungroup() %>%
    select(game_id,offense_1,offense_2,offense_3,offense_4,offense_5,offense_6,offense_7,shift_length,strength_state) %>%
    pivot_longer(offense_1:offense_7,values_to = "playerID") %>%
    select(-name) %>%
    filter(playerID != "MISSING") %>%
    mutate(playerID = as.numeric(as.character(playerID))) %>%
    group_by(playerID,strength_state) %>%
    summarize(toi = round(sum(shift_length)/60,2),
              .groups = "drop") %>%
    pivot_wider(names_from = strength_state,names_prefix = "toi_",values_from = toi,values_fill = 0)
  
  for(i in all){
    if(i %not_in% names(toi)){
      toi[, i] <- 0
    }
  }
  
  toi <- toi %>%
    mutate(toi_all = toi_5v5+toi_4v4+toi_3v3+toi_6v6+toi_6v5+toi_5v6+toi_5v4+toi_5v3+toi_4v3+toi_6v4+toi_6v3+toi_4v5+toi_3v5+toi_3v4+toi_4v6+toi_3v6+toi_6v6+toi_7v5+toi_5v7,
           toi_even = toi_5v5+toi_4v4+toi_3v3+toi_6v6+toi_6v5+toi_5v6,
           toi_pp = toi_5v4+toi_5v3+toi_4v3+toi_6v4+toi_6v3+toi_7v5,
           toi_pk = toi_4v5+toi_3v5+toi_3v4+toi_4v6+toi_3v6+toi_5v7
    ) %>%
    select(playerID,toi_all,toi_even,toi_pp,toi_pk,toi_5v5,toi_4v4,toi_3v3,toi_6v6,toi_6v5,toi_5v6,toi_5v4,toi_5v3,toi_4v3,toi_6v4,toi_6v3,toi_7v5,toi_4v5,toi_3v5,toi_3v4,toi_4v6,toi_3v6,toi_5v7)
  
  return(toi)
}
`%not_in%` <- Negate(`%in%`)

pbp <- readRDS(url("https://github.com/zackkehl/HockeyZK_dataupdates/raw/main/data/pbp_24_25.rds"))
player_info <- readRDS(url("https://github.com/zackkehl/HockeyZK_dataupdates/raw/main/data/player_data.rds"))
player_stats_24_25 <- readRDS(url("https://github.com/zkautoupdates/autoupdates/raw/main/data/player_stats_24_25.rds"))

combos <- group_shifts(pbp)
combos <- combos %>% filter(offense_players != "MISSING") %>% filter(defense_players != "MISSING") %>%
  filter(strength_state == "5v5")
combos$shift_change_index <- row_number(combos$shift_change_index)
toi <- player_toi(pbp)

# F Lines #
gc()
f_lines <- apply(combos,1,trios)
f_lines <- bind_rows(f_lines)

f_lines <- f_lines %>% 
  rename("player1" = X1,"player2" = X2,"player3" = X3, shift_change_index = shifts..shift_change_index..) %>% 
  filter(player1 != player2 & player1 != player3 & player2 != player3) %>% 
  mutate_all(function(x) as.numeric(as.character(x)))
rownames(f_lines) <- NULL
f_line_data <- combos %>%
  ungroup() %>%
  select(shift_change_index,xGF,GF,CF,FF,xGA,GA,CA,FA,shift_length,strength_state)
f_lines <- f_lines %>%
  left_join(f_line_data,by="shift_change_index") %>%
  select(-shift_change_index) %>%
  left_join(player_info %>% select(playerID,full_name,position),by=c("player1"="playerID")) %>%
  rename(player1_full_name = full_name,player1_position = position) %>%
  left_join(player_info %>% select(playerID,full_name,position),by=c("player2"="playerID")) %>%
  rename(player2_full_name = full_name,player2_position = position) %>%
  left_join(player_info %>% select(playerID,full_name,position),by=c("player3"="playerID")) %>%
  rename(player3_full_name = full_name,player3_position = position) %>%
  select(player1,player1_full_name,player1_position,player2,player2_full_name,player2_position,player3,player3_full_name,player3_position,everything()) %>%
  filter(player1_position == "F" & player2_position == "F" & player3_position == "F") %>%
  group_by(player1,player2,player3,player1_full_name,player2_full_name,player3_full_name) %>%
  summarize(
    TOI = sum(shift_length),
    xGF = sum(xGF),
    GF = sum(GF),
    CF = sum(CF),
    FF = sum(FF),
    xGA = sum(xGA),
    GA = sum(GA),
    CA = sum(CA),
    FA = sum(FA),
    .groups = "drop"
  ) %>%
  mutate(pair = paste(player1_full_name,player2_full_name,player3_full_name,sep = " - ")) %>%
  select(player1,player2,player3,pair,everything(),-c(player1_full_name,player2_full_name,player3_full_name)) %>%
  mutate(
    "xGF%" = xGF/(xGF+xGA),
    "GF%" = GF/(GF+GA),
    "CF%" = CF/(CF+CA)
  ) %>%
  arrange(-TOI)

# D Pairs #
gc()
d_pairs <- apply(combos,1,pairs)
d_pairs <- bind_rows(d_pairs)

d_pairs <- d_pairs %>% 
  rename("player1" = X1,"player2" = X2,shift_change_index = shifts..shift_change_index..) %>% 
  filter(player1 != player2) %>% 
  mutate_all(function(x) as.numeric(as.character(x)))
rownames(d_pairs) <- NULL
d_pair_data <- combos %>%
  ungroup() %>%
  select(shift_change_index,xGF,GF,CF,FF,xGA,GA,CA,FA,shift_length,strength_state)
d_pairs <- d_pairs %>%
  left_join(d_pair_data,by="shift_change_index") %>%
  select(-shift_change_index) %>%
  left_join(player_info %>% select(playerID,full_name,position),by=c("player1"="playerID")) %>%
  rename(player1_full_name = full_name,player1_position = position) %>%
  left_join(player_info %>% select(playerID,full_name,position),by=c("player2"="playerID")) %>%
  rename(player2_full_name = full_name,player2_position = position) %>%
  select(player1,player1_full_name,player1_position,player2,player2_full_name,player2_position,everything()) %>%
  filter(player1_position == "D" & player2_position == "D") %>%
  group_by(player1,player2,player1_full_name,player2_full_name) %>%
  summarize(
    TOI = sum(shift_length),
    xGF = sum(xGF),
    GF = sum(GF),
    CF = sum(CF),
    FF = sum(FF),
    xGA = sum(xGA),
    GA = sum(GA),
    CA = sum(CA),
    FA = sum(FA),
    .groups = "drop"
  ) %>%
  mutate(pair = paste(player1_full_name,player2_full_name,sep = " - ")) %>%
  select(player1,player2,pair,everything(),-c(player1_full_name,player2_full_name)) %>%
  mutate(
    "xGF%" = xGF/(xGF+xGA),
    "GF%" = GF/(GF+GA),
    "CF%" = CF/(CF+CA)
  ) %>%
  arrange(-TOI)

# QOT #
gc()
qot <- apply(combos,1,QOT)
qot <- bind_rows(qot)
colnames(qot) <- c("player1","player2","shift_change_index")

qot_data <- combos %>%
  ungroup() %>%
  select(shift_change_index,xGF,GF,CF,FF,xGA,GA,CA,FA,shift_length,strength_state)

qot <- qot %>%
  filter(player1 != player2) %>%
  mutate_all(function(x) as.numeric(as.character(x))) %>%
  left_join(qot_data,by="shift_change_index") %>%
  left_join(player_info %>% select(playerID,full_name,position),by=c("player1"="playerID")) %>%
  rename(player1_full_name = full_name,player1_position = position) %>%
  left_join(player_info %>% select(playerID,full_name,position),by=c("player2"="playerID")) %>%
  rename(player2_full_name = full_name,player2_position = position) %>%
  select(player1,player1_full_name,player1_position,player2,player2_full_name,player2_position,everything()) %>%
  group_by(player1,player2,player1_full_name,player2_full_name) %>%
  summarize(toi_together = sum(shift_length),.groups = "drop") %>%
  left_join(toi %>% select(playerID,player1_toi=toi_5v5),by=c("player1"="playerID")) %>%
  left_join(player_stats_24_25 %>% select(playerID,teammate_OVR=OVR),by=c("player2"="playerID")) %>%
  mutate(toi_together=toi_together/60, toi_perc=toi_together/player1_toi, teammate_strength=toi_perc*teammate_OVR) %>%
  drop_na(teammate_strength) %>%
  group_by(playerID=player1) %>%
  summarize(QOT = sum(teammate_strength),.groups = "drop") %>%
  left_join(player_info %>% select(playerID,full_name,position,team),by="playerID") %>%
  select(playerID,full_name,position,team,QOT) %>%
  arrange(-QOT)

# QOC #
gc()
qoc <- apply(combos,1,QOC)
qoc <- bind_rows(qoc)
colnames(qoc) <- c("player1","player2","shift_change_index")

qoc_data <- combos %>%
  ungroup() %>%
  select(shift_change_index,xGF,GF,CF,FF,xGA,GA,CA,FA,shift_length,strength_state)

qoc <- qoc %>%
  filter(player1 != player2) %>%
  mutate_all(function(x) as.numeric(as.character(x))) %>%
  left_join(qoc_data,by="shift_change_index") %>%
  left_join(player_info %>% select(playerID,full_name,position),by=c("player1"="playerID")) %>%
  rename(player1_full_name = full_name,player1_position = position) %>%
  left_join(player_info %>% select(playerID,full_name,position),by=c("player2"="playerID")) %>%
  rename(player2_full_name = full_name,player2_position = position) %>%
  select(player1,player1_full_name,player1_position,player2,player2_full_name,player2_position,everything()) %>%
  group_by(player1,player2,player1_full_name,player2_full_name) %>%
  summarize(toi_together = sum(shift_length),.groups = "drop") %>%
  left_join(toi %>% select(playerID,player1_toi=toi_5v5),by=c("player1"="playerID")) %>%
  left_join(player_stats_24_25 %>% select(playerID,teammate_OVR=OVR),by=c("player2"="playerID")) %>%
  mutate(toi_together=toi_together/60, toi_perc=toi_together/player1_toi, opp_strength=toi_perc*teammate_OVR) %>%
  drop_na(opp_strength) %>%
  group_by(playerID=player1) %>%
  summarize(QOC = sum(opp_strength),.groups = "drop") %>%
  left_join(player_info %>% select(playerID,full_name,position,team),by="playerID") %>%
  select(playerID,QOC) %>%
  arrange(-QOC)

# Save data
f_lines %>% saveRDS("data/f_lines_24_25.rds")
d_pairs %>% saveRDS("data/d_pairs_24_25.rds")
qotc <- left_join(qot,qoc,by="playerID")
qotc %>% saveRDS("data/qotc_24_25.rds")
