library(tidyverse)
library(glue)

## GET CURRENT SEASON DATA #####################################################
current_season <- "25_26"

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
`%not_in%` <- Negate(`%in%`)
skater_onice <- function(pbp,game_strength){
  power_play <- c("5v4","5v3","4v3","6v4","6v3")
  penalty_kill <- c("4v5","3v5","3v4","4v6","3v6")
  
  if (game_strength == "pp"){
    pbp <- pbp %>% filter(strength_state %in% power_play)
  }
  if (game_strength == "pk"){
    pbp <- pbp %>% filter(strength_state %in% penalty_kill)
  }
  if (game_strength == "even") {
    pbp <- pbp %>% filter(strength_state == "5v5")
  }
  
  pbp <- pbp %>% 
    filter(period < 5) %>%
    filter(event_type %in% c("GOAL","SHOT_ON_GOAL","MISSED_SHOT","BLOCKED_SHOT"))
  
  pbp <- pbp %>%
    group_by(event_id) %>%
    mutate(
      home_players = paste(home_on_1,home_on_2,home_on_3,home_on_4,
                           home_on_5,home_on_6,home_on_7,sep = ";"),
      away_players = paste(away_on_1,away_on_2,away_on_3,away_on_4,
                           away_on_5,away_on_6,away_on_7,sep = ";")
    ) %>%
    ungroup() %>%
    select(game_id,event_id,home_team,away_team,event_team_name,event_type,xg,home_players,away_players)
  
  pbp$home_players <- str_remove_all(pbp$home_players,";NA")
  pbp$away_players <- str_remove_all(pbp$away_players,";NA")
  
  home_stats <- pbp %>%
    select(-away_players) %>%
    separate_rows(home_players,sep = ";") %>%
    mutate(xgf = xg * (event_team_name == home_team),
           xga = xg * (event_team_name == away_team),
           gf = ifelse(event_type == "GOAL" & event_team_name == home_team,1,0),
           ga = ifelse(event_type == "GOAL" & event_team_name == away_team,1,0),
           ff = ifelse(event_type != "MISSED_SHOT" & event_team_name == home_team,1,0),
           fa = ifelse(event_type != "MISSED_SHOT" & event_team_name == away_team,1,0)) %>%
    group_by(playerID = home_players) %>%
    summarise(
      gf = sum(gf),
      ga = sum(ga),
      xgf = sum(xgf, na.rm = TRUE),
      xga = sum(xga, na.rm = TRUE),
      cf = sum(event_team_name == home_team),
      ca = sum(event_team_name == away_team),
      ff = sum(ff),
      fa = sum(fa),
      .groups = "drop"
    )
  
  away_stats <- pbp %>%
    select(-home_players) %>%
    separate_rows(away_players,sep = ";") %>%
    mutate(xgf = xg * (event_team_name == away_team),
           xga = xg * (event_team_name == home_team),
           gf = ifelse(event_type == "GOAL" & event_team_name == away_team,1,0),
           ga = ifelse(event_type == "GOAL" & event_team_name == home_team,1,0),
           ff = ifelse(event_type != "MISSED_SHOT" & event_team_name == away_team,1,0),
           fa = ifelse(event_type != "MISSED_SHOT" & event_team_name == home_team,1,0)) %>%
    group_by(playerID = away_players) %>%
    summarise(
      gf = sum(gf),
      ga = sum(ga),
      xgf = sum(xgf, na.rm = TRUE),
      xga = sum(xga, na.rm = TRUE),
      cf = sum(event_team_name == away_team),
      ca = sum(event_team_name == home_team),
      ff = sum(ff),
      fa = sum(fa),
      .groups = "drop"
    )
  
  total <- bind_rows(home_stats,away_stats) %>%
    group_by(playerID) %>%
    summarise(
      gf = sum(gf),
      ga = sum(ga),
      xgf = sum(xgf),
      xga = sum(xga),
      cf = sum(cf),
      ca = sum(ca),
      ff = sum(ff),
      fa = sum(fa),
      .groups = "drop"
    )
  
  total$playerID = as.numeric(as.character(total$playerID))
  total <- total %>% drop_na()
  
  return(total)
  
}
savant_cols <- c("playerID","OVR","EVO","EVD","PPO","SHD","FIN","PEN","game_score",
                 "gp","toi_gp","goals","assists","points","points_primary","ixg")
rapm_cols <- c("playerID","RAPM_xGF","RAPM_GF","RAPM_CF","RAPM_xGA","RAPM_GA","RAPM_CA")
onice_cols <- c("playerID","xgf_60","gf_60","cf_60","xga_60","ga_60","ca_60")
season_start <- substr(current_season,1,2)
season_end <- substr(current_season,4,5)
start_date <- glue("20{season_start}-10-01")

player_stats <- readRDS(url(glue("https://github.com/zkautoupdates/autoupdates/raw/main/data/player_stats_{current_season}.rds")))
player_stats <- player_stats %>% arrange(desc(OVR)) %>% mutate(toi_gp = toi/gp)
pbp <- readRDS(url(glue("https://github.com/zackkehl/HockeyZK_dataupdates/raw/main/data/pbp_{current_season}.rds")))
playernames <- readRDS(url("https://github.com/zackkehl/HockeyZK_dataupdates/raw/main/data/player_data.rds")) %>% 
  mutate(headshot = glue("https://assets.nhle.com/mugs/nhl/20{season_start}20{season_end}/{team}/{playerID}.png"))
toi <- player_toi(pbp)
f_lines <- readRDS(url(glue("https://github.com/zkautoupdates/autoupdates/raw/main/data/f_lines_{current_season}.rds"))) %>% mutate(season = current_season)
d_pairs <- readRDS(url(glue("https://github.com/zkautoupdates/autoupdates/raw/main/data/d_pairs_{current_season}.rds"))) %>% mutate(season = current_season)
qotc <- readRDS(url(glue("https://github.com/zkautoupdates/autoupdates/raw/main/data/qotc_{current_season}.rds")))
rapm <- readRDS(url(glue("https://github.com/zkautoupdates/autoupdates/raw/main/data/player_rapm_ev_{current_season}.rds"))) %>% 
  rename(RAPM_xGF = xGF_60, RAPM_GF = GF_60, RAPM_CF = CF_60, RAPM_xGA = xGA_60, RAPM_GA = GA_60, RAPM_CA = CA_60) %>% 
  select(all_of(rapm_cols))
even <- skater_onice(pbp,"even") %>%
  left_join(toi %>% select(playerID,toi=toi_5v5),by="playerID") %>%
  mutate(xgf_60=xgf/toi*60,gf_60=gf/toi*60,xga_60=xga/toi*60,ga_60=ga/toi*60,cf_60=cf/toi*60,ca_60=ca/toi*60) %>%
  select(all_of(onice_cols))
toi_threshold <- min(250,(length(unique(pbp$game_id))/16)*10)


data_to_use <- playernames %>%
  left_join(player_stats %>% select(all_of(savant_cols)),by="playerID") %>%
  left_join(qotc %>% select(playerID,QOT,QOC),by="playerID") %>%
  left_join(rapm, by="playerID") %>%
  left_join(even, by="playerID")
data_to_use <- data_to_use[!is.na(data_to_use$OVR),]

forwards <- data_to_use %>%
  filter(position == "F") %>%
  mutate(across(c("OVR","EVO","EVD","FIN","PEN","game_score"), ~ round(percent_rank(.)*100,0), .names = "{.col}_ntile")) %>%
  mutate(across(goals:points, ~ round(percent_rank(.)*100,0), .names = "{.col}_ntile")) %>%
  mutate(across(QOT:QOC, ~ round(percent_rank(.)*100,0), .names = "{.col}_ntile")) %>%
  mutate(across(RAPM_xGF:RAPM_CF, ~ scale(.), .names = "{.col}_z")) %>%
  mutate(across(RAPM_xGA:RAPM_CA, ~ scale(.)*-1, .names = "{.col}_z")) %>%
  mutate(across(xgf_60:cf_60, ~ scale(.), .names = "{.col}_z")) %>%
  mutate(across(xga_60:ca_60, ~ scale(.)*-1, .names = "{.col}_z"))
forwards_pp <- data_to_use %>%
  filter(position == "F" & PPO != 0) %>%
  mutate(PPO_ntile = round(percent_rank(PPO)*100,0)) %>%
  select(playerID,PPO_ntile)
forwards_pk <- data_to_use %>%
  filter(position == "F" & SHD != 0) %>%
  mutate(SHD_ntile = round(percent_rank(SHD)*100,0)) %>%
  select(playerID,SHD_ntile)
forwards <- forwards %>%
  left_join(forwards_pp,by="playerID") %>%
  left_join(forwards_pk,by="playerID") %>%
  relocate(PPO_ntile,SHD_ntile,.after = EVD_ntile)

defense <- data_to_use %>%
  filter(position == "D") %>%
  mutate(across(c("OVR","EVO","EVD","FIN","PEN","game_score"), ~ round(percent_rank(.)*100,0), .names = "{.col}_ntile")) %>%
  mutate(across(goals:points, ~ round(percent_rank(.)*100,0), .names = "{.col}_ntile")) %>%
  mutate(across(QOT:QOC, ~ round(percent_rank(.)*100,0), .names = "{.col}_ntile")) %>%
  mutate(across(RAPM_xGF:RAPM_CF, ~ scale(.), .names = "{.col}_z")) %>%
  mutate(across(RAPM_xGA:RAPM_CA, ~ scale(.)*-1, .names = "{.col}_z")) %>%
  mutate(across(xgf_60:cf_60, ~ scale(.), .names = "{.col}_z")) %>%
  mutate(across(xga_60:ca_60, ~ scale(.)*-1, .names = "{.col}_z"))
defense_pp <- data_to_use %>%
  filter(position == "D" & PPO != 0) %>%
  mutate(PPO_ntile = round(percent_rank(PPO)*100,0)) %>%
  select(playerID,PPO_ntile)
defense_pk <- data_to_use %>%
  filter(position == "D" & SHD != 0) %>%
  mutate(SHD_ntile = round(percent_rank(SHD)*100,0)) %>%
  select(playerID,SHD_ntile)
defense <- defense %>%
  left_join(defense_pp,by="playerID") %>%
  left_join(defense_pk,by="playerID") %>%
  relocate(PPO_ntile,SHD_ntile,.after = EVD_ntile)

data_to_use <- rbind(forwards,defense) %>% arrange(-OVR) %>%
  mutate(team_logo = glue("https://assets.nhle.com/logos/nhl/svg/{team}_light.svg"), 
         feet = height %/% 12, 
         inches = height %% 12, 
         height = glue("{feet}'{inches}"), 
         age = floor(time_length(difftime(start_date,birth_date),"years"))) %>% 
  select(-c(feet,inches)) %>%
  mutate_at(vars(RAPM_xGF_z,RAPM_GF_z,RAPM_CF_z,RAPM_xGA_z,RAPM_GA_z,RAPM_CA_z,xgf_60_z,gf_60_z,cf_60_z,xga_60_z,ga_60_z,ca_60_z),
            ~ifelse(.>3,3,.)) %>%
  mutate_at(vars(RAPM_xGF_z,RAPM_GF_z,RAPM_CF_z,RAPM_xGA_z,RAPM_GA_z,RAPM_CA_z,xgf_60_z,gf_60_z,cf_60_z,xga_60_z,ga_60_z,ca_60_z),
            ~ifelse(.<(-3),-3,.)) %>%
  mutate(season = current_season) %>%
  relocate(team,season,age,.after = position_code) %>%
  select(-c(isActive,birth_date))

rm(player_stats,pbp,playernames,toi,qotc,rapm,even,forwards,forwards_pp,forwards_pk,defense,defense_pp,defense_pk)
gc()
################################################################################

playerviz_data <- readRDS(url("https://github.com/zkautoupdates/autoupdates/raw/main/data/playerviz_data.rds")) %>%
  filter(season != current_season) %>%
  rbind(data_to_use) %>%
  arrange(desc(season),-OVR)

playerviz_f_lines <- readRDS(url("https://github.com/zkautoupdates/autoupdates/raw/main/data/playerviz_f_lines.rds")) %>%
  filter(season != current_season) %>%
  rbind(f_lines) %>%
  arrange(desc(season),-TOI)

playerviz_d_pairs <- readRDS(url("https://github.com/zkautoupdates/autoupdates/raw/main/data/playerviz_d_pairs.rds")) %>%
  filter(season != current_season) %>%
  rbind(d_pairs) %>%
  arrange(desc(season),-TOI)

playerviz_data %>% saveRDS("data/playerviz_data.rds")
playerviz_f_lines %>% saveRDS("data/playerviz_f_lines.rds")
playerviz_d_pairs %>% saveRDS("data/playerviz_d_pairs.rds")
