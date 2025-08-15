library(tidyverse)
library(glue)

`%not_in%` <- Negate(`%in%`)
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
skater_individual <- function(pbp,game_strength){
  power_play <- c("5v4","5v3","4v3","6v4","6v3")
  penalty_kill <- c("4v5","3v5","3v4","4v6","3v6")
  
  games <- pbp %>%
    filter(!is.na(event_player_1_id)) %>%
    group_by(playerID = event_player_1_id) %>%
    summarize(gp = length(unique(game_id)),
              .groups = "drop")
  
  if (game_strength == "pp"){
    pbp <- pbp %>% filter(strength_state %in% power_play)
  }
  if (game_strength == "pk"){
    pbp <- pbp %>% filter(strength_state %in% penalty_kill)
  }
  if (game_strength == "even") {
    pbp <- pbp %>% filter(strength_state == "5v5")
  }
  
  pbp <- pbp %>% filter(period < 5)
  
  
  ind <- pbp %>%
    filter(event_type %in% c("GOAL","SHOT_ON_GOAL","MISSED_SHOT","BLOCKED_SHOT","PENALTY")) %>%
    group_by(playerID = event_player_1_id) %>%
    summarize(ixg = sum(xg, na.rm = TRUE),
              goals = sum(event_type == "GOAL"),
              icf = sum(event_type %in% c("GOAL","SHOT_ON_GOAL","MISSED_SHOT","BLOCKED_SHOT")),
              iff = sum(event_type %in% c("GOAL","SHOT_ON_GOAL","MISSED_SHOT")),
              isog = sum(event_type %in% c("GOAL","SHOT_ON_GOAL")),
              gax = goals - ixg,
              pens_taken = sum(event_type == "PENALTY"),
              .groups = "drop"
    )
  a1 <- pbp %>%
    filter(event_type == "GOAL" & !is.na(event_player_2_id)) %>%
    group_by(playerID = event_player_2_id) %>%
    summarize(assists_prim = sum(event_type == "GOAL"),
              .groups = "drop")
  a2 <- pbp %>%
    filter(event_type == "GOAL" & !is.na(event_player_3_id)) %>%
    group_by(playerID = event_player_3_id) %>%
    summarize(assists_sec = sum(event_type == "GOAL"),
              .groups = "drop")
  pens_drawn <- pbp %>%
    filter(event_type == "PENALTY") %>%
    group_by(playerID = event_player_2_id) %>%
    summarize(pens_drawn = sum(event_type == "PENALTY"),
              .groups = "drop")
  
  player_stats <- games %>%
    full_join(ind, by="playerID") %>%
    full_join(a1, by="playerID") %>%
    full_join(a2, by="playerID") %>%
    full_join(pens_drawn, by="playerID") %>%
    mutate(
      across(
        .cols = everything(),
        ~replace(.x, is.na(.x), 0)
      )
    ) %>%
    mutate(
      goals = ifelse(is.na(goals), 0, goals),
      assists_prim = ifelse(is.na(assists_prim), 0, assists_prim),
      assists_sec = ifelse(is.na(assists_sec), 0, assists_sec),
      assists = assists_prim + assists_sec,
      points = goals + assists,
      points_primary = goals + assists_prim
    ) %>%
    select(
      playerID, gp, ixg, goals, assists, points, assists_prim, assists_sec, points_primary, gax, icf, iff, isog, pens_taken, pens_drawn
    )
  
  player_stats <- player_stats %>% drop_na()
  
  return(player_stats)
}
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
goalie_stats <- function(pbp,game_strength){
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
  
  goalie <- pbp %>%
    group_by(playerID = event_goalie_id) %>%
    summarise(
      gp = length(unique(game_id)),
      xga = sum(xg, na.rm = TRUE),
      ga = sum(event_type == "GOAL"),
      saves = sum(event_type == "SHOT_ON_GOAL"),
      shots_against = ga + saves,
      gsax = xga - ga,
      .groups = "drop"
    ) %>%
    select(playerID,gp,gsax,xga,ga,saves,shots_against) %>%
    drop_na()
  
  return(goalie)
  
}

#### Player/Goalie Data - Single Season ########################################
pbp <- readRDS(url("https://github.com/zackkehl/HockeyZK_dataupdates/raw/main/data/pbp_24_25.rds"))

toi <- player_toi(pbp)
toi_all <- toi %>% select(playerID,toi_all) %>% rename(toi = toi_all)
toi_5v5 <- toi %>% select(playerID,toi_5v5) %>% rename(toi = toi_5v5)
toi_pp <- toi %>% select(playerID,toi_pp) %>% rename(toi = toi_pp)
toi_pk <- toi %>% select(playerID,toi_pk) %>% rename(toi = toi_pk)
toi_threshold_even <- min(250,(length(unique(pbp$game_id))/16)*10)
toi_threshold_spec <- min(100,(length(unique(pbp$game_id))/16)*2)

# All Situations #
individual_all <- skater_individual(pbp,"all") %>%
  left_join(toi_all) %>%
  mutate(g_60 = (goals/toi)*60,prim_a_60 = (assists_prim/toi)*60,sec_a_60 = (assists_sec/toi)*60,ixg_60 = (ixg/toi)*60,icf_60 = (icf/toi)*60)
individual_all <- na.omit(individual_all)
onice_all <- skater_onice(pbp,"all") %>%
  left_join(toi_all) %>%
  mutate(gf_perc = gf/(gf+ga),xgf_perc = xgf/(xgf+xga),cf_perc = cf/(cf+ca),gf_60 = (gf/toi)*60,ga_60 = (ga/toi)*60,xgf_60 = (xgf/toi)*60,xga_60 = (xga/toi)*60,cf_60 = (cf/toi)*60,ca_60 = (ca/toi)*60) %>%
  select(-toi)
onice_all <- na.omit(onice_all)
player_summary <- individual_all %>% left_join(onice_all,by="playerID")
player_summary <- na.omit(player_summary)
all_summary <- player_summary %>%
  mutate(game_score = ((0.75*goals) + (0.7*assists_prim) + (0.55*assists_sec) + (0.075*isog) + (0.1*ixg) + (0.05*cf) + (0.1*xgf) + (0.15*gf) - (0.05*ca) - (0.1*xga) - (0.15*xga)) / gp)
gc()

# 5v5 #
rapm_even <- readRDS(url("https://github.com/zkautoupdates/autoupdates/raw/main/data/player_rapm_ev_24_25.rds"))
rapm_even <- rapm_even %>%
  rename(RAPM_xGF = xGF_60,RAPM_GF = GF_60,RAPM_CF = CF_60,RAPM_xGA = xGA_60,RAPM_GA = GA_60,RAPM_CA = CA_60)

individual_even <- skater_individual(pbp,"even") %>%
  left_join(toi_5v5) %>%
  mutate(g_60 = (goals/toi)*60,prim_a_60 = (assists_prim/toi)*60,sec_a_60 = (assists_sec/toi)*60,ixg_60 = (ixg/toi)*60,icf_60 = (icf/toi)*60)
individual_even <- na.omit(individual_even)

onice_even <- skater_onice(pbp,"even") %>%
  left_join(toi_5v5) %>%
  mutate(gf_perc = gf/(gf+ga),xgf_perc = xgf/(xgf+xga),cf_perc = cf/(cf+ca),gf_60 = (gf/toi)*60,ga_60 = (ga/toi)*60,xgf_60 = (xgf/toi)*60,xga_60 = (xga/toi)*60,cf_60 = (cf/toi)*60,ca_60 = (ca/toi)*60) %>%
  select(-toi)
onice_even <- na.omit(onice_even)

even_summary <- rapm_even %>% left_join(individual_even,by="playerID") %>% left_join(onice_even,by="playerID")
even_summary <- even_summary %>%
  filter(toi > toi_threshold_even) %>%
  mutate(EVO = ((((0.75*goals) + (0.7*assists_prim) + (0.55*assists_sec) + (0.075*isog) + (0.1*ixg) + (0.05*cf) + (0.1*xgf) + (0.15*gf)) / gp) + ((RAPM_xGF + (RAPM_GF/5))/2))/2,
         EVD = (RAPM_xGA + (RAPM_GA/5))/2)
even_summary <- even_summary[!is.na(even_summary$EVO),]
even_summary <- even_summary[!is.na(even_summary$EVD),]
even_summary <- even_summary %>%
  select(playerID,EVO,EVD,RAPM_xGF,RAPM_GF,RAPM_CF,RAPM_xGA,RAPM_GA,RAPM_CA) %>% arrange(desc(EVO))
rm(individual_even,onice_even,toi_5v5,rapm_even)

# PP #
rapm_pp <- readRDS(url("https://github.com/zkautoupdates/autoupdates/raw/main/data/player_rapm_pp_24_25.rds"))
gc()
rapm_pp <- rapm_pp %>%
  rename(RAPM_xGF = xGF_60,RAPM_GF = GF_60,RAPM_CF = CF_60,RAPM_xGA = xGA_60,RAPM_GA = GA_60,RAPM_CA = CA_60)

individual_pp <- skater_individual(pbp,"pp") %>%
  left_join(toi_pp) %>%
  mutate(g_60 = (goals/toi)*60,prim_a_60 = (assists_prim/toi)*60, sec_a_60 = (assists_sec/toi)*60,isog_60 = (isog/toi)*60,ixg_60 = (ixg/toi)*60,icf_60 = (icf/toi)*60)
individual_pp <- na.omit(individual_pp)

onice_pp <- skater_onice(pbp,"pp") %>%
  left_join(toi_pp) %>%
  mutate(gf_perc = gf/(gf+ga),xgf_perc = xgf/(xgf+xga),cf_perc = cf/(cf+ca),gf_60 = (gf/toi)*60,ga_60 = (ga/toi)*60,xgf_60 = (xgf/toi)*60,xga_60 = (xga/toi)*60,cf_60 = (cf/toi)*60,ca_60 = (ca/toi)*60) %>%
  select(-toi)
onice_pp <- na.omit(onice_pp)

pp_summary <- rapm_pp %>% left_join(individual_pp,by="playerID") %>% left_join(onice_pp,by="playerID")
pp_summary <- pp_summary %>% filter(toi>toi_threshold_spec)
pp_summary <- pp_summary %>%
  mutate(PPO = ((((0.75*g_60) + (0.7*prim_a_60) + (0.55*sec_a_60) + (0.075*isog_60) + (0.1*ixg_60) + (0.05*cf_60) + (0.1*xgf_60) + (0.15*gf_60)) / gp) + ((RAPM_xGF + (RAPM_GF/5))/2))/2)
pp_summary <- pp_summary[!is.na(pp_summary$PPO),]
pp_summary <- pp_summary %>%
  select(playerID,PPO) %>% arrange(desc(PPO))
rm(individual_pp,onice_pp,rapm_pp,toi_pp)

# SH #
rapm_pk <- readRDS(url("https://github.com/zkautoupdates/autoupdates/raw/main/data/player_rapm_sh_24_25.rds"))
gc()
rapm_pk <- rapm_pk %>%
  rename(RAPM_xGF = xGF_60,RAPM_GF = GF_60,RAPM_CF = CF_60,RAPM_xGA = xGA_60,RAPM_GA = GA_60,RAPM_CA = CA_60)

individual_pk <- skater_individual(pbp,"pk") %>% select(playerID,gp)
individual_pk <- na.omit(individual_pk)

onice_pk <- skater_onice(pbp,"pp") %>%
  left_join(toi_pk) %>%
  mutate(ga_60 = (ga/toi)*60,xga_60 = (xga/toi)*60,ca_60 = (ca/toi)*60) %>%
  select(playerID,ga,xga,ca,fa,toi,ga_60,xga_60,ca_60)
onice_pk <- na.omit(onice_pk)

pk_summary <- rapm_pk %>% left_join(individual_pk,by="playerID") %>% left_join(onice_pk,by="playerID")
pk_summary <- pk_summary %>% filter(toi>toi_threshold_spec)
pk_summary <- pk_summary %>%
  mutate(SHD = (RAPM_xGA + (RAPM_GA/5))/2)
pk_summary <- pk_summary[!is.na(pk_summary$SHD),]
pk_summary <- pk_summary %>%
  select(playerID,SHD) %>% arrange(desc(SHD))
rm(individual_pk,onice_pk,rapm_pk,toi_pk)


# FIN #
finishing_summary <- player_summary %>%
  mutate(FIN = (gax/toi)*ixg) %>%
  select(playerID,FIN)

# PEN #
penalty_summary <- player_summary %>%
  mutate(pens_against = (pens_taken/toi),
         pens_for = (pens_drawn/toi),
         PEN = (pens_for - pens_against)) %>%
  select(playerID,PEN)

# Summary #
player_summary <- all_summary %>% 
  left_join(even_summary,by="playerID") %>%
  left_join(pp_summary,by="playerID") %>%
  left_join(pk_summary,by="playerID") %>%
  left_join(finishing_summary,by="playerID") %>%
  left_join(penalty_summary,by="playerID") %>%
  mutate(
    across(PPO:PEN, ~replace_na(.x,0))
  ) %>%
  mutate(EVO = (EVO*1.15),EVD = (EVD*-0.78),PPO = (PPO*0.15),SHD = (SHD*-0.07),FIN = (FIN*0.65),PEN = (PEN*10),
         OVR = EVO + EVD + PPO + SHD + FIN + PEN) %>%
  relocate(OVR,EVO,EVD,PPO,SHD,FIN,PEN,game_score,.after = playerID) %>%
  relocate(RAPM_xGF,RAPM_GF,RAPM_CF,RAPM_xGA,RAPM_GA,RAPM_CA,.after = ca_60) %>%
  arrange(desc(OVR)) %>%
  filter(toi > toi_threshold_even)
player_summary <- na.omit(player_summary)

playernames <- readRDS(url("https://github.com/zackkehl/HockeyZK_dataupdates/raw/main/data/player_data.rds"))

player_summary <- playernames %>% left_join(player_summary, by="playerID")
player_summary <- na.omit(player_summary)
player_stats_24_25 <- player_summary %>% arrange(desc(OVR))
player_stats_24_25 %>% saveRDS("data/player_stats_24_25.rds")

# Goalies #
goalie_games <- min(10,(length(unique(pbp$game_id))/16)/5)
goalie_summary <- goalie_stats(pbp,"all")
goalie_summary <- goalie_summary %>%
  filter(gp>goalie_games) %>%
  mutate(GOA = (gsax/shots_against)*xga/4+0.55) %>%
  relocate(GOA,.after = playerID) %>%
  arrange(desc(GOA))

playernames <- readRDS(url("https://github.com/zackkehl/HockeyZK_dataupdates/raw/main/data/player_data.rds"))

goalie_summary <- playernames %>% left_join(goalie_summary, by="playerID")
goalie_summary <- na.omit(goalie_summary)
goalie_stats_24_25 <- goalie_summary %>% arrange(desc(GOA))
goalie_stats_24_25 %>% saveRDS("data/goalie_stats_24_25.rds")

################################################################################
