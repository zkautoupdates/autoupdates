library(tidyverse)
library(glue)
library(fastDummies)
library(Matrix)
library(glmnet)
library(doParallel)
library(foreach)

# Functions #
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

# RAPM EV #
pbp <- readRDS(url("https://github.com/zackkehl/HockeyZK_dataupdates/raw/main/data/pbp_24_25.rds"))
even_strength <- c("3v3", "4v4", "5v5")

pbp <- pbp %>%
  filter(period < 5)

pbp$home_on_1 <- ifelse(is.na(pbp$home_on_1), "MISSING", pbp$home_on_1)
pbp$home_on_2 <- ifelse(is.na(pbp$home_on_2), "MISSING", pbp$home_on_2)
pbp$home_on_3 <- ifelse(is.na(pbp$home_on_3), "MISSING", pbp$home_on_3)
pbp$home_on_4 <- ifelse(is.na(pbp$home_on_4), "MISSING", pbp$home_on_4)
pbp$home_on_5 <- ifelse(is.na(pbp$home_on_5), "MISSING", pbp$home_on_5)
pbp$home_on_6 <- ifelse(is.na(pbp$home_on_6), "MISSING", pbp$home_on_6)
pbp$away_on_1 <- ifelse(is.na(pbp$away_on_1), "MISSING", pbp$away_on_1)
pbp$away_on_2 <- ifelse(is.na(pbp$away_on_2), "MISSING", pbp$away_on_2)
pbp$away_on_3 <- ifelse(is.na(pbp$away_on_3), "MISSING", pbp$away_on_3)
pbp$away_on_4 <- ifelse(is.na(pbp$away_on_4), "MISSING", pbp$away_on_4)
pbp$away_on_5 <- ifelse(is.na(pbp$away_on_5), "MISSING", pbp$away_on_5)
pbp$away_on_6 <- ifelse(is.na(pbp$away_on_6), "MISSING", pbp$away_on_6)

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
pbp$home_xGF <- ifelse(pbp$event_team_name==pbp$home_team, pbp$xg, 0)
pbp$away_xGF <- ifelse(pbp$event_team_name!=pbp$home_team, pbp$xg, 0)
pbp$home_GF <- ifelse(pbp$event_type=="GOAL" & pbp$event_team_name==pbp$home_team, 1, 0)
pbp$away_GF <- ifelse(pbp$event_type=="GOAL" & pbp$event_team_name==pbp$away_team, 1, 0)
pbp$home_CF <- ifelse(pbp$event_type %in% c("GOAL","SHOT_ON_GOAL","MISSED_SHOT","BLOCKED_SHOT") & pbp$event_team_name==pbp$home_team, 1, 0)
pbp$away_CF <- ifelse(pbp$event_type %in% c("GOAL","SHOT_ON_GOAL","MISSED_SHOT","BLOCKED_SHOT") & pbp$event_team_name==pbp$away_team, 1, 0)
pbp$tied <- ifelse(pbp$home_score==pbp$away_score, 1, 0)
pbp$home_lead_1 <- ifelse(pbp$home_score-pbp$away_score==1, 1, 0)
pbp$home_lead_2 <- ifelse(pbp$home_score-pbp$away_score==2, 1, 0)
pbp$home_lead_3 <- ifelse(pbp$home_score-pbp$away_score>=3, 1, 0)
pbp$away_lead_1 <- ifelse(pbp$home_score-pbp$away_score==(-1), 1, 0)
pbp$away_lead_2 <- ifelse(pbp$home_score-pbp$away_score==(-2), 1, 0)
pbp$away_lead_3 <- ifelse(pbp$home_score-pbp$away_score<=(-3), 1, 0)
pbp$Five <- ifelse(pbp$strength_state=="5v5", 1, 0)
pbp$Four <- ifelse(pbp$strength_state=="4v4", 1, 0)
pbp$Three <- ifelse(pbp$strength_state=="3v3", 1, 0)
pbp$home_xGF[is.na(pbp$home_xGF)] <- 0
pbp$away_xGF[is.na(pbp$away_xGF)] <- 0
pbp$event_length[is.na(pbp$event_length)] <- 0

pbp_ev <- pbp %>% filter(strength_state %in% even_strength)
rm(pbp)

grouped_shifts <- pbp_ev %>%
  group_by(game_id, shift_change_index, period, game_score_state, home_on_1, home_on_2, home_on_3, home_on_4, home_on_5, home_on_6, 
           away_on_1, away_on_2, away_on_3, away_on_4, away_on_5, away_on_6) %>%
  summarise(shift_length = sum(event_length), homexGF = sum(home_xGF), awayxGF = sum(away_xGF),
            homeGF = sum(home_GF), awayGF = sum(away_GF), homeCF = sum(home_CF), awayCF = sum(away_CF),
            Home_Up_1 = max(home_lead_1), Home_Up_2 = max(home_lead_2), Home_Up_3 = max(home_lead_3),
            Away_Up_1 = max(away_lead_1), Away_Up_2 = max(away_lead_2), Away_Up_3 = max(away_lead_3), Tied = max(tied), 
            State_5v5 = max(Five), State_4v4 = max(Four), State_3v3 = max(Three)) %>%
  filter(shift_length > 0)

home_as_off <- grouped_shifts %>%
  rename(offense_1 = home_on_1, offense_2 = home_on_2, offense_3 = home_on_3, offense_4 = home_on_4, offense_5 = home_on_5, offense_6 = home_on_6,
         defense_1 = away_on_1, defense_2 = away_on_2, defense_3 = away_on_3, defense_4 = away_on_4, defense_5 = away_on_5, defense_6 = away_on_6,
         Up_1 = Home_Up_1, Up_2 = Home_Up_2, Up_3 = Home_Up_3, Down_1 = Away_Up_1, Down_2 = Away_Up_2, Down_3 = Away_Up_3, xGF = homexGF, GF = homeGF, CF = homeCF) %>%
  select(game_id, shift_change_index, period, game_score_state,
         offense_1, offense_2, offense_3, offense_4, offense_5, offense_6, 
         defense_1, defense_2, defense_3, defense_4, defense_5, defense_6, 
         xGF, GF, CF, shift_length, shift_change_index, Tied, State_5v5, State_4v4, State_3v3, 
         Up_1, Up_2, Up_3, Down_1, Down_2, Down_3) %>%
  mutate(xGF_60 = xGF*3600/shift_length, GF_60 = GF*3600/shift_length, CF_60 = CF*3600/shift_length, is_home = 1)

away_as_off <- grouped_shifts %>%
  rename(offense_1 = away_on_1, offense_2 = away_on_2, offense_3 = away_on_3, offense_4 = away_on_4, offense_5 = away_on_5, offense_6 = away_on_6,
         defense_1 = home_on_1, defense_2 = home_on_2, defense_3 = home_on_3, defense_4 = home_on_4, defense_5 = home_on_5, defense_6 = home_on_6,
         Up_1 = Away_Up_1, Up_2 = Away_Up_2, Up_3 = Away_Up_3, Down_1 = Home_Up_1, Down_2 = Home_Up_2, Down_3 = Home_Up_3, xGF = awayxGF, GF = awayGF, CF = awayCF) %>%
  select(game_id, shift_change_index, period, game_score_state,
         offense_1, offense_2, offense_3, offense_4, offense_5, offense_6, 
         defense_1, defense_2, defense_3, defense_4, defense_5, defense_6, 
         xGF, GF, CF, shift_length, shift_change_index, Tied, State_5v5, State_4v4, State_3v3, 
         Up_1, Up_2, Up_3, Down_1, Down_2, Down_3) %>%
  mutate(xGF_60 = xGF*3600/shift_length, GF_60 = GF*3600/shift_length, CF_60 = CF*3600/shift_length, is_home = 0)

shifts_combined <- full_join(home_as_off, away_as_off)
rm(pbp_ev,home_as_off,away_as_off,grouped_shifts)

n_cores <- parallel::detectCores()
cl <- makeCluster(n_cores)
registerDoParallel(cl)
getDoParWorkers()

shifts_subset = subset(shifts_combined, select = c(offense_1:offense_6,defense_1:defense_6,shift_length,State_4v4,State_3v3,Up_1:Down_3,xGF_60,GF_60,CF_60,is_home))
rm(shifts_combined)

shifts_combined_dummies_off <- dummy_cols(shifts_subset, select_columns = c("offense_1"))
gc()
shifts_combined_dummies_off <- dummy_cols(shifts_combined_dummies_off, select_columns = c("offense_2"))
gc()
shifts_combined_dummies_off <- dummy_cols(shifts_combined_dummies_off, select_columns = c("offense_3"))
gc()
shifts_combined_dummies_off <- dummy_cols(shifts_combined_dummies_off, select_columns = c("offense_4"))
gc()
shifts_combined_dummies_off <- dummy_cols(shifts_combined_dummies_off, select_columns = c("offense_5"))
gc()
shifts_combined_dummies_off <- dummy_cols(shifts_combined_dummies_off, select_columns = c("offense_6"))
gc()

shifts_combined_dummies_def <- dummy_cols(shifts_subset, select_columns = c("defense_1"))
gc()
shifts_combined_dummies_def <- dummy_cols(shifts_combined_dummies_def, select_columns = c("defense_2"))
gc()
shifts_combined_dummies_def <- dummy_cols(shifts_combined_dummies_def, select_columns = c("defense_3"))
gc()
shifts_combined_dummies_def <- dummy_cols(shifts_combined_dummies_def, select_columns = c("defense_4"))
gc()
shifts_combined_dummies_def <- dummy_cols(shifts_combined_dummies_def, select_columns = c("defense_5"))
gc()
shifts_combined_dummies_def <- dummy_cols(shifts_combined_dummies_def, select_columns = c("defense_6"))
gc()

shifts_combined_dummies_off = subset(shifts_combined_dummies_off, select = -c(offense_1,offense_2,offense_3,offense_4,offense_5,offense_6,defense_1,defense_2,defense_3,defense_4,defense_5,defense_6))
gc()
shifts_combined_dummies_def = subset(shifts_combined_dummies_def, select = -c(offense_1,offense_2,offense_3,offense_4,offense_5,offense_6,defense_1,defense_2,defense_3,defense_4,defense_5,defense_6,shift_length,State_4v4,State_3v3,Up_1:Down_3,xGF_60,GF_60,CF_60,is_home))
gc()
shifts_combined_dummies <- cbind(shifts_combined_dummies_off, shifts_combined_dummies_def)
rm(shifts_subset,shifts_combined_dummies_off,shifts_combined_dummies_def)
gc()

colnames(shifts_combined_dummies) = gsub("offense_1", "offense_", colnames(shifts_combined_dummies))
colnames(shifts_combined_dummies) = gsub("offense_2", "offense_", colnames(shifts_combined_dummies))
colnames(shifts_combined_dummies) = gsub("offense_3", "offense_", colnames(shifts_combined_dummies))
colnames(shifts_combined_dummies) = gsub("offense_4", "offense_", colnames(shifts_combined_dummies))
colnames(shifts_combined_dummies) = gsub("offense_5", "offense_", colnames(shifts_combined_dummies))
colnames(shifts_combined_dummies) = gsub("offense_6", "offense_", colnames(shifts_combined_dummies))
colnames(shifts_combined_dummies) = gsub("defense_1", "defense_", colnames(shifts_combined_dummies))
colnames(shifts_combined_dummies) = gsub("defense_2", "defense_", colnames(shifts_combined_dummies))
colnames(shifts_combined_dummies) = gsub("defense_3", "defense_", colnames(shifts_combined_dummies))
colnames(shifts_combined_dummies) = gsub("defense_4", "defense_", colnames(shifts_combined_dummies))
colnames(shifts_combined_dummies) = gsub("defense_5", "defense_", colnames(shifts_combined_dummies))
colnames(shifts_combined_dummies) = gsub("defense_6", "defense_", colnames(shifts_combined_dummies))
gc()

shifts_combined_dummies <- as.data.frame(lapply(split.default(shifts_combined_dummies, names(shifts_combined_dummies)), function(x) Reduce(`+`, x)))
gc()

shifts_combined_dummies <- shifts_combined_dummies %>% select(-contains("Goalie"))
shifts_combined_dummies <- shifts_combined_dummies %>% select(-contains("Missing"))

xGF60 <- as.numeric(c(shifts_combined_dummies$xGF_60))
GF60 <- as.numeric(c(shifts_combined_dummies$GF_60))
CF60 <- as.numeric(c(shifts_combined_dummies$CF_60))
shift_length <- as.numeric(c(shifts_combined_dummies$shift_length))
subsetted_dummies = subset(shifts_combined_dummies, select = -c(shift_length, xGF_60, GF_60, CF_60))
rm(shifts_combined_dummies)
gc()

RAPM_xGF <- as.matrix(subsetted_dummies)
Sparse_RAPM_xGF <- Matrix(RAPM_xGF, sparse = TRUE)
rm(RAPM_xGF)

RAPM_GF <- as.matrix(subsetted_dummies)
Sparse_RAPM_GF <- Matrix(RAPM_GF, sparse = TRUE)
rm(RAPM_GF)

RAPM_CF <- as.matrix(subsetted_dummies)
Sparse_RAPM_CF <- Matrix(RAPM_CF, sparse = TRUE)
rm(RAPM_CF)
gc()

rm(subsetted_dummies)

Cross_Validated_Results_xGF <- cv.glmnet(x=Sparse_RAPM_xGF, y=xGF60, weights=shift_length, alpha=0, nfolds=10, standardize=FALSE, parallel=TRUE)
gc()
Cross_Validated_Results_GF <- cv.glmnet(x=Sparse_RAPM_GF, y=GF60, weights=shift_length, alpha=0, nfolds=10, standardize=FALSE, parallel=TRUE)
gc()
Cross_Validated_Results_CF <- cv.glmnet(x=Sparse_RAPM_CF, y=CF60, weights=shift_length, alpha=0, nfolds=10, standardize=FALSE, parallel=TRUE)
gc()

Run_RAPM_xGF <- glmnet(x=Sparse_RAPM_xGF, y=xGF60, weights=shift_length, lambda = Cross_Validated_Results_xGF[["lambda.min"]], alpha=0, nfolds=10, standardize=FALSE, parallel=TRUE)
Run_RAPM_GF <- glmnet(x=Sparse_RAPM_GF, y=GF60, weights=shift_length, lambda = Cross_Validated_Results_xGF[["lambda.min"]], alpha=0, nfolds=10, standardize=FALSE, parallel=TRUE)
Run_RAPM_CF <- glmnet(x=Sparse_RAPM_CF, y=CF60, weights=shift_length, lambda = Cross_Validated_Results_xGF[["lambda.min"]], alpha=0, nfolds=10, standardize=FALSE, parallel=TRUE)

stopCluster(cl)
registerDoSEQ()

RAPM_xGF_coefficients <- as.data.frame(as.matrix(coef(Run_RAPM_xGF)))
RAPM_GF_coefficients <- as.data.frame(as.matrix(coef(Run_RAPM_GF)))
RAPM_CF_coefficients <- as.data.frame(as.matrix(coef(Run_RAPM_CF)))

Binded_Coefficients_xGF <- cbind(rownames(RAPM_xGF_coefficients), RAPM_xGF_coefficients) %>%
  rename(Player = `rownames(RAPM_xGF_coefficients)`, xGF_60 = s0)
Binded_Coefficients_GF <- cbind(rownames(RAPM_GF_coefficients), RAPM_GF_coefficients) %>%
  rename(Player = `rownames(RAPM_GF_coefficients)`, GF_60 = s0)
Binded_Coefficients_CF <- cbind(rownames(RAPM_CF_coefficients), RAPM_CF_coefficients) %>%
  rename(Player = `rownames(RAPM_CF_coefficients)`, CF_60 = s0)

offense_RAPM_xGF <- Binded_Coefficients_xGF %>%
  filter(grepl("offense", Binded_Coefficients_xGF$Player))
offense_RAPM_GF <- Binded_Coefficients_GF %>%
  filter(grepl("offense", Binded_Coefficients_GF$Player))
offense_RAPM_CF <- Binded_Coefficients_CF %>%
  filter(grepl("offense", Binded_Coefficients_CF$Player))

offense_RAPM_xGF$Player = str_replace_all(offense_RAPM_xGF$Player, "offense__", "")
offense_RAPM_GF$Player = str_replace_all(offense_RAPM_GF$Player, "offense__", "")
offense_RAPM_CF$Player = str_replace_all(offense_RAPM_CF$Player, "offense__", "")

defense_RAPM_xGA <- Binded_Coefficients_xGF %>%
  filter(grepl("defense", Binded_Coefficients_xGF$Player)) %>%
  rename(xGA_60 = xGF_60)
defense_RAPM_GA <- Binded_Coefficients_GF %>%
  filter(grepl("defense", Binded_Coefficients_GF$Player)) %>%
  rename(GA_60 = GF_60)
defense_RAPM_CA <- Binded_Coefficients_CF %>%
  filter(grepl("defense", Binded_Coefficients_CF$Player)) %>%
  rename(CA_60 = CF_60)

defense_RAPM_xGA$Player = str_replace_all(defense_RAPM_xGA$Player, "defense__", "")
defense_RAPM_GA$Player = str_replace_all(defense_RAPM_GA$Player, "defense__", "")
defense_RAPM_CA$Player = str_replace_all(defense_RAPM_CA$Player, "defense__", "")

joined_RAPM_xG <- inner_join(offense_RAPM_xGF, defense_RAPM_xGA, by="Player")
joined_RAPM_G <- inner_join(offense_RAPM_GF, defense_RAPM_GA, by="Player")
joined_RAPM_C <- inner_join(offense_RAPM_CF, defense_RAPM_CA, by="Player")

joined_RAPM_xG$xGPM_60 <- joined_RAPM_xG$xGF_60 - joined_RAPM_xG$xGA_60
joined_RAPM_G$GPM_60 <- joined_RAPM_G$GF_60 - joined_RAPM_G$GA_60
joined_RAPM_C$CPM_60 <- joined_RAPM_C$CF_60 - joined_RAPM_C$CA_60

joined_RAPM <- inner_join(joined_RAPM_xG, joined_RAPM_G, by="Player")
joined_RAPM <- inner_join(joined_RAPM, joined_RAPM_C, by="Player")

joined_RAPM <- joined_RAPM %>%
  arrange(desc(xGPM_60))

joined_RAPM$Player = as.numeric(as.character(joined_RAPM$Player))

rapm_even <- joined_RAPM %>% rename(playerID = Player)
rm(joined_RAPM,cl,n_cores,defense_RAPM_xGA,defense_RAPM_GA,defense_RAPM_CA,joined_RAPM_G,joined_RAPM_C,joined_RAPM_xG,offense_RAPM_xGF,offense_RAPM_GF,offense_RAPM_CF,Binded_Coefficients_GF,Binded_Coefficients_CF,Binded_Coefficients_xGF,Cross_Validated_Results_GF,Cross_Validated_Results_CF,Cross_Validated_Results_xGF,RAPM_GF_coefficients,RAPM_CF_coefficients,RAPM_xGF_coefficients,Run_RAPM_xGF,Run_RAPM_GF,Run_RAPM_CF,Sparse_RAPM_xGF,even_strength,GF60,shift_length,xGF60,Sparse_RAPM_GF,Sparse_RAPM_CF,CF60)
gc()

rapm_even %>% saveRDS("data/player_rapm_ev_24_25.rds")

## RAPM PP ##
pbp <- readRDS(url("https://github.com/zackkehl/HockeyZK_dataupdates/raw/main/data/pbp_24_25.rds"))
power_play <- c("5v4","5v3","4v3","6v4","6v3")

pbp <- pbp %>%
  filter(period < 5)

pbp$home_on_1 <- ifelse(is.na(pbp$home_on_1), "MISSING", pbp$home_on_1)
pbp$home_on_2 <- ifelse(is.na(pbp$home_on_2), "MISSING", pbp$home_on_2)
pbp$home_on_3 <- ifelse(is.na(pbp$home_on_3), "MISSING", pbp$home_on_3)
pbp$home_on_4 <- ifelse(is.na(pbp$home_on_4), "MISSING", pbp$home_on_4)
pbp$home_on_5 <- ifelse(is.na(pbp$home_on_5), "MISSING", pbp$home_on_5)
pbp$home_on_6 <- ifelse(is.na(pbp$home_on_6), "MISSING", pbp$home_on_6)
pbp$away_on_1 <- ifelse(is.na(pbp$away_on_1), "MISSING", pbp$away_on_1)
pbp$away_on_2 <- ifelse(is.na(pbp$away_on_2), "MISSING", pbp$away_on_2)
pbp$away_on_3 <- ifelse(is.na(pbp$away_on_3), "MISSING", pbp$away_on_3)
pbp$away_on_4 <- ifelse(is.na(pbp$away_on_4), "MISSING", pbp$away_on_4)
pbp$away_on_5 <- ifelse(is.na(pbp$away_on_5), "MISSING", pbp$away_on_5)
pbp$away_on_6 <- ifelse(is.na(pbp$away_on_6), "MISSING", pbp$away_on_6)

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
pbp$home_xGF <- ifelse(pbp$event_team_name==pbp$home_team, pbp$xg, 0)
pbp$away_xGF <- ifelse(pbp$event_team_name!=pbp$home_team, pbp$xg, 0)
pbp$home_GF <- ifelse(pbp$event_type=="GOAL" & pbp$event_team_name==pbp$home_team, 1, 0)
pbp$away_GF <- ifelse(pbp$event_type=="GOAL" & pbp$event_team_name==pbp$away_team, 1, 0)
pbp$home_CF <- ifelse(pbp$event_type %in% c("GOAL","SHOT_ON_GOAL","MISSED_SHOT","BLOCKED_SHOT") & pbp$event_team_name==pbp$home_team, 1, 0)
pbp$away_CF <- ifelse(pbp$event_type %in% c("GOAL","SHOT_ON_GOAL","MISSED_SHOT","BLOCKED_SHOT") & pbp$event_team_name==pbp$away_team, 1, 0)
pbp$tied <- ifelse(pbp$home_score==pbp$away_score, 1, 0)
pbp$home_lead_1 <- ifelse(pbp$home_score-pbp$away_score==1, 1, 0)
pbp$home_lead_2 <- ifelse(pbp$home_score-pbp$away_score==2, 1, 0)
pbp$home_lead_3 <- ifelse(pbp$home_score-pbp$away_score>=3, 1, 0)
pbp$away_lead_1 <- ifelse(pbp$home_score-pbp$away_score==(-1), 1, 0)
pbp$away_lead_2 <- ifelse(pbp$home_score-pbp$away_score==(-2), 1, 0)
pbp$away_lead_3 <- ifelse(pbp$home_score-pbp$away_score<=(-3), 1, 0)
pbp$Five <- ifelse(pbp$strength_state=="5v5", 1, 0)
pbp$Four <- ifelse(pbp$strength_state=="4v4", 1, 0)
pbp$Three <- ifelse(pbp$strength_state=="3v3", 1, 0)
pbp$home_xGF[is.na(pbp$home_xGF)] <- 0
pbp$away_xGF[is.na(pbp$away_xGF)] <- 0
pbp$event_length[is.na(pbp$event_length)] <- 0

pbp_ev <- pbp %>% filter(strength_state %in% power_play)
rm(pbp)

grouped_shifts <- pbp_ev %>%
  group_by(game_id, shift_change_index, period, game_score_state, home_on_1, home_on_2, home_on_3, home_on_4, home_on_5, home_on_6, 
           away_on_1, away_on_2, away_on_3, away_on_4, away_on_5, away_on_6) %>%
  summarise(shift_length = sum(event_length), homexGF = sum(home_xGF), awayxGF = sum(away_xGF),
            homeGF = sum(home_GF), awayGF = sum(away_GF), homeCF = sum(home_CF), awayCF = sum(away_CF),
            Home_Up_1 = max(home_lead_1), Home_Up_2 = max(home_lead_2), Home_Up_3 = max(home_lead_3),
            Away_Up_1 = max(away_lead_1), Away_Up_2 = max(away_lead_2), Away_Up_3 = max(away_lead_3), Tied = max(tied), 
            State_5v5 = max(Five), State_4v4 = max(Four), State_3v3 = max(Three)) %>%
  filter(shift_length > 0)

home_as_off <- grouped_shifts %>%
  rename(offense_1 = home_on_1, offense_2 = home_on_2, offense_3 = home_on_3, offense_4 = home_on_4, offense_5 = home_on_5, offense_6 = home_on_6,
         defense_1 = away_on_1, defense_2 = away_on_2, defense_3 = away_on_3, defense_4 = away_on_4, defense_5 = away_on_5, defense_6 = away_on_6,
         Up_1 = Home_Up_1, Up_2 = Home_Up_2, Up_3 = Home_Up_3, Down_1 = Away_Up_1, Down_2 = Away_Up_2, Down_3 = Away_Up_3, xGF = homexGF, GF = homeGF, CF = homeCF) %>%
  select(game_id, shift_change_index, period, game_score_state,
         offense_1, offense_2, offense_3, offense_4, offense_5, offense_6, 
         defense_1, defense_2, defense_3, defense_4, defense_5, defense_6, 
         xGF, GF, CF, shift_length, shift_change_index, Tied, State_5v5, State_4v4, State_3v3, 
         Up_1, Up_2, Up_3, Down_1, Down_2, Down_3) %>%
  mutate(xGF_60 = xGF*3600/shift_length, GF_60 = GF*3600/shift_length, CF_60 = CF*3600/shift_length, is_home = 1)

away_as_off <- grouped_shifts %>%
  rename(offense_1 = away_on_1, offense_2 = away_on_2, offense_3 = away_on_3, offense_4 = away_on_4, offense_5 = away_on_5, offense_6 = away_on_6,
         defense_1 = home_on_1, defense_2 = home_on_2, defense_3 = home_on_3, defense_4 = home_on_4, defense_5 = home_on_5, defense_6 = home_on_6,
         Up_1 = Away_Up_1, Up_2 = Away_Up_2, Up_3 = Away_Up_3, Down_1 = Home_Up_1, Down_2 = Home_Up_2, Down_3 = Home_Up_3, xGF = awayxGF, GF = awayGF, CF = awayCF) %>%
  select(game_id, shift_change_index, period, game_score_state,
         offense_1, offense_2, offense_3, offense_4, offense_5, offense_6, 
         defense_1, defense_2, defense_3, defense_4, defense_5, defense_6, 
         xGF, GF, CF, shift_length, shift_change_index, Tied, State_5v5, State_4v4, State_3v3, 
         Up_1, Up_2, Up_3, Down_1, Down_2, Down_3) %>%
  mutate(xGF_60 = xGF*3600/shift_length, GF_60 = GF*3600/shift_length, CF_60 = CF*3600/shift_length, is_home = 0)

shifts_combined <- full_join(home_as_off, away_as_off)
rm(pbp_ev,home_as_off,away_as_off,grouped_shifts)

n_cores <- parallel::detectCores()
cl <- makeCluster(n_cores)
registerDoParallel(cl)
getDoParWorkers()

shifts_subset = subset(shifts_combined, select = c(offense_1:offense_6,defense_1:defense_6,shift_length,State_4v4,State_3v3,Up_1:Down_3,xGF_60,GF_60,CF_60,is_home))
rm(shifts_combined)

shifts_combined_dummies_off <- dummy_cols(shifts_subset, select_columns = c("offense_1"))
gc()
shifts_combined_dummies_off <- dummy_cols(shifts_combined_dummies_off, select_columns = c("offense_2"))
gc()
shifts_combined_dummies_off <- dummy_cols(shifts_combined_dummies_off, select_columns = c("offense_3"))
gc()
shifts_combined_dummies_off <- dummy_cols(shifts_combined_dummies_off, select_columns = c("offense_4"))
gc()
shifts_combined_dummies_off <- dummy_cols(shifts_combined_dummies_off, select_columns = c("offense_5"))
gc()
shifts_combined_dummies_off <- dummy_cols(shifts_combined_dummies_off, select_columns = c("offense_6"))
gc()

shifts_combined_dummies_def <- dummy_cols(shifts_subset, select_columns = c("defense_1"))
gc()
shifts_combined_dummies_def <- dummy_cols(shifts_combined_dummies_def, select_columns = c("defense_2"))
gc()
shifts_combined_dummies_def <- dummy_cols(shifts_combined_dummies_def, select_columns = c("defense_3"))
gc()
shifts_combined_dummies_def <- dummy_cols(shifts_combined_dummies_def, select_columns = c("defense_4"))
gc()
shifts_combined_dummies_def <- dummy_cols(shifts_combined_dummies_def, select_columns = c("defense_5"))
gc()
shifts_combined_dummies_def <- dummy_cols(shifts_combined_dummies_def, select_columns = c("defense_6"))
gc()

shifts_combined_dummies_off = subset(shifts_combined_dummies_off, select = -c(offense_1,offense_2,offense_3,offense_4,offense_5,offense_6,defense_1,defense_2,defense_3,defense_4,defense_5,defense_6))
gc()
shifts_combined_dummies_def = subset(shifts_combined_dummies_def, select = -c(offense_1,offense_2,offense_3,offense_4,offense_5,offense_6,defense_1,defense_2,defense_3,defense_4,defense_5,defense_6,shift_length,State_4v4,State_3v3,Up_1:Down_3,xGF_60,GF_60,CF_60,is_home))
gc()
shifts_combined_dummies <- cbind(shifts_combined_dummies_off, shifts_combined_dummies_def)
rm(shifts_subset,shifts_combined_dummies_off,shifts_combined_dummies_def)
gc()

colnames(shifts_combined_dummies) = gsub("offense_1", "offense_", colnames(shifts_combined_dummies))
colnames(shifts_combined_dummies) = gsub("offense_2", "offense_", colnames(shifts_combined_dummies))
colnames(shifts_combined_dummies) = gsub("offense_3", "offense_", colnames(shifts_combined_dummies))
colnames(shifts_combined_dummies) = gsub("offense_4", "offense_", colnames(shifts_combined_dummies))
colnames(shifts_combined_dummies) = gsub("offense_5", "offense_", colnames(shifts_combined_dummies))
colnames(shifts_combined_dummies) = gsub("offense_6", "offense_", colnames(shifts_combined_dummies))
colnames(shifts_combined_dummies) = gsub("defense_1", "defense_", colnames(shifts_combined_dummies))
colnames(shifts_combined_dummies) = gsub("defense_2", "defense_", colnames(shifts_combined_dummies))
colnames(shifts_combined_dummies) = gsub("defense_3", "defense_", colnames(shifts_combined_dummies))
colnames(shifts_combined_dummies) = gsub("defense_4", "defense_", colnames(shifts_combined_dummies))
colnames(shifts_combined_dummies) = gsub("defense_5", "defense_", colnames(shifts_combined_dummies))
colnames(shifts_combined_dummies) = gsub("defense_6", "defense_", colnames(shifts_combined_dummies))
gc()

shifts_combined_dummies <- as.data.frame(lapply(split.default(shifts_combined_dummies, names(shifts_combined_dummies)), function(x) Reduce(`+`, x)))
gc()

shifts_combined_dummies <- shifts_combined_dummies %>% select(-contains("Goalie"))
shifts_combined_dummies <- shifts_combined_dummies %>% select(-contains("Missing"))

xGF60 <- as.numeric(c(shifts_combined_dummies$xGF_60))
GF60 <- as.numeric(c(shifts_combined_dummies$GF_60))
CF60 <- as.numeric(c(shifts_combined_dummies$CF_60))
shift_length <- as.numeric(c(shifts_combined_dummies$shift_length))
subsetted_dummies = subset(shifts_combined_dummies, select = -c(shift_length, xGF_60, GF_60, CF_60))
rm(shifts_combined_dummies)
gc()

RAPM_xGF <- as.matrix(subsetted_dummies)
Sparse_RAPM_xGF <- Matrix(RAPM_xGF, sparse = TRUE)
rm(RAPM_xGF)

RAPM_GF <- as.matrix(subsetted_dummies)
Sparse_RAPM_GF <- Matrix(RAPM_GF, sparse = TRUE)
rm(RAPM_GF)

RAPM_CF <- as.matrix(subsetted_dummies)
Sparse_RAPM_CF <- Matrix(RAPM_CF, sparse = TRUE)
rm(RAPM_CF)
gc()

rm(subsetted_dummies)

Cross_Validated_Results_xGF <- cv.glmnet(x=Sparse_RAPM_xGF, y=xGF60, weights=shift_length, alpha=0, nfolds=10, standardize=FALSE, parallel=TRUE)
gc()
Cross_Validated_Results_GF <- cv.glmnet(x=Sparse_RAPM_GF, y=GF60, weights=shift_length, alpha=0, nfolds=10, standardize=FALSE, parallel=TRUE)
gc()
Cross_Validated_Results_CF <- cv.glmnet(x=Sparse_RAPM_CF, y=CF60, weights=shift_length, alpha=0, nfolds=10, standardize=FALSE, parallel=TRUE)
gc()

Run_RAPM_xGF <- glmnet(x=Sparse_RAPM_xGF, y=xGF60, weights=shift_length, lambda = Cross_Validated_Results_xGF[["lambda.min"]], alpha=0, nfolds=10, standardize=FALSE, parallel=TRUE)
Run_RAPM_GF <- glmnet(x=Sparse_RAPM_GF, y=GF60, weights=shift_length, lambda = Cross_Validated_Results_xGF[["lambda.min"]], alpha=0, nfolds=10, standardize=FALSE, parallel=TRUE)
Run_RAPM_CF <- glmnet(x=Sparse_RAPM_CF, y=CF60, weights=shift_length, lambda = Cross_Validated_Results_xGF[["lambda.min"]], alpha=0, nfolds=10, standardize=FALSE, parallel=TRUE)

stopCluster(cl)
registerDoSEQ()

RAPM_xGF_coefficients <- as.data.frame(as.matrix(coef(Run_RAPM_xGF)))
RAPM_GF_coefficients <- as.data.frame(as.matrix(coef(Run_RAPM_GF)))
RAPM_CF_coefficients <- as.data.frame(as.matrix(coef(Run_RAPM_CF)))

Binded_Coefficients_xGF <- cbind(rownames(RAPM_xGF_coefficients), RAPM_xGF_coefficients) %>%
  rename(Player = `rownames(RAPM_xGF_coefficients)`, xGF_60 = s0)
Binded_Coefficients_GF <- cbind(rownames(RAPM_GF_coefficients), RAPM_GF_coefficients) %>%
  rename(Player = `rownames(RAPM_GF_coefficients)`, GF_60 = s0)
Binded_Coefficients_CF <- cbind(rownames(RAPM_CF_coefficients), RAPM_CF_coefficients) %>%
  rename(Player = `rownames(RAPM_CF_coefficients)`, CF_60 = s0)

offense_RAPM_xGF <- Binded_Coefficients_xGF %>%
  filter(grepl("offense", Binded_Coefficients_xGF$Player))
offense_RAPM_GF <- Binded_Coefficients_GF %>%
  filter(grepl("offense", Binded_Coefficients_GF$Player))
offense_RAPM_CF <- Binded_Coefficients_CF %>%
  filter(grepl("offense", Binded_Coefficients_CF$Player))

offense_RAPM_xGF$Player = str_replace_all(offense_RAPM_xGF$Player, "offense__", "")
offense_RAPM_GF$Player = str_replace_all(offense_RAPM_GF$Player, "offense__", "")
offense_RAPM_CF$Player = str_replace_all(offense_RAPM_CF$Player, "offense__", "")

defense_RAPM_xGA <- Binded_Coefficients_xGF %>%
  filter(grepl("defense", Binded_Coefficients_xGF$Player)) %>%
  rename(xGA_60 = xGF_60)
defense_RAPM_GA <- Binded_Coefficients_GF %>%
  filter(grepl("defense", Binded_Coefficients_GF$Player)) %>%
  rename(GA_60 = GF_60)
defense_RAPM_CA <- Binded_Coefficients_CF %>%
  filter(grepl("defense", Binded_Coefficients_CF$Player)) %>%
  rename(CA_60 = CF_60)

defense_RAPM_xGA$Player = str_replace_all(defense_RAPM_xGA$Player, "defense__", "")
defense_RAPM_GA$Player = str_replace_all(defense_RAPM_GA$Player, "defense__", "")
defense_RAPM_CA$Player = str_replace_all(defense_RAPM_CA$Player, "defense__", "")

joined_RAPM_xG <- inner_join(offense_RAPM_xGF, defense_RAPM_xGA, by="Player")
joined_RAPM_G <- inner_join(offense_RAPM_GF, defense_RAPM_GA, by="Player")
joined_RAPM_C <- inner_join(offense_RAPM_CF, defense_RAPM_CA, by="Player")

joined_RAPM_xG$xGPM_60 <- joined_RAPM_xG$xGF_60 - joined_RAPM_xG$xGA_60
joined_RAPM_G$GPM_60 <- joined_RAPM_G$GF_60 - joined_RAPM_G$GA_60
joined_RAPM_C$CPM_60 <- joined_RAPM_C$CF_60 - joined_RAPM_C$CA_60

joined_RAPM <- inner_join(joined_RAPM_xG, joined_RAPM_G, by="Player")
joined_RAPM <- inner_join(joined_RAPM, joined_RAPM_C, by="Player")

joined_RAPM <- joined_RAPM %>%
  arrange(desc(xGPM_60))

joined_RAPM$Player = as.numeric(as.character(joined_RAPM$Player))

rapm_pp <- joined_RAPM %>% rename(playerID = Player)
rm(joined_RAPM,cl,n_cores,defense_RAPM_xGA,defense_RAPM_GA,defense_RAPM_CA,joined_RAPM_G,joined_RAPM_C,joined_RAPM_xG,offense_RAPM_xGF,offense_RAPM_GF,offense_RAPM_CF,Binded_Coefficients_GF,Binded_Coefficients_CF,Binded_Coefficients_xGF,Cross_Validated_Results_GF,Cross_Validated_Results_CF,Cross_Validated_Results_xGF,RAPM_GF_coefficients,RAPM_CF_coefficients,RAPM_xGF_coefficients,Run_RAPM_xGF,Run_RAPM_GF,Run_RAPM_CF,Sparse_RAPM_xGF,even_strength,GF60,shift_length,xGF60,Sparse_RAPM_GF,Sparse_RAPM_CF,CF60)
gc()

rapm_pp %>% saveRDS("data/player_rapm_pp_24_25.rds")

## RAPM SH ##
pbp <- readRDS(url("https://github.com/zackkehl/HockeyZK_dataupdates/raw/main/data/pbp_24_25.rds"))
penalty_kill <- c("4v5","3v5","3v4","4v6","3v6")

pbp <- pbp %>%
  filter(period < 5)

pbp$home_on_1 <- ifelse(is.na(pbp$home_on_1), "MISSING", pbp$home_on_1)
pbp$home_on_2 <- ifelse(is.na(pbp$home_on_2), "MISSING", pbp$home_on_2)
pbp$home_on_3 <- ifelse(is.na(pbp$home_on_3), "MISSING", pbp$home_on_3)
pbp$home_on_4 <- ifelse(is.na(pbp$home_on_4), "MISSING", pbp$home_on_4)
pbp$home_on_5 <- ifelse(is.na(pbp$home_on_5), "MISSING", pbp$home_on_5)
pbp$home_on_6 <- ifelse(is.na(pbp$home_on_6), "MISSING", pbp$home_on_6)
pbp$away_on_1 <- ifelse(is.na(pbp$away_on_1), "MISSING", pbp$away_on_1)
pbp$away_on_2 <- ifelse(is.na(pbp$away_on_2), "MISSING", pbp$away_on_2)
pbp$away_on_3 <- ifelse(is.na(pbp$away_on_3), "MISSING", pbp$away_on_3)
pbp$away_on_4 <- ifelse(is.na(pbp$away_on_4), "MISSING", pbp$away_on_4)
pbp$away_on_5 <- ifelse(is.na(pbp$away_on_5), "MISSING", pbp$away_on_5)
pbp$away_on_6 <- ifelse(is.na(pbp$away_on_6), "MISSING", pbp$away_on_6)

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
pbp$home_xGF <- ifelse(pbp$event_team_name==pbp$home_team, pbp$xg, 0)
pbp$away_xGF <- ifelse(pbp$event_team_name!=pbp$home_team, pbp$xg, 0)
pbp$home_GF <- ifelse(pbp$event_type=="GOAL" & pbp$event_team_name==pbp$home_team, 1, 0)
pbp$away_GF <- ifelse(pbp$event_type=="GOAL" & pbp$event_team_name==pbp$away_team, 1, 0)
pbp$home_CF <- ifelse(pbp$event_type %in% c("GOAL","SHOT_ON_GOAL","MISSED_SHOT","BLOCKED_SHOT") & pbp$event_team_name==pbp$home_team, 1, 0)
pbp$away_CF <- ifelse(pbp$event_type %in% c("GOAL","SHOT_ON_GOAL","MISSED_SHOT","BLOCKED_SHOT") & pbp$event_team_name==pbp$away_team, 1, 0)
pbp$tied <- ifelse(pbp$home_score==pbp$away_score, 1, 0)
pbp$home_lead_1 <- ifelse(pbp$home_score-pbp$away_score==1, 1, 0)
pbp$home_lead_2 <- ifelse(pbp$home_score-pbp$away_score==2, 1, 0)
pbp$home_lead_3 <- ifelse(pbp$home_score-pbp$away_score>=3, 1, 0)
pbp$away_lead_1 <- ifelse(pbp$home_score-pbp$away_score==(-1), 1, 0)
pbp$away_lead_2 <- ifelse(pbp$home_score-pbp$away_score==(-2), 1, 0)
pbp$away_lead_3 <- ifelse(pbp$home_score-pbp$away_score<=(-3), 1, 0)
pbp$Five <- ifelse(pbp$strength_state=="5v5", 1, 0)
pbp$Four <- ifelse(pbp$strength_state=="4v4", 1, 0)
pbp$Three <- ifelse(pbp$strength_state=="3v3", 1, 0)
pbp$home_xGF[is.na(pbp$home_xGF)] <- 0
pbp$away_xGF[is.na(pbp$away_xGF)] <- 0
pbp$event_length[is.na(pbp$event_length)] <- 0

pbp_ev <- pbp %>% filter(strength_state %in% penalty_kill)
rm(pbp)

grouped_shifts <- pbp_ev %>%
  group_by(game_id, shift_change_index, period, game_score_state, home_on_1, home_on_2, home_on_3, home_on_4, home_on_5, home_on_6, 
           away_on_1, away_on_2, away_on_3, away_on_4, away_on_5, away_on_6) %>%
  summarise(shift_length = sum(event_length), homexGF = sum(home_xGF), awayxGF = sum(away_xGF),
            homeGF = sum(home_GF), awayGF = sum(away_GF), homeCF = sum(home_CF), awayCF = sum(away_CF),
            Home_Up_1 = max(home_lead_1), Home_Up_2 = max(home_lead_2), Home_Up_3 = max(home_lead_3),
            Away_Up_1 = max(away_lead_1), Away_Up_2 = max(away_lead_2), Away_Up_3 = max(away_lead_3), Tied = max(tied), 
            State_5v5 = max(Five), State_4v4 = max(Four), State_3v3 = max(Three)) %>%
  filter(shift_length > 0)

home_as_off <- grouped_shifts %>%
  rename(offense_1 = home_on_1, offense_2 = home_on_2, offense_3 = home_on_3, offense_4 = home_on_4, offense_5 = home_on_5, offense_6 = home_on_6,
         defense_1 = away_on_1, defense_2 = away_on_2, defense_3 = away_on_3, defense_4 = away_on_4, defense_5 = away_on_5, defense_6 = away_on_6,
         Up_1 = Home_Up_1, Up_2 = Home_Up_2, Up_3 = Home_Up_3, Down_1 = Away_Up_1, Down_2 = Away_Up_2, Down_3 = Away_Up_3, xGF = homexGF, GF = homeGF, CF = homeCF) %>%
  select(game_id, shift_change_index, period, game_score_state,
         offense_1, offense_2, offense_3, offense_4, offense_5, offense_6, 
         defense_1, defense_2, defense_3, defense_4, defense_5, defense_6, 
         xGF, GF, CF, shift_length, shift_change_index, Tied, State_5v5, State_4v4, State_3v3, 
         Up_1, Up_2, Up_3, Down_1, Down_2, Down_3) %>%
  mutate(xGF_60 = xGF*3600/shift_length, GF_60 = GF*3600/shift_length, CF_60 = CF*3600/shift_length, is_home = 1)

away_as_off <- grouped_shifts %>%
  rename(offense_1 = away_on_1, offense_2 = away_on_2, offense_3 = away_on_3, offense_4 = away_on_4, offense_5 = away_on_5, offense_6 = away_on_6,
         defense_1 = home_on_1, defense_2 = home_on_2, defense_3 = home_on_3, defense_4 = home_on_4, defense_5 = home_on_5, defense_6 = home_on_6,
         Up_1 = Away_Up_1, Up_2 = Away_Up_2, Up_3 = Away_Up_3, Down_1 = Home_Up_1, Down_2 = Home_Up_2, Down_3 = Home_Up_3, xGF = awayxGF, GF = awayGF, CF = awayCF) %>%
  select(game_id, shift_change_index, period, game_score_state,
         offense_1, offense_2, offense_3, offense_4, offense_5, offense_6, 
         defense_1, defense_2, defense_3, defense_4, defense_5, defense_6, 
         xGF, GF, CF, shift_length, shift_change_index, Tied, State_5v5, State_4v4, State_3v3, 
         Up_1, Up_2, Up_3, Down_1, Down_2, Down_3) %>%
  mutate(xGF_60 = xGF*3600/shift_length, GF_60 = GF*3600/shift_length, CF_60 = CF*3600/shift_length, is_home = 0)

shifts_combined <- full_join(home_as_off, away_as_off)
rm(pbp_ev,home_as_off,away_as_off,grouped_shifts)

n_cores <- parallel::detectCores()
cl <- makeCluster(n_cores)
registerDoParallel(cl)
getDoParWorkers()

shifts_subset = subset(shifts_combined, select = c(offense_1:offense_6,defense_1:defense_6,shift_length,State_4v4,State_3v3,Up_1:Down_3,xGF_60,GF_60,CF_60,is_home))
rm(shifts_combined)

shifts_combined_dummies_off <- dummy_cols(shifts_subset, select_columns = c("offense_1"))
gc()
shifts_combined_dummies_off <- dummy_cols(shifts_combined_dummies_off, select_columns = c("offense_2"))
gc()
shifts_combined_dummies_off <- dummy_cols(shifts_combined_dummies_off, select_columns = c("offense_3"))
gc()
shifts_combined_dummies_off <- dummy_cols(shifts_combined_dummies_off, select_columns = c("offense_4"))
gc()
shifts_combined_dummies_off <- dummy_cols(shifts_combined_dummies_off, select_columns = c("offense_5"))
gc()
shifts_combined_dummies_off <- dummy_cols(shifts_combined_dummies_off, select_columns = c("offense_6"))
gc()

shifts_combined_dummies_def <- dummy_cols(shifts_subset, select_columns = c("defense_1"))
gc()
shifts_combined_dummies_def <- dummy_cols(shifts_combined_dummies_def, select_columns = c("defense_2"))
gc()
shifts_combined_dummies_def <- dummy_cols(shifts_combined_dummies_def, select_columns = c("defense_3"))
gc()
shifts_combined_dummies_def <- dummy_cols(shifts_combined_dummies_def, select_columns = c("defense_4"))
gc()
shifts_combined_dummies_def <- dummy_cols(shifts_combined_dummies_def, select_columns = c("defense_5"))
gc()
shifts_combined_dummies_def <- dummy_cols(shifts_combined_dummies_def, select_columns = c("defense_6"))
gc()

shifts_combined_dummies_off = subset(shifts_combined_dummies_off, select = -c(offense_1,offense_2,offense_3,offense_4,offense_5,offense_6,defense_1,defense_2,defense_3,defense_4,defense_5,defense_6))
gc()
shifts_combined_dummies_def = subset(shifts_combined_dummies_def, select = -c(offense_1,offense_2,offense_3,offense_4,offense_5,offense_6,defense_1,defense_2,defense_3,defense_4,defense_5,defense_6,shift_length,State_4v4,State_3v3,Up_1:Down_3,xGF_60,GF_60,CF_60,is_home))
gc()
shifts_combined_dummies <- cbind(shifts_combined_dummies_off, shifts_combined_dummies_def)
rm(shifts_subset,shifts_combined_dummies_off,shifts_combined_dummies_def)
gc()

colnames(shifts_combined_dummies) = gsub("offense_1", "offense_", colnames(shifts_combined_dummies))
colnames(shifts_combined_dummies) = gsub("offense_2", "offense_", colnames(shifts_combined_dummies))
colnames(shifts_combined_dummies) = gsub("offense_3", "offense_", colnames(shifts_combined_dummies))
colnames(shifts_combined_dummies) = gsub("offense_4", "offense_", colnames(shifts_combined_dummies))
colnames(shifts_combined_dummies) = gsub("offense_5", "offense_", colnames(shifts_combined_dummies))
colnames(shifts_combined_dummies) = gsub("offense_6", "offense_", colnames(shifts_combined_dummies))
colnames(shifts_combined_dummies) = gsub("defense_1", "defense_", colnames(shifts_combined_dummies))
colnames(shifts_combined_dummies) = gsub("defense_2", "defense_", colnames(shifts_combined_dummies))
colnames(shifts_combined_dummies) = gsub("defense_3", "defense_", colnames(shifts_combined_dummies))
colnames(shifts_combined_dummies) = gsub("defense_4", "defense_", colnames(shifts_combined_dummies))
colnames(shifts_combined_dummies) = gsub("defense_5", "defense_", colnames(shifts_combined_dummies))
colnames(shifts_combined_dummies) = gsub("defense_6", "defense_", colnames(shifts_combined_dummies))
gc()

shifts_combined_dummies <- as.data.frame(lapply(split.default(shifts_combined_dummies, names(shifts_combined_dummies)), function(x) Reduce(`+`, x)))
gc()

shifts_combined_dummies <- shifts_combined_dummies %>% select(-contains("Goalie"))
shifts_combined_dummies <- shifts_combined_dummies %>% select(-contains("Missing"))

xGF60 <- as.numeric(c(shifts_combined_dummies$xGF_60))
GF60 <- as.numeric(c(shifts_combined_dummies$GF_60))
CF60 <- as.numeric(c(shifts_combined_dummies$CF_60))
shift_length <- as.numeric(c(shifts_combined_dummies$shift_length))
subsetted_dummies = subset(shifts_combined_dummies, select = -c(shift_length, xGF_60, GF_60, CF_60))
rm(shifts_combined_dummies)
gc()

RAPM_xGF <- as.matrix(subsetted_dummies)
Sparse_RAPM_xGF <- Matrix(RAPM_xGF, sparse = TRUE)
rm(RAPM_xGF)

RAPM_GF <- as.matrix(subsetted_dummies)
Sparse_RAPM_GF <- Matrix(RAPM_GF, sparse = TRUE)
rm(RAPM_GF)

RAPM_CF <- as.matrix(subsetted_dummies)
Sparse_RAPM_CF <- Matrix(RAPM_CF, sparse = TRUE)
rm(RAPM_CF)
gc()

rm(subsetted_dummies)

Cross_Validated_Results_xGF <- cv.glmnet(x=Sparse_RAPM_xGF, y=xGF60, weights=shift_length, alpha=0, nfolds=10, standardize=FALSE, parallel=TRUE)
gc()
Cross_Validated_Results_GF <- cv.glmnet(x=Sparse_RAPM_GF, y=GF60, weights=shift_length, alpha=0, nfolds=10, standardize=FALSE, parallel=TRUE)
gc()
Cross_Validated_Results_CF <- cv.glmnet(x=Sparse_RAPM_CF, y=CF60, weights=shift_length, alpha=0, nfolds=10, standardize=FALSE, parallel=TRUE)
gc()

Run_RAPM_xGF <- glmnet(x=Sparse_RAPM_xGF, y=xGF60, weights=shift_length, lambda = Cross_Validated_Results_xGF[["lambda.min"]], alpha=0, nfolds=10, standardize=FALSE, parallel=TRUE)
Run_RAPM_GF <- glmnet(x=Sparse_RAPM_GF, y=GF60, weights=shift_length, lambda = Cross_Validated_Results_xGF[["lambda.min"]], alpha=0, nfolds=10, standardize=FALSE, parallel=TRUE)
Run_RAPM_CF <- glmnet(x=Sparse_RAPM_CF, y=CF60, weights=shift_length, lambda = Cross_Validated_Results_xGF[["lambda.min"]], alpha=0, nfolds=10, standardize=FALSE, parallel=TRUE)

stopCluster(cl)
registerDoSEQ()

RAPM_xGF_coefficients <- as.data.frame(as.matrix(coef(Run_RAPM_xGF)))
RAPM_GF_coefficients <- as.data.frame(as.matrix(coef(Run_RAPM_GF)))
RAPM_CF_coefficients <- as.data.frame(as.matrix(coef(Run_RAPM_CF)))

Binded_Coefficients_xGF <- cbind(rownames(RAPM_xGF_coefficients), RAPM_xGF_coefficients) %>%
  rename(Player = `rownames(RAPM_xGF_coefficients)`, xGF_60 = s0)
Binded_Coefficients_GF <- cbind(rownames(RAPM_GF_coefficients), RAPM_GF_coefficients) %>%
  rename(Player = `rownames(RAPM_GF_coefficients)`, GF_60 = s0)
Binded_Coefficients_CF <- cbind(rownames(RAPM_CF_coefficients), RAPM_CF_coefficients) %>%
  rename(Player = `rownames(RAPM_CF_coefficients)`, CF_60 = s0)

offense_RAPM_xGF <- Binded_Coefficients_xGF %>%
  filter(grepl("offense", Binded_Coefficients_xGF$Player))
offense_RAPM_GF <- Binded_Coefficients_GF %>%
  filter(grepl("offense", Binded_Coefficients_GF$Player))
offense_RAPM_CF <- Binded_Coefficients_CF %>%
  filter(grepl("offense", Binded_Coefficients_CF$Player))

offense_RAPM_xGF$Player = str_replace_all(offense_RAPM_xGF$Player, "offense__", "")
offense_RAPM_GF$Player = str_replace_all(offense_RAPM_GF$Player, "offense__", "")
offense_RAPM_CF$Player = str_replace_all(offense_RAPM_CF$Player, "offense__", "")

defense_RAPM_xGA <- Binded_Coefficients_xGF %>%
  filter(grepl("defense", Binded_Coefficients_xGF$Player)) %>%
  rename(xGA_60 = xGF_60)
defense_RAPM_GA <- Binded_Coefficients_GF %>%
  filter(grepl("defense", Binded_Coefficients_GF$Player)) %>%
  rename(GA_60 = GF_60)
defense_RAPM_CA <- Binded_Coefficients_CF %>%
  filter(grepl("defense", Binded_Coefficients_CF$Player)) %>%
  rename(CA_60 = CF_60)

defense_RAPM_xGA$Player = str_replace_all(defense_RAPM_xGA$Player, "defense__", "")
defense_RAPM_GA$Player = str_replace_all(defense_RAPM_GA$Player, "defense__", "")
defense_RAPM_CA$Player = str_replace_all(defense_RAPM_CA$Player, "defense__", "")

joined_RAPM_xG <- inner_join(offense_RAPM_xGF, defense_RAPM_xGA, by="Player")
joined_RAPM_G <- inner_join(offense_RAPM_GF, defense_RAPM_GA, by="Player")
joined_RAPM_C <- inner_join(offense_RAPM_CF, defense_RAPM_CA, by="Player")

joined_RAPM_xG$xGPM_60 <- joined_RAPM_xG$xGF_60 - joined_RAPM_xG$xGA_60
joined_RAPM_G$GPM_60 <- joined_RAPM_G$GF_60 - joined_RAPM_G$GA_60
joined_RAPM_C$CPM_60 <- joined_RAPM_C$CF_60 - joined_RAPM_C$CA_60

joined_RAPM <- inner_join(joined_RAPM_xG, joined_RAPM_G, by="Player")
joined_RAPM <- inner_join(joined_RAPM, joined_RAPM_C, by="Player")

joined_RAPM <- joined_RAPM %>%
  arrange(desc(xGPM_60))

joined_RAPM$Player = as.numeric(as.character(joined_RAPM$Player))

rapm_sh <- joined_RAPM %>% rename(playerID = Player)
rm(joined_RAPM,cl,n_cores,defense_RAPM_xGA,defense_RAPM_GA,defense_RAPM_CA,joined_RAPM_G,joined_RAPM_C,joined_RAPM_xG,offense_RAPM_xGF,offense_RAPM_GF,offense_RAPM_CF,Binded_Coefficients_GF,Binded_Coefficients_CF,Binded_Coefficients_xGF,Cross_Validated_Results_GF,Cross_Validated_Results_CF,Cross_Validated_Results_xGF,RAPM_GF_coefficients,RAPM_CF_coefficients,RAPM_xGF_coefficients,Run_RAPM_xGF,Run_RAPM_GF,Run_RAPM_CF,Sparse_RAPM_xGF,even_strength,GF60,shift_length,xGF60,Sparse_RAPM_GF,Sparse_RAPM_CF,CF60)
gc()

rapm_sh %>% saveRDS("data/player_rapm_sh_24_25.rds")


