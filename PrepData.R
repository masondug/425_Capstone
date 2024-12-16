offense_grouped <- ozone_shots %>% 
  group_by(game_id, shooting_team, goalie_team, cluster, season) %>% ## we remove goalie name from shot because we do not care if the goalie is pulled from an offensive POV
  summarize(season = mean(season),
            Goals = sum(goal),
            shotsOnGoal = sum(shotWasOnGoal),
            xGoals = sum(xGoal),
            xFroze = sum(xFroze),
            xRebound = sum(xRebound),
            xPlayContinuedInZone = sum(xPlayContinuedInZone),
            xPlayContinuedOutsideZone = sum(xPlayContinuedOutsideZone),
            xPlayStopped = sum(xPlayStopped),
            xShotWasOnGoal = sum(xShotWasOnGoal),
            shots = n()) %>% 
  ungroup() %>% 
  group_by(game_id, shooting_team, season, goalie_team) %>% 
  complete(cluster = 1:12, fill = list(
    Goals = 0,
    shotsOnGoal = 0,
    xGoals = NA_real_,
    xFroze = NA_real_,
    xRebound = NA_real_,
    xPlayContinuedInZone = NA_real_,
    xPlayContinuedOutsideZone = NA_real_,
    xPlayStopped = NA_real_,
    xShotWasOnGoal = NA_real_,
    shots = 0
  )) %>%
  ungroup() %>%
  mutate(
    xGoals = (xGoals - mean(xGoals, na.rm = TRUE)) / sd(xGoals, na.rm = TRUE),
    xFroze = (xFroze - mean(xFroze, na.rm = TRUE)) / sd(xFroze, na.rm = TRUE),
    xRebound = (xRebound - mean(xRebound, na.rm = TRUE)) / sd(xRebound, na.rm = TRUE),
    xPlayContinuedInZone = (xPlayContinuedInZone - mean(xPlayContinuedInZone, na.rm = TRUE)) / sd(xPlayContinuedInZone, na.rm = TRUE),
    xPlayContinuedOutsideZone = (xPlayContinuedOutsideZone - mean(xPlayContinuedOutsideZone, na.rm = TRUE)) / sd(xPlayContinuedOutsideZone, na.rm = TRUE),
    xPlayStopped = (xPlayStopped - mean(xPlayStopped, na.rm = TRUE)) / sd(xPlayStopped, na.rm = TRUE),
    xShotWasOnGoal = (xShotWasOnGoal - mean(xShotWasOnGoal, na.rm = TRUE)) / sd(xShotWasOnGoal, na.rm = TRUE)
  )

## Now making stats for goalies and removing games where multiple goalies were used. While these stats could be useful, I have no data for time on ice, so stats cannot be normalized.

goalie_grouped <- ozone_shots %>%
  group_by(game_id, shooting_team, goalie_team) %>%
  mutate(goalie_num = n_distinct(goalieNameForShot)) %>% 
  filter(goalie_num == 1) %>% 
  ungroup() %>% 
  group_by(game_id, shooting_team, goalie_team, goalieNameForShot, cluster) %>%
  summarize(season = mean(season),
            Goals = sum(goal),
            shotsOnGoal = sum(shotWasOnGoal),
            xGoals = sum(xGoal),
            xFroze = sum(xFroze),
            xRebound = sum(xRebound),
            xPlayContinuedInZone = sum(xPlayContinuedInZone),
            xPlayContinuedOutsideZone = sum(xPlayContinuedOutsideZone),
            xPlayStopped = sum(xPlayStopped),
            xShotWasOnGoal = sum(xShotWasOnGoal),
            shots = n()) %>% 
  group_by(game_id, shooting_team, goalie_team, goalieNameForShot, season) %>%
  complete(cluster = 1:12, fill = list(
    Goals = 0,
    shotsOnGoal = 0,
    xGoals = NA_real_,
    xFroze = NA_real_,
    xRebound = NA_real_,
    xPlayContinuedInZone = NA_real_,
    xPlayContinuedOutsideZone = NA_real_,
    xPlayStopped = NA_real_,
    xShotWasOnGoal = NA_real_,
    shots = 0
  )) %>%
  ungroup() %>%
  mutate(
    xGoals = (xGoals - mean(xGoals, na.rm = TRUE)) / sd(xGoals, na.rm = TRUE),
    xFroze = (xFroze - mean(xFroze, na.rm = TRUE)) / sd(xFroze, na.rm = TRUE),
    xRebound = (xRebound - mean(xRebound, na.rm = TRUE)) / sd(xRebound, na.rm = TRUE),
    xPlayContinuedInZone = (xPlayContinuedInZone - mean(xPlayContinuedInZone, na.rm = TRUE)) / sd(xPlayContinuedInZone, na.rm = TRUE),
    xPlayContinuedOutsideZone = (xPlayContinuedOutsideZone - mean(xPlayContinuedOutsideZone, na.rm = TRUE)) / sd(xPlayContinuedOutsideZone, na.rm = TRUE),
    xPlayStopped = (xPlayStopped - mean(xPlayStopped, na.rm = TRUE)) / sd(xPlayStopped, na.rm = TRUE),
    xShotWasOnGoal = (xShotWasOnGoal - mean(xShotWasOnGoal, na.rm = TRUE)) / sd(xShotWasOnGoal, na.rm = TRUE)
  )

## Now, we need to add the game date to the data in order for us to calculate pre-game averages. Using the game data from MoneyPuck that we used to analyze scores:

offense_grouped <- offense_grouped %>% 
  mutate(game_id = as.numeric(paste0(substr(game_id, 1, 4), '0', substr(game_id, 5, nchar(game_id))))) %>% 
  left_join(all_teams %>% 
              dplyr::select(gameId, gameDate), ## we only want the date
            by = c('game_id' = 'gameId'),
            relationship = 'many-to-many') %>% 
  unique() %>% 
  mutate(goals_scored = Goals)

## change date format from integer to date
offense_grouped$gameDate <- ymd(offense_grouped$gameDate) %>% as.Date(format("%m/%d/%Y"))

goalie_grouped <- goalie_grouped %>% 
  mutate(game_id = as.numeric(paste0(substr(game_id, 1, 4), '0', substr(game_id, 5, nchar(game_id))))) %>% 
  left_join(all_teams %>% 
              dplyr::select(gameId, gameDate), ## we only want the date
            by = c('game_id' = 'gameId'),
            relationship = 'many-to-many') %>% 
  unique() %>% 
  mutate(goals_conceded = Goals)

## change date format from integer to date
goalie_grouped$gameDate <- ymd(goalie_grouped$gameDate) %>% as.Date(format("%m/%d/%Y"))

## Now, we want to find the pre-game average for the expected statistics per cluster.

calculate_rolling_averages <- function(data, group_cols, exclude_cols) {
  # Identify numeric columns for pre-game averages, excluding specified columns
  stat_cols <- names(data)[sapply(data, is.numeric) & !(names(data) %in% c(exclude_cols, group_cols))]
  
  data %>%
    arrange(across(all_of(group_cols)), gameDate) %>% # Sort by group columns and gameDate
    group_by(across(all_of(group_cols))) %>%          # Group by specified columns
    mutate(
      games_played = row_number() - 1, # Count games played (excluding the current game)
      across(
        all_of(stat_cols),
        ~ lag(cumsum(replace_na(., 0)) / lag(cumsum(!is.na(.)), default = 1), default = NA) # Correctly handle NA in mean calculation
      )
    ) %>%
    ungroup()
}

exclude_cols <- c("Goals", "game_id", "season")

offense_averages <- calculate_rolling_averages(offense_grouped, group_cols = c('shooting_team', 'season', 'cluster'), exclude_cols) %>% 
  mutate(savePct = (shotsOnGoal-goals_scored) / shotsOnGoal) %>% 
  arrange(shooting_team)

goalie_averages <- calculate_rolling_averages(goalie_grouped, group_cols = c('goalie_team', 'goalieNameForShot', 'season', 'cluster'), exclude_cols) %>% 
  mutate(savePct = (shotsOnGoal - goals_conceded) / shotsOnGoal) %>% 
  arrange(shooting_team)

## join by goalie and offense data by game and cluster

df <- offense_averages %>% 
  left_join(goalie_averages,
            by = c('game_id', 'shooting_team', 'season', 'goalie_team', 'cluster', 'Goals', 'gameDate'),
            suffix = c('.offense', '.goalie')) %>% 
  filter(!is.na(goalieNameForShot),
         games_played.offense > 0,
         games_played.goalie > 0,
         !is.na(xGoals.offense),
         !is.na(xGoals.goalie),
         !is.nan(xGoals.offense),
         !is.nan(xGoals.goalie),
         !is.infinite(xGoals.offense),
         !is.infinite(xGoals.goalie),
         xGoals.offense != -Inf,
         xGoals.goalie != -Inf) %>% 
  ## now lets check for games where all clusters have data
  group_by(game_id, shooting_team) %>% 
  mutate(cluster_num = n_distinct(cluster)) %>% 
  filter(cluster_num == 12) %>% 
  dplyr::select(-cluster_num)

## I also want to use the `all_teams` dataframe created earlier to add if a team was home or away.

df <- df %>% 
  left_join(all_teams %>% select(team, gameId, home_or_away),
            by = c('shooting_team' = 'team',
                   'game_id' = 'gameId')) %>% 
  unique() %>% 
  select(-games_played.offense, -games_played.goalie)

df <- df[complete.cases(df), ]