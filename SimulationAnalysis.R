goalie_eval <- data.frame(ranef(zip_model_lasso)$cond)

goalie_eval$Cluster <- sapply(str_split(row.names(goalie_eval), ':'), `[`, 1)
goalie_eval$Goalie <- sapply(str_split(row.names(goalie_eval), ':'), `[`, 2)
rownames(goalie_eval) = c()
goalie_eval <- goalie_eval %>%
  ## because a negative value is better (results in less goals scored) we can use -1 to make the results more intuitive
  mutate(Estimate = -X.Intercept.) %>%
  select(-X.Intercept.)

## Using data downloaded from [Hockey Reference](https://www.hockey-reference.com/leagues/NHL_2025_goalies.html)

goalies <- read.csv('Data/goalies.csv') %>% 
  ## removes accents
  mutate(Player = stri_trans_general(Player, "Latin-ASCII"))

goalie_eval <- goalie_eval %>% 
  left_join(goalies, by = c('Goalie' = 'Player')) %>% 
  ## removes goalies no longer in the NHL -- checked a few to make sure there weren't errors
  na.omit() %>% 
  select(-Pos) %>% 
  ## removes goalies who have played for two teams
  ### reduces variance by a goalie playing with a different team and defense
  filter(Team != '2TM') %>% 
  ## beacause goalies have multiple goalies in a season, let's get the two who have played the most
  group_by(Team, Cluster) %>% 
  arrange(desc(GS)) %>%  # Sort by games played (GP) in descending order
  slice_head(n = 2) %>%  # Select the top two goalies for each team and cluster
  ## assigning the starter as the goalie who has the most starts for the season
  mutate(Role = ifelse(GS == max(GS), 'Starter', 'Backup'))

ridge_data <- goalie_eval %>%
  select(-GP) %>% 
  group_by(Team, Cluster) %>%
  pivot_wider(
    names_from = Role,
    values_from = c(GS, Estimate)
  ) %>%
  # Replace missing values with the other column's value only where NA
  mutate(
    GS_Starter = mean(GS_Starter, na.rm = TRUE),
    GS_Backup = mean(GS_Backup, na.rm = TRUE),
    Estimate_Starter = mean(Estimate_Starter, na.rm = TRUE),
    Estimate_Backup = mean(Estimate_Backup, na.rm = TRUE),
    Estimate_Difference = Estimate_Starter - Estimate_Backup
  ) %>%
  ungroup() %>% 
  # Remove Goalie column and ensure unique rows
  select(-Goalie) %>% 
  distinct() %>% 
  mutate(Cluster = factor(Cluster, levels = as.character(1:12))) %>% 
  # Calculate means for emphasis
  group_by(Cluster) %>%
  mutate(Mean_Difference = mean(Estimate_Difference, na.rm = TRUE)) %>%
  ungroup() 
  
## using data as of 12/12

# team_data_2024 <- read.csv('team_data.csv') %>% 
#   filter(situation == 'all') %>% 
#   select(-situation, -name, -team.1, -position)

shots_2024 <- read.csv('Data/shots_2024.csv') %>% 
  filter(xCordAdjusted > 25,
         shotOnEmptyNet == 0) %>% 
  mutate(shooting_team = ifelse(team == 'HOME', homeTeamCode, awayTeamCode),
         goalie_team = ifelse(team == 'HOME', awayTeamCode, homeTeamCode),
         game_id = as.numeric(paste0(season, game_id)))

shots_2024$cluster <- as.factor(predict(gmm_model, 
                                        newdata = shots_2024 %>%dplyr::select(xCordAdjusted, yCordAdjusted),
                                        newdata.weights = shots_2024$xGoal)$classification)

offense_averages_2024 <- shots_2024 %>% 
  group_by(shooting_team, cluster, season) %>% ## we remove goalie name from shot because we do not care if the goalie is pulled from an offensive POV
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
  group_by(shooting_team, season) %>% 
  complete(cluster = as.factor(1:12), fill = list(
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
  ) %>% 
  mutate(goals_scored = Goals,
         savePct = goals_scored / shotsOnGoal)

goalie_averages_2024 <- shots_2024 %>%
  group_by(game_id, goalie_team) %>%
  mutate(goalie_num = n_distinct(goalieNameForShot)) %>% 
  filter(goalie_num == 1) %>% 
  ungroup() %>% 
  group_by(goalie_team, goalieNameForShot, cluster) %>%
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
  group_by(goalie_team, goalieNameForShot, season) %>%
  complete(cluster = as.factor(1:12), fill = list(
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
  ) %>% 
  mutate(goals_conceded = Goals,
         savePct = goals_conceded / shotsOnGoal)

specific_goalies <- goalie_averages_2024 %>% 
  filter(goalieNameForShot %in% unique(goalie_eval$Goalie)) %>% 
  na.omit() %>% 
  ## removing traded goalie that appears for multiple teams
  filter(goalieNameForShot != 'Justus Annunen') %>% 
  group_by(goalieNameForShot) %>% 
  filter(n_distinct(cluster) == 12) %>% 
  ## teams with 2 unique goalies -- not just 1
  group_by(goalie_team) %>% 
  filter(n_distinct(goalieNameForShot) == 2) %>% 
  ungroup()

simulation_data <- offense_averages_2024 %>%
  expand_grid(goalie_team = unique(all_teams$team)) %>% 
  left_join(specific_goalies,
            by = c('goalie_team', 'cluster', 'season'),
            suffix = c('.offense', '.goalie')) %>% 
  ## removing games where offense and goalie are the same
  filter(shooting_team != goalie_team)

simulation_data$predicted_score <- predict(zip_model_lasso, newdata = simulation_data, type = 'response')

predicted_values <- simulation_data %>% 
  ungroup() %>% 
  group_by(shooting_team, goalieNameForShot, goalie_team) %>% 
  summarize(score = sum(predicted_score, na.rm = T))

## rejoining role
starter_win_percentage <- predicted_values %>% 
  na.omit() %>% 
  filter(goalie_team != 'VGK') %>% 
  left_join(goalies, by = c('goalieNameForShot' = 'Player',
                            'goalie_team' = 'Team')) %>% 
  group_by(goalie_team) %>% 
  mutate(Role = ifelse(GS == max(GS), 'Starter', 'Backup')) %>% 
  group_by(goalie_team, shooting_team) %>% 
  summarise(
    starter_score = sum(score[Role == "Starter"], na.rm = TRUE),
    backup_score = sum(score[Role == "Backup"], na.rm = TRUE)
  ) %>% 
  mutate(
    starter_won = starter_score < backup_score,  # Starter wins if their score is lower
    score_difference = starter_score - backup_score  # Calculate score difference for each game
  ) %>% 
  group_by(goalie_team) %>% 
  summarise(
    total_games = n(),
    starter_wins = sum(starter_won, na.rm = TRUE),
    starter_win_percentage = (starter_wins / total_games) * 100,
    average_difference = mean(score_difference, na.rm = TRUE)  # Average score difference
  )
