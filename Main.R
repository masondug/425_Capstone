## analyzing which seasons should be included
all_teams <- readRDS(url("https://www.dropbox.com/scl/fi/cwee4v5psta8hs3wm31bh/all_teams.rds?rlkey=cinetyhr09w99n9ee1sscvx1b&st=pvvaaiqu&dl=1")) %>%
  filter(situation == 'all') %>%
  mutate(season = factor(season, levels = sort(unique(season), decreasing = TRUE)))


## getting data ready for clustering
ozone_shots <- read_csv(url('https://www.dropbox.com/scl/fi/0kguwelspz0joiw5e8jtw/shots_to_2023.csv?rlkey=yx9cgbgp391gwazjssonr3786&st=7woest5o&dl=1'))

ozone_shots <- ozone_shots %>% 
  filter(season > 2016,
         season != 2020,
         xCordAdjusted > 25,
         xCordAdjusted < 90,
         isPlayoffGame == 0,
         shotOnEmptyNet == 0) %>% 
  mutate(shooting_team = ifelse(team == 'HOME', homeTeamCode, awayTeamCode),
         goalie_team = ifelse(team == 'HOME', awayTeamCode, homeTeamCode),
         game_id = as.numeric(paste0(season, game_id)))

## gmm model to cluster data
# gmm_model <- Mclust(
#   data = ozone_shots %>% select(xCordAdjusted, yCordAdjusted),
#   weights = ozone_shots$xGoal,
#   G = 12)
## this is commented out because it takes a lot of time, the following line will use the RDS file of the saved model
gmm_model <- readRDS('Data/gmm_model.rds')


## applying gmm to ozone shots
ozone_shots <- ozone_shots %>%
  mutate(cluster = gmm_model$classification)


source('PrepData.R')

## takes a long time, the model was saved as an rds file 
#source('Analysis.R')
zip_model_lasso <- readRDS(url('https://www.dropbox.com/scl/fi/3yh24l5ss5fpzh7nm1zgt/zip_model_lasso.rds?rlkey=al6skp0rjweaxjph6spu3nasr&st=q4a1d7og&dl=1'))

df$predicted_score <- predict(zip_model_lasso, newdata = df, type = 'response')

df_total <- df %>% 
  group_by(game_id, shooting_team, goalie_team, home_or_away) %>% 
  summarize(Goals = sum(Goals))

source('SimulationAnalysis.R')

