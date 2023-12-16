 
install.packages('Lahman')
install.packages('tidyverse')

install.packages("devtools")
devtools::install_github("camdenk/mlbplotR")

library(Lahman)
library(tidyverse)
library(mlbplotR)

colnames(Teams)

Teams <- Teams %>% 
  filter(yearID == 2022) %>% 
  group_by(teamID) %>% 
  summarise(W = W)

Teams %>% 
  ggplot(aes(x = teamID, y = W)) + 
  geom_col()

view(Teams)

team_abbr <- valid_team_names()
# remove the league logos for this example 
team_abbr <- team_abbr[!team_abbr %in% c('AL','NL','MLB')]

wins = Teams$W

wins 

team_abbr



wins = c(101,74,83,78,74,62,92,68,81,66,106,65,73,111,69,86,78,101,99,60,87,62,89,90,81,93,86,68,92,55)
teams = team_abbr

df <- data.frame(teams = teams, wins = wins)
view(df)

ggplot(df, aes(x = teams, y = wins)) + 
  geom_mlb_logos(aes(team_abbr = teams), width = .075, alpha = .7) + 
  geom_col(aes(color = teams, fill = teams), width = 0.5) + 
  scale_color_mlb(type = 'secondary') + 
  scale_fill_mlb(alpha = 0.4) + 
  theme_minimal() + 
  theme(axis.text = element_mlb_logo())

colnames(Lahman :: Teams)



era <- Lahman :: Teams %>% 
  filter(yearID == 2022) %>% 
  group_by(teamID) %>% 
  summarise(era = ERA)

view(era)

era_by_team = c(3.46,4.25,3.97,4.53,4.00,4.86,3.46,5.06,3.92,4.04,2.9,4.7,3.77,2.80,3.86,3.83,3.98,3.57,3.30,4.52,3.97,4.66,3.81,3.59,3.85,3.79,3.41,4.22,3.87,5.00)

df_2 <- data.frame(teams = team_abbr, era = era_by_team)

ggplot(data = df_2, aes(x = teams, y = era)) + 
  geom_mlb_logos(aes(team_abbr = teams), width = 0.075, alpha = .9) + 
  geom_col(aes(color = teams, fill = teams), width = 0.5) + 
  scale_color_mlb(type = 'secondary') + 
  scale_fill_mlb(alpha = 0.4) + 
  theme_minimal() + 
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) + 
  ggtitle('ERA by Team') + 
  xlab('Teams') + 
  ylab('Team ERA')

combined <- data.frame(teams = team_abbr, era = era_by_team, wins = wins)
combined

ggplot(data = combined, aes(x = era, y = wins)) + 
  geom_mlb_logos(aes(team_abbr = teams), width = .075, alpha = .95) + 
  geom_point(alpha = .01) + 
  geom_smooth(method = 'lm', color = 'darkblue', se = FALSE, linewidth = .5) + 
  scale_x_reverse() + 
    theme_minimal() + 
  ggtitle('Wins by Team ERA') +
  xlab('Team ERA') + 
  ylab('Wins')

linfit <- lm(wins ~ era, data = combined)
linfit

# what is R and R^2

# y = -23 * era + 172.2 

library(broom)

summary <- augment(linfit, data = combined)

summary %>% 
  ggplot(aes(x = era, y = .resid/162)) + 
  geom_point(alpha = .01) + 
  geom_mlb_logos(aes(team_abbr = teams), width = .075, alpha = 1 ) +
  geom_hline(yintercept = 0, linetype = 3, color = 'red') + 
  theme_minimal() +
  xlab('Team ERA') + 
  ylab('Residual')



# Not the best indicator of wins, residuals are too high in my opinion

rmse <- summary %>% 
  summarize(RMSE = sqrt(mean(abs(.resid^2))))
rmse # not horrible but not great 


colnames(Teams)

# can calculate more statistics!!
# lets do batting average 







  
