### Scraping Baseball Savant Statcast Data ###

library(tidyverse)
library(dplyr)
library(ggplot2)
library(baseballr)
library(reshape2)
library(zoo)

### Bryce Harper Data ### 

harper_id <- playerid_lookup(last_name = 'Harper', first_name = 'Bryce') %>% 
  pull(mlbam_id)

BH21 <- scrape_statcast_savant_batter(start_date = '2021-04-01', end_date = '2021-10-03',
                                      batterid = harper_id)

BH20 <- scrape_statcast_savant_batter(start_date = '2020-07-23', end_date = '2020-09-27',
                                     batterid = harper_id) 

BH19 <- scrape_statcast_savant_batter(start_date = '2019-03-01', end_date = '2019-10-01',
                                      batterid = harper_id) 

BH18 <- scrape_statcast_savant_batter(start_date = '2018-03-01', end_date = '2018-10-01',
                                   batterid = harper_id)

BH <- rbind(BH21,BH20,BH19,BH18)

BH

BH2 <- BH %>% 
  filter(type == 'X', launch_speed != 0)


bh_avg_year <- BH2 %>% 
  group_by(game_year) %>% 
  summarise(wOBACON = round(sum(woba_value, na.rm = TRUE)/sum(woba_denom, na.rm = TRUE), 3),
  LS = round(mean(launch_speed, na.rm = TRUE), 1), LA = round(mean(launch_speed, na.rm = TRUE), 1))

view(bh_avg_year)

bh_group <- BH2 %>% 
  group_by(game_date) %>% 
  summarise('Average Launch Angle' = mean(launch_angle, na.rm = TRUE),
            'Average Batted Ball Speed' = mean(launch_speed, na.rm = TRUE)) %>% 
  ungroup() %>% 
  melt(id = ('game_date')) %>% 
  mutate(Year = as.factor(substr(game_date,1,4)))

view(bh_group)

bh_group %>% 
  ggplot(aes(game_date, value)) + 
  geom_point() + 
  stat_smooth(aes(group = Year, color = Year)) + 
  facet_wrap(~variable, scales = 'free_y') + 
  ggtitle("\nBryce Harper: 2018 - 2021\n") + 
  labs(subtitle ="Harper incresad his average batted ball speed from 90.3 mph in 2020 to 91.1 mph in 2021")












