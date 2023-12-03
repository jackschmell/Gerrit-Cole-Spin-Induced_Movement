### Gerrit Cole Pitch Movement Viz ### 

library(tidyverse)
library(baseballr)
library(dplyr)

cole_id <- playerid_lookup(last_name = 'Cole', first_name = 'Gerrit') %>% 
  pull(mlbam_id)

cole_stats <- scrape_statcast_savant_pitcher(start_date = '2023-03-01', end_date = '2023-10-01',
                                      pitcherid = cole_id)

# lets look at the data 

cole_stats

# Lets clean the data 

cleaned_cole_stats <- cole_stats %>% 
  filter(!(is.na(pfx_x)), !is.na(pfx_z)) %>% 
  mutate(pfx_in_x_pv = -12 * pfx_x, pfx_in_z = 12 * pfx_z)

# Lets see what pitches Gerrit Cole throws 

unique(cleaned_cole_stats$pitch_name)

# Organize 
palette <- c('4-Seam Fastball' = 'cyan',
            'Slider' = 'green', 
            'Cutter' = 'red', 
            'Changeup' = 'purple',
            'Knuckle Curve' = 'orange',
            'Sinker' = 'black') 

cleaned_cole_stats %>% 
  ggplot(aes(x = pfx_in_x_pv, y = pfx_in_z, color = pitch_name)) + 
  geom_point(alpha = .4) + 
  scale_color_manual(values = palette) + 
  theme_classic() + 
  geom_vline(xintercept = 0, linetype = 'dashed') + 
  geom_hline(yintercept = 0, linetype = 'dashed') + 
  scale_x_continuous(limits = c(-25,25), breaks = seq(-25,25,by = 5)) + 
  scale_y_continuous(limits = c(-20,25), breaks = seq(-20,25,by = 5)) + 
  labs(title = 'Gerrit Cole 2023 Pitch Movement Profiles | Pitcher POV', x = 'Horizontal Movement In Inches',
       y = 'Vertical Movement In Inches')

# Colate pitch location heat map

cleaned_cole_stats %>% 
  mutate(plate_x_pv = -1 * plate_x) %>% 
  ggplot(aes(x = plate_x_pv, y = plate_z)) + 
  stat_density2d(aes(fill = ..level..), geom = 'polygon') + 
  scale_fill_gradient(low = 'white', high = 'darkgreen') + 
  geom_vline(xintercept = 0, linetype = 'dashed') + 
  geom_hline(yintercept = 2.5, linetype = 'dashed') + 
  scale_x_continuous(limits = c(-3,3), breaks = seq(-3,3,by = 2)) + 
  scale_y_continuous(limits = c(-1,5), breaks = seq(-1,5,by = 2)) +
  geom_rect(mapping = aes(xmin = -.75, xmax = .75, ymin = 1.5, ymax = 3.5), color = 'black',
            fill = NA, linewidth = 1) + 
  theme_classic() + 
  theme(axis.line = element_blank(),  # Remove axis lines
        axis.text = element_blank(),   # Remove axis text (tick labels)
        axis.ticks = element_blank(),  # Remove axis ticks
        axis.title = element_blank()) + 
  ggtitle('Gerrit Cole 2023 Pitch Location Heat Map | Pitcher POV') 

cole_stats %>% colnames()

# Release Speed Plots by Year: 

GC15 <- scrape_statcast_savant_pitcher(start_date = '2015-03-01', end_date = '2015-10-01',
                                       pitcherid = cole_id)
GC16 <- scrape_statcast_savant_pitcher(start_date = '2016-03-01', end_date = '2016-10-01',
                                      pitcherid = cole_id)
GC17 <- scrape_statcast_savant_pitcher(start_date = '2017-03-01', end_date = '2017-10-01',
                                       pitcherid = cole_id)
GC18 <- scrape_statcast_savant_pitcher(start_date = '2018-03-01', end_date = '2018-10-01',
                                   pitcherid = cole_id)
GC19 <- scrape_statcast_savant_pitcher(start_date = '2019-03-01', end_date = '2019-10-01',
                                      pitcherid = cole_id)
GC20 <- scrape_statcast_savant_pitcher(start_date = '2020-03-01', end_date = '2020-10-01',
                                       pitcherid = cole_id)
GC21 <- scrape_statcast_savant_pitcher(start_date = '2021-03-01', end_date = '2021-10-01',
                                       pitcherid = cole_id)
GC22 <- scrape_statcast_savant_pitcher(start_date = '2022-03-01', end_date = '2022-10-01',
                                       pitcherid = cole_id)
GC23 <- scrape_statcast_savant_pitcher(start_date = '2023-03-01', end_date = '2023-10-01',
                                       pitcherid = cole_id)

GC <- rbind(GC15,GC16,GC17,GC18,GC19,GC20,GC21,GC22,GC23)

GC_group <- GC %>% 
  filter(!is.na(release_speed)) %>% 
  group_by(game_date) %>% 
  summarise(avg_velo = mean(release_speed, na.rm = TRUE)) %>% 
  mutate(Year = as.factor(substr(game_date,1,4)))


GC_group %>% 
  ggplot(aes(game_date,avg_velo)) + 
  geom_point() + 
  stat_smooth(aes(group = Year, color = Year)) + 
  ggtitle('Gerrit Cole Average Veclocity By Year | 2015 - 2023 ') + 
  labs(x = 'Date', y = 'Average Velocity')

GC_group %>% 
  group_by(Year) %>% 
  summarise(avg_velo = mean(avg_velo, na.rm = TRUE))

# Gerrit Cole FB Velcoity by year
gc_fb_by_year <- GC %>% 
  filter(pitch_name == '4-Seam Fastball') %>% 
  group_by(game_year) %>% 
  summarise(avg_fb_velo = mean(release_speed)) 


gc_pitch_movement_23 <- GC23 %>% 
  mutate(pitch_movement = ((12*pfx_x) ^2 + (12*pfx_z)^2)^.5) %>% 
  group_by(pitch_name) %>% 
  summarise(avg_pitch_movement_in = mean(pitch_movement))

gc_pitch_movement_23

gc_pitch_movement_22 <- GC22 %>% 
  mutate(pitch_movement = ((12*pfx_x) ^2 + (12*pfx_z)^2)^.5) %>% 
  group_by(pitch_name) %>% 
  summarise(avg_pitch_movement_in = mean(pitch_movement))

gc_pitch_movement_22

gc_pitch_movement_21 <- GC21 %>% 
  mutate(pitch_movement = ((12*pfx_x) ^2 + (12*pfx_z)^2)^.5) %>% 
  group_by(pitch_name) %>% 
  summarise(avg_pitch_movement_in = mean(pitch_movement))

gc_pitch_movement_21 

gc_pitch_movement_20 <- GC20 %>% 
  mutate(pitch_movement = ((12*pfx_x) ^2 + (12*pfx_z)^2)^.5) %>% 
  group_by(pitch_name) %>% 
  summarise(avg_pitch_movement_in = mean(pitch_movement))

gc_pitch_movement_20

gc_pitch_movement_19 <- GC19 %>% 
  mutate(pitch_movement = ((12*pfx_x) ^2 + (12*pfx_z)^2)^.5) %>% 
  group_by(pitch_name) %>% 
  summarise(avg_pitch_movement_in = mean(pitch_movement))

gc_pitch_movement_19

gc_pitch_movement_18 <- GC18  %>% 
  mutate(pitch_movement = ((12*pfx_x) ^2 + (12*pfx_z)^2)^.5) %>% 
  group_by(pitch_name) %>% 
  summarise(avg_pitch_movement_in = mean(pitch_movement))

gc_pitch_movement_18

gc_pitch_movement_17 <- GC17  %>% 
  mutate(pitch_movement = ((12*pfx_x) ^2 + (12*pfx_z)^2)^.5) %>% 
  group_by(pitch_name) %>% 
  summarise(avg_pitch_movement_in = mean(pitch_movement))

gc_pitch_movement_17

gc_pitch_movement_16 <- GC16 %>% mutate(pitch_movement = ((12*pfx_x) ^2 + (12*pfx_z)^2)^.5) %>% 
  group_by(pitch_name) %>% 
  summarise(avg_pitch_movement_in = mean(pitch_movement))

gc_pitch_movement_16 

gc_pitch_movement_15 <- GC15  %>% 
  mutate(pitch_movement = ((12*pfx_x) ^2 + (12*pfx_z)^2)^.5) %>% 
  group_by(pitch_name) %>% 
  summarise(avg_pitch_movement_in = mean(pitch_movement))

gc_pitch_movement_15


# Putting it all together 

gc_pitch_movement_binded <- rbind(gc_pitch_movement_23,gc_pitch_movement_22,gc_pitch_movement_21,gc_pitch_movement_20,gc_pitch_movement_19,gc_pitch_movement_18,gc_pitch_movement_17,gc_pitch_movement_16,gc_pitch_movement_15)

gc_pitch_movement_binded <- na.omit(gc_pitch_movement_binded) 

gc_pitch_movement_binded <- filter(gc_pitch_movement_binded, pitch_name %in% c('4-Seam Fastball', 'Changeup', 'Cutter', 'Knuckle Curve', 'Slider', 'Sinker'))

view(gc_pitch_movement_binded)

# Define the sequence of years
years_sequence <- c(rep(2023, 5), rep(2022, 6), rep(2021, 5), rep(2020, 5), 
                    rep(2019, 5), rep(2018, 5), rep(2017, 5), rep(2016, 5), rep(2015, 5))

# Add a new column 'Year' with the specified sequence of years
gc_pitch_movement_binded <- gc_pitch_movement_binded %>%
  mutate(Year = years_sequence)

view(gc_pitch_movement_binded)

# Now I am gonna create all the pitch movement charts : 

# 2022 : 

(gc_pitch_movement_22_chart <- GC22 %>% 
  mutate(pfx_x_in_pv = -12 * pfx_x, pfx_in_z = 12 * pfx_z) %>% 
  ggplot(aes(pfx_x_in_pv, pfx_in_z, color = pitch_name)) + 
  geom_point(alpha = .4) + 
  scale_color_manual(values = palette) + 
  theme_classic() + 
  geom_vline(xintercept = 0, linetype = 'dashed') + 
  geom_hline(yintercept = 0, linetype = 'dashed') + 
  scale_x_continuous(limits = c(-25,25), breaks = seq(-25,25,by = 5)) + 
  scale_y_continuous(limits = c(-20,25), breaks = seq(-20,25,by = 5)) + 
  labs(title = 'Gerrit Cole 2022 Pitch Movement Profiles | Pitcher POV', x = 'Horizontal Movement In Inches',
       y = 'Vertical Movement In Inches')) 

# 2021 : 
(gc_pitch_movement_21 <- GC21  %>% 
  mutate(pfx_x_in_pv = -12 * pfx_x, pfx_in_z = 12 * pfx_z) %>% 
  ggplot(aes(pfx_x_in_pv, pfx_in_z, color = pitch_name)) + 
  geom_point(alpha = .4) + 
  scale_color_manual(values = palette) + 
  theme_classic() + 
  geom_vline(xintercept = 0, linetype = 'dashed') + 
  geom_hline(yintercept = 0, linetype = 'dashed') + 
  scale_x_continuous(limits = c(-25,25), breaks = seq(-25,25,by = 5)) + 
  scale_y_continuous(limits = c(-20,25), breaks = seq(-20,25,by = 5)) + 
  labs(title = 'Gerrit Cole 2021 Pitch Movement Profiles | Pitcher POV', x = 'Horizontal Movement In Inches',
       y = 'Vertical Movement In Inches'))

# 2020 : 
(gc_pitch_movement_20 <- GC20  %>% 
    mutate(pfx_x_in_pv = -12 * pfx_x, pfx_in_z = 12 * pfx_z) %>% 
    ggplot(aes(pfx_x_in_pv, pfx_in_z, color = pitch_name)) + 
    geom_point(alpha = .4) + 
    scale_color_manual(values = palette) + 
    theme_classic() + 
    geom_vline(xintercept = 0, linetype = 'dashed') + 
    geom_hline(yintercept = 0, linetype = 'dashed') + 
    scale_x_continuous(limits = c(-25,25), breaks = seq(-25,25,by = 5)) + 
    scale_y_continuous(limits = c(-20,25), breaks = seq(-20,25,by = 5)) + 
    labs(title = 'Gerrit Cole 2020 Pitch Movement Profiles | Pitcher POV', x = 'Horizontal Movement In Inches',
         y = 'Vertical Movement In Inches', subtitle = 'COVID SEASON'))

(gc_pitch_movement_19 <- GC19 %>% 
    mutate(pfx_x_in_pv = -12 * pfx_x, pfx_in_z = 12 * pfx_z) %>% 
    ggplot(aes(pfx_x_in_pv, pfx_in_z, color = pitch_name)) + 
    geom_point(alpha = .4) + 
    scale_color_manual(values = palette) + 
    theme_classic() + 
    geom_vline(xintercept = 0, linetype = 'dashed') + 
    geom_hline(yintercept = 0, linetype = 'dashed') + 
    scale_x_continuous(limits = c(-25,25), breaks = seq(-25,25,by = 5)) + 
    scale_y_continuous(limits = c(-20,25), breaks = seq(-20,25,by = 5)) + 
    labs(title = 'Gerrit Cole 2019 Pitch Movement Profiles | Pitcher POV', x = 'Horizontal Movement In Inches',
         y = 'Vertical Movement In Inches'))










 















  








































  
  


  