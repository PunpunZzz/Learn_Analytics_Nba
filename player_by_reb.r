library(hoopR)
library(tidyverse)

#same as pyton
nba_data <- nba_leagueleaders(
  league_id = "00",
  per_mode48 = "Totals", 
  scope = "RS",
  season = "2025-26", 
  season_type_all_star = "Regular Season",
  stat_category_abbreviation = "REB"
)
df_all <- nba_data$LeagueLeaders

classement_avg_2026 <- df_all %>%
  mutate(
    REB = as.numeric(REB),
    GP = as.numeric(GP)
  ) %>%
  mutate(REB_AVG = round(REB / GP, 1)) %>% #create the calcul
  select(PLAYER, TEAM, GP, REB_AVG) %>%
  filter(GP > 64) %>% 
  arrange(desc(REB_AVG)) %>%
  head(100)

#print results
print(as.data.frame(classement_avg_2026))

#now creating graphs
top_30 <- classement_avg_2026 %>%
  head(30)

#firsr plot lines ranking with team colors
ggplot(data = top_30, aes(x = reorder(PLAYER, REB_AVG), y = REB_AVG, fill = TEAM)) +
  geom_col(show.legend = TRUE) +
  coord_flip() + #to have it on the side (rankign top bottom)
  labs(
    title = "Top 30 best rebounders for 2025-26 season",
    subtitle = "Average rebounds per game / Minimum 65 matches played",
    x = "Player",
    y = "Rebounding average",
    fill = "Team"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.text.y = element_text(size = 10)
  )

#now scatter point with games palyed and avg
ggplot(data = classement_avg_2026, aes(x = REB_AVG, y = GP)) +
  geom_point(aes(color = TEAM), size = 3) +
  labs(
    title = "Top 100 best rebounders for 2025-26 season",
    subtitle = "Average rebounds per game / Minimum 65 matches played",
    x = "Rebounding average",
    y = "Games played",
    fill = "Team"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
  )

#to have all the data
classement_avg_2026_full <- df_all %>%
  mutate(
    REB = as.numeric(REB),
    GP = as.numeric(GP)
  ) %>%
  mutate(REB_AVG = round(REB / GP, 1)) %>% 
  select(PLAYER, TEAM, GP, REB_AVG) %>%
  arrange(desc(REB_AVG)) 

#box plot with full teams disparities
ggplot(data = classement_avg_2026_full, aes(x = reorder(TEAM, REB_AVG), y = REB_AVG, fill = TEAM)) +
  geom_boxplot(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Average per teams",
    x = "Teams",
    y = "Rebounding average"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
  )

#new dp dispar
data_disparite <- classement_avg_2026_full %>%
  group_by(TEAM) %>%
  arrange(desc(REB_AVG)) %>%
  slice_head(n = 2) %>% #keep 2 bests
  mutate(Rang = paste0("R", row_number())) %>% 
  ungroup() %>%
  select(TEAM, REB_AVG, Rang) %>%
  pivot_wider(names_from = Rang, values_from = REB_AVG) %>% 
  mutate(Gap = R1 - R2) %>% 
  arrange(Gap)

#creating lolipop
ggplot(data = data_disparite) +
  geom_segment(aes(x = reorder(TEAM, Gap), xend = TEAM, y = R2, yend = R1)) +
  geom_point(aes(x = TEAM, y = R2), color = "#00297c", size = 3.5) +
  geom_point(aes(x = TEAM, y = R1), color = "#850018", size = 3.5) +
  coord_flip() +
  labs(
    title = "Disparities in rebounding between best and second-best player for each team (2025-26)",
    subtitle = "Red = Best /  Blue = Second",
    x = "Team",
    y = "Rebounding average"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    panel.grid.minor = element_blank()
  )