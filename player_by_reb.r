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

# 3. Affichage du résultat
print(as.data.frame(classement_avg_2026))
