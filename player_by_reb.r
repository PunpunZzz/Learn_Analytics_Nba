get_eff <- function(saison) {
  #select data to take from
  data <- nba_leaguedashplayerstats(
    season = saison,
    season_type = "Regular Season",
    measure_type = "Base",
    per_mode = "Totals"
  )
  data$LeagueDashPlayerStats %>%
    mutate(across(c(GP, PTS, REB, AST, STL, BLK, FGA, FGM, FTA, FTM, TOV), as.numeric)) %>%
    mutate(
      #calculate total
      EFF = PTS + REB + AST + STL + BLK - (FGA - FGM) - (FTA - FTM) - TOV,
      #on average to not "discriminate" from the number of games played
      EFF_AVG = round(EFF / GP, 1)
    ) %>%
    select(PLAYER_ID, PLAYER = PLAYER_NAME, TEAM = TEAM_ABBREVIATION, GP, EFF_AVG)
}

eff_2025 <- get_eff("2024-25")
eff_2026 <- get_eff("2025-26")

comparaison <- inner_join(eff_2025, eff_2026, by = "PLAYER_ID", suffix = c("_2025", "_2026")) %>%
  select(PLAYER = PLAYER_2026, TEAM = TEAM_2026, GP_2025, GP_2026, EFF_2025 = EFF_AVG_2025, EFF_2026 = EFF_AVG_2026) %>%
  mutate(Diff = EFF_2026 - EFF_2025) %>% #diff between the two season
  arrange(desc(Diff))

print(as.data.frame(head(comparaison, 100)))

#scatter plot
ggplot(data = comparaison, aes(x = EFF_2025, y = EFF_2026)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(aes(color = Diff), size = 2.5) +
  geom_text(data = comparaison %>% filter(EFF_2026 > 28 | abs(Diff) > 6),
            aes(label = PLAYER), size = 3, vjust = -0.8, check_overlap = TRUE) +
  scale_color_gradient2(low = "#850018", mid = "grey80", high = "#00297c", midpoint = 0) +
  labs(
    title = "EFF per game: 2024-25 vs 2025-26",
    x = "EFF 2024-25",
    y = "EFF 2025-26",
    color = "Evolution"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12)
  )


#lolipop for 1 team
comparer_equipe <- function(equipe) {
  ggplot(data = comparaison %>% 
  filter(TEAM == equipe)) + #get only from the team u want
    geom_segment(aes(x = reorder(PLAYER, EFF_2026), xend = PLAYER, y = EFF_2025, yend = EFF_2026)) +
    geom_point(aes(x = PLAYER, y = EFF_2025), color = "#00297c", size = 3) +
    geom_point(aes(x = PLAYER, y = EFF_2026), color = "#850018", size = 3) +
    coord_flip() +
    labs(
      title = paste("EFF evolution for: ", equipe),
      subtitle = "Blue = 2024-25 and Red = 2025-26",
      x = "Player",
      y = "Efficiency per game"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 12)
    )
}
comparer_equipe("DEN")
comparer_equipe("LAL")
comparer_equipe("NYK")
comparer_equipe("CHI")