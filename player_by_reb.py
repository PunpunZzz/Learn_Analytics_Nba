import pandas as pd
from nba_api.stats.endpoints import leagueleaders

ranking = leagueleaders.LeagueLeaders(
    league_id='00', #nba
    per_mode48='PerGame',
    scope='RS', #all players
    season='2025-26',
    season_type_all_star='Regular Season',
    stat_category_abbreviation='REB'
)
df_all = ranking.get_data_frames()[0]


pbp_py_p = (
    df_all
    .groupby(["PLAYER_ID", "PLAYER", "TEAM"])
    .agg({"GP": ["sum"], "REB": ["mean"]}) 
)

#concate name for agg
pbp_py_p.columns = list(map("_".join, pbp_py_p.columns.values))

#check at least 65 games played
sort_crit = "GP_sum > 64" 

resultat = (
    pbp_py_p.query(sort_crit)
    .sort_values(by="REB_mean", ascending=False)
    .head(100)
)

print(resultat.to_string())
