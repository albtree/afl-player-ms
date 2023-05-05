library(tidyverse)
library(fitzRoy)
library(gt)
library(gtExtras)
library(nflverse)
library(googledrive)
library(hrbrthemes)
library(RColorBrewer)

use_description()

# Roster Pull -------------------------------------------------------------
df_players <- map_dfr(2014:2022, fetch_player_stats_fryzigg) %>%
  filter(match_round == "1"|
           match_round == "2"|
           match_round == "3"|
           match_round == "4"|
           match_round == "5"|
           match_round == "6"|
           match_round == "7"|
           match_round == "8"|
           match_round == "9"|
           match_round == "10"|
           match_round == "11"|
           match_round == "12"|
           match_round == "13"|
           match_round == "14"|
           match_round == "15"|
           match_round == "16"|
           match_round == "17"|
           match_round == "18"|
           match_round == "19"|
           match_round == "20"|
           match_round == "21"|
           match_round == "22"|
           match_round == "23") %>%
  mutate(full_name = paste(player_first_name, player_last_name),
         season = str_sub(match_date, 1,4)) %>%
  dplyr::select(afl_fantasy_score, season, match_round, full_name, player_team, player_position) %>%
  mutate(season = as.numeric(season)) %>%
  drop_na(afl_fantasy_score)


# Position Mutating -------------------------------------------------------


df_positions_teams <- df_players %>%
  dplyr::select(season, full_name, player_team, player_position) %>%
  mutate(fantasy_position = case_when(player_position == "RK" ~ "Ruck",
                                      player_position == "R" ~ "Mid",
                                      player_position == "RR" ~ "Mid",
                                      player_position == "FF" ~ "Forward",
                                      player_position == "FB" ~ "Back",
                                      player_position == "FPR" ~ "Forward",
                                      player_position == "BPR" ~ "Back",
                                      player_position == "CHB" ~ "Back",
                                      player_position == "CHF" ~ "Forward",
                                      player_position == "WR" ~ "Mid",
                                      player_position == "C" ~ "Mid",
                                      player_position == "HBFR" ~ "Back",
                                      player_position == "HBFL" ~ "Back",
                                      player_position == "BPL" ~ "Back",
                                      player_position == "HFFL" ~ "Forward",
                                      player_position == "WL" ~ "Mid",
                                      player_position == "FPL" ~ "Forward",
                                      player_position == "HFFR" ~ "Forward",
                                      TRUE ~ player_position)) 

df_positions_players <- df_positions_teams %>%
  group_by(season, full_name, player_team, fantasy_position) %>%
  summarise(count_of_position = n()) %>%
  ungroup()%>%
  filter(fantasy_position != "INT", fantasy_position != "SUB") %>%
  group_by(season, full_name, player_team) %>%
  arrange(-count_of_position) %>%
  slice_max(count_of_position, n = 1, with_ties = FALSE) %>%
  ungroup()

# Team Stats --------------------------------------------------------------

df_teams20 <- df_players %>%
  filter(season == 2020) %>%
  mutate(team_games_played = 17)

df_teamsnot20 <- df_players %>%
  filter(season <2020 | season > 2020) %>%
  mutate(team_games_played = 22)

df_teams <- bind_rows(df_teams20, df_teamsnot20)

team_totals <- df_teams %>%
  group_by(player_team,season) %>%
  summarise(team_fp_season = sum(afl_fantasy_score)) %>%
  ungroup() %>%
  mutate(team_games_played = if_else(season == 2020, 17, 22),
         team_fp_pergame = team_fp_season/team_games_played)


# Joining Team Stats and Player Stats -------------------------------------


player_totals <- df_players %>%
  group_by(full_name, player_team, season) %>%
  summarise(player_fpoint_season = sum(afl_fantasy_score),
            games_played = n()) %>%
  ungroup() %>%
  mutate(player_fpoint_per_game = (player_fpoint_season/games_played)) %>%
  left_join(team_totals, by = c('player_team' = 'player_team', 'season' = 'season'), na_matches = "never") %>%
  mutate(player_share_pergame = player_fpoint_per_game/team_fp_pergame,
         player_share_season = player_fpoint_season/team_fp_season) %>%
  mutate_if(is.numeric, round, 4) %>%
  mutate(player_share_pergame_perc = player_share_pergame*100) %>%
  left_join(df_positions_players, by = c('full_name' = 'full_name', 'player_team' = 'player_team', 'season' = 'season'), na_matches = "never") %>%
  dplyr::select(full_name, player_team, season, games_played, fantasy_position, player_fpoint_season, player_fpoint_per_game,
                player_share_season, player_share_pergame, player_share_pergame_perc, player_share_season, team_fp_season,
                team_fp_pergame) %>%
  mutate(player_fpoint_per_game = round(player_fpoint_per_game, digits = 2),
         team_fp_pergame = round(team_fp_pergame, digits = 2))


# Tables ------------------------------------------------------------------


player_totals %>%
  filter(games_played > 15) %>%
  dplyr::select(full_name, season, fantasy_position, player_team, player_share_pergame_perc) %>%
  arrange(desc(player_share_pergame_perc))%>%
  dplyr::slice(1:15) %>%
  gt() %>%
  tab_header(
    title = "Top AFL Players in Fantasy Market share",
    subtitle = "Top 15 - 2014-2022"
  ) %>%
  cols_align(
    "center",
    columns = c(full_name, season, fantasy_position, player_team, player_share_pergame_perc)
  ) %>% 
  cols_label(
    full_name = "Player",
    season = "Season",
    fantasy_position = "Pos.",
    player_team = "Team",
    player_share_pergame_perc = "Market Share %"
  ) %>%
  tab_source_note(
    source_note = "Table: @TAlbTree. Idea: @RowanTFL"
  ) %>%
  tab_source_note(
    source_note = "Data: #fitzRoy package"
  ) %>%
  gt_hulk_col_numeric(player_share_pergame_perc) %>% 
  tab_options(footnotes.font.size = 12)

player_totals %>%
  filter(games_played > 15, season == 2022) %>%
  dplyr::select(full_name, season, fantasy_position, player_team, player_share_pergame_perc, player_fpoint_per_game) %>%
  arrange(desc(player_share_pergame_perc))%>%
  dplyr::slice(1:15) %>%
  gt() %>%
  tab_header(
    title = "Top AFL Players in Fantasy Market share",
    subtitle = "Top 15 - Season: 2022"
  ) %>%
  cols_align(
    "center",
    columns = c(full_name, season, fantasy_position, player_team, player_share_pergame_perc, player_fpoint_per_game)
  ) %>% 
  cols_label(
    full_name = "Player",
    season = "Season",
    fantasy_position = "Pos.",
    player_team = "Team",
    player_share_pergame_perc = "Market Share %",
    player_fpoint_per_game = "FPPG"
  ) %>%
  tab_source_note(
    source_note = "FPPG = Fantasy Points Per Game"
  ) %>%
  tab_source_note(
    source_note = "Table: @TAlbTree. Idea: @RowanTFL"
  ) %>%
  tab_source_note(
    source_note = "Data: #fitzRoy package"
  ) %>%
  gt_hulk_col_numeric(player_share_pergame_perc:player_fpoint_per_game) %>% 
  tab_options(footnotes.font.size = 12)




# Team Total Charts -------------------------------------------------------
colourCount = length(unique(team_totals$player_team))
getPalette = colorRampPalette(brewer.pal(9, "Set2"))


team_totals %>%
  mutate(team_totals_year = paste(player_team, season)) %>%
  slice_max(team_fp_pergame, n = 20) %>%
  ggplot(aes(x = reorder(team_totals_year, team_fp_pergame), y = team_fp_pergame)) +
  geom_col(aes(colour = player_team, fill = player_team, alpha = 0.6)) +
  coord_flip() +
  labs(title = "Top 20 AFL Fantasy Team Averages",
       subtitle = "Seasons 2014-2022",
       caption = "Chart: @TAlbTree data: #fitzRoy") +
  xlab("") +
  ylab("Team Fantasy Points Per Game") +
  theme_ipsum_rc() +
  theme(legend.position = "",
        axis.text.y = element_text(size = 8))+
  scale_fill_manual(values = getPalette(colourCount))+
  scale_colour_manual(values = getPalette(colourCount))


team_totals %>%
  filter(season == 2022) %>%
  ggplot(aes(x = reorder(player_team, team_fp_pergame), y = team_fp_pergame)) +
  geom_col(aes(colour = player_team, fill = player_team, alpha = 0.6)) +
  coord_flip() +
  labs(title = "AFL Fantasy Team Scoring Averages",
       subtitle = "Season = 2022",
       caption = "Chart: @TAlbTree data: #fitzRoy") +
  xlab("") +
  ylab("Team Fantasy Points Per Game")  +
  theme_ipsum_rc() +
  theme(legend.position = "",
        axis.text.y = element_text(size = 8))+
  scale_fill_manual(values = getPalette(colourCount))+
  scale_colour_manual(values = getPalette(colourCount))
