# import libraries
library(ffsimulator)
library(ffscrapr)
library(tidyverse)
library(gt)
library(gtExtras)

# connect to the league
conn <- sleeper_connect(season = 2024,  league_id = "1048806886772027392")

# set wd
setwd("C:/Users/Volleyball/Downloads")

#determine the league efficiency
eff <- ff_standings(conn) |>
  dplyr::summarise(
    act_pts = sum(points_for),
    pot_pts = sum(potential_points), 
    eff = act_pts / pot_pts
  )

# set season standards
n_seasons = 1000
n_weeks = 17
reg_season = 13
act_week = 14
playoff_teams = 6
taco_place = 10

# get actual standings
standings <- ff_standings(conn = conn) %>%
  dplyr::rename(act_wins = h2h_wins, act_pf = points_for, act_pa = points_against)

# importing the data
scoring_history <- ffscrapr::ff_scoringhistory(conn, seasons = 2018:2024)

latest_rankings <- ffs_latest_rankings(type = "week")

bye_weeks <- data.frame(
  team = c("DET", "LAC", "PHI", "TEN", "KC", "LAR", "MIA", "MIN",
           "CHI", "DAL", "PIT", "SF", "CLE", "GB", "LV", "SEA", "ARI", "CAR", "NYG", "TB",
           "ATL", "BUF", "CIN", "JAC", "NO", "NYJ", "BAL", "DEN", "HOU", "IND", "NE", "WAS"),
  bye = c(5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 9, 9, 10, 10, 10, 10, 11, 11, 11, 11, 12, 12, 12, 12,
          12, 12, 14, 14, 14, 14, 14, 14)
)

latest_rankings <- left_join(x = latest_rankings, y = bye_weeks, by = 'team')

rosters <- ffs_rosters(conn)
rosters$player_name <- nflreadr::clean_player_names(rosters$player_name, lowercase = TRUE)

ff_ids <- nflreadr::load_ff_playerids()
ff_ids$name <- nflreadr::clean_player_names(ff_ids$name, lowercase = TRUE)
names(ff_ids)[names(ff_ids) == 'name'] <- 'player_name'
names(ff_ids)[names(ff_ids) == 'position'] <- 'pos'
ff_ids$pos <- replace(ff_ids$pos, ff_ids$pos == "PK", "K")

rosters <- merge(x = rosters, y = ff_ids[,c("player_name", "team", "pos", "fantasypros_id")], by = c("player_name", "team", "pos"), all.x = TRUE)

rosters$fantasypros_id <- dplyr::coalesce(rosters$fantasypros_id.x, rosters$fantasypros_id.y)

rosters <- rosters%>%
  dplyr::filter(fantasypros_id.x != 17541 | is.na(fantasypros_id.x))

lineup_constraints <- ffs_starter_positions(conn)

league_info <- ffscrapr::ff_league(conn)


# generating projections
adp_outcomes <- ffs_adp_outcomes(
  scoring_history = scoring_history,
  gp_model = "none", # or "simple"
  pos_filter = c("QB","RB","WR","TE", "K")
)

projected_scores <- ffs_generate_projections(
  adp_outcomes = adp_outcomes,
  latest_rankings = latest_rankings,
  n_seasons = n_seasons,
  weeks = 1:n_weeks, 
  rosters = rosters 
)

# calculating roster scores
roster_scores <- ffs_score_rosters(
  projected_scores = projected_scores,
  rosters = rosters
)

optimal_scores <- ffs_optimise_lineups(
  roster_scores = roster_scores,
  lineup_constraints = lineup_constraints,
  lineup_efficiency_mean = eff$eff,
  lineup_efficiency_sd = 0.05,
  best_ball = FALSE, # or TRUE
  pos_filter = c("QB","RB","WR","TE", "K")
)

summary_season <- optimal_scores |> 
  dplyr::group_by(season, franchise_id, franchise_name) |>
  dplyr::summarise(
    sf = 0,
    final = 0,
    champion = 0,
    tb_sf = 0,
    tb_final = 0,
    tb_champ = 0
  )

# mark the teams who earned the bye in the championship and toilet bowl
summary_season$sf <- ifelse(summary_season$franchise_id == 4, 1, summary_season$sf)
summary_season$sf <- ifelse(summary_season$franchise_id == 8, 1, summary_season$sf)
summary_season$tb_sf <- ifelse(summary_season$franchise_id == 3, 1, summary_season$tb_sf)
summary_season$tb_sf <- ifelse(summary_season$franchise_id == 9, 1, summary_season$tb_sf)


### simulate the playoffs

# loop through every season to determine the winner
for(i in 1:n_seasons){
  #filter to just the season we're analyzing and playoff teams
  season_playoffs <- optimal_scores |> dplyr::filter(season == i)
  
  # get the franchise id of the winning of game 1
  game_1 <- ifelse(season_playoffs[c(week == 14 & franchise_id == 5),]$actual_score > season_playoffs[c(week == 15 & franchise_id == 11),]$actual_score,
                   5, 11)
  
  # get the franchise id of the winning of game 2
  game_2 <- ifelse(season_playoffs[c(week == 14 & franchise_id == 2),]$actual_score > season_playoffs[c(week == 15 & franchise_id == 6),]$actual_score,
                   2, 6)
  
  # get the franchise id of the winning of game 3
  game_3 <- ifelse(season_playoffs[c(week == 15 & franchise_id == 4),]$actual_score > season_playoffs[c(week == 16 & franchise_id == game_1),]$actual_score,
                   4, game_1)
  
  # get the franchise id of the winning of game 4
  game_4 <- ifelse(season_playoffs[c(week == 15 & franchise_id == 8),]$actual_score > season_playoffs[c(week == 16 & franchise_id == game_2),]$actual_score,
                   8, game_2)
  
  # get the champion
  champion <- ifelse(sum(season_playoffs[c(week == 16 & franchise_id == game_3),]$actual_score, season_playoffs[c(week == 17 & franchise_id == game_3),]$actual_score) > sum(season_playoffs[c(week == 16 & franchise_id == game_4),]$actual_score, season_playoffs[c(week == 17 & franchise_id == game_4),]$actual_score),
                     game_3, game_4)
  
  
  # mark the semifinalists, finalists, and champion
  summary_season$sf <- ifelse(summary_season$franchise_id == game_1 & summary_season$season == i, 1, summary_season$sf)
  summary_season$sf <- ifelse(summary_season$franchise_id == game_2 & summary_season$season == i, 1, summary_season$sf)
  summary_season$final <- ifelse(summary_season$franchise_id == game_3 & summary_season$season == i, 1, summary_season$final)
  summary_season$final <- ifelse(summary_season$franchise_id == game_4 & summary_season$season == i, 1, summary_season$final)
  summary_season$champion <- ifelse(summary_season$franchise_id == champion & summary_season$season == i, 1, summary_season$champion)
  
  # get the losers bracket teams
  lb_seasons <- optimal_scores |> dplyr::filter(season == i)
  
  # get the franchise id of the losing team from game 1 of the losers bracket
  last_1 <- ifelse(lb_seasons[c(week == 14 & franchise_id == 1),]$actual_score < lb_seasons[c(week == 14 & franchise_id == 12),]$actual_score,
                   1, 12)
  
  # get the franchise id of the losing team from game 2 of the losers bracket
  last_2 <- ifelse(lb_seasons[c(week == 14 & franchise_id == 10),]$actual_score < lb_seasons[c(week == 14 & franchise_id == 7),]$actual_score,
                   10, 7)
  
  # get the franchise id of the losing of game 3 of the losers bracket
  last_3 <- ifelse(season_playoffs[c(week == 15 & franchise_id == 9),]$actual_score < season_playoffs[c(week == 16 & franchise_id == last_1),]$actual_score,
                   9, last_1)
  
  # get the franchise id of the losing of game 4 of the losers bracket
  last_4 <- ifelse(season_playoffs[c(week == 15 & franchise_id == 3),]$actual_score < season_playoffs[c(week == 16 & franchise_id == last_2),]$actual_score,
                   3, last_2)
  
  # determine the toilet king
  toilet <- ifelse(sum(lb_seasons[c(week == 16 & franchise_id == last_3),]$actual_score, lb_seasons[c(week == 17 & franchise_id == last_3),]$actual_score) < sum(lb_seasons[c(week == 16 & franchise_id == last_4),]$actual_score, lb_seasons[c(week == 17 & franchise_id == last_4),]$actual_score),
               last_3, last_4)
  
  # mark the semifinalists, finalists, and champion of the losers bracket
  summary_season$tb_sf <- ifelse(summary_season$franchise_id == last_1 & summary_season$season == i, 1, summary_season$tb_sf)
  summary_season$tb_sf <- ifelse(summary_season$franchise_id == last_2 & summary_season$season == i, 1, summary_season$tb_sf)
  summary_season$tb_final <- ifelse(summary_season$franchise_id == last_3 & summary_season$season == i, 1, summary_season$tb_final)
  summary_season$tb_final <- ifelse(summary_season$franchise_id == last_4 & summary_season$season == i, 1, summary_season$tb_final)
  summary_season$tb_champ <- ifelse(summary_season$franchise_id == toilet & summary_season$season == i, 1, summary_season$tb_champ)
}


# calculate season summary
playoff_odds <- summary_season |> 
  dplyr::group_by(franchise_name) |> 
  dplyr::summarize(
    sf_prob = sum(sf == 1) / dplyr::n(),
    final_prob = sum(final == 1) / dplyr::n(),
    title_prob = sum(champion == 1) / dplyr::n(),
    tb_sf_prob = sum(tb_sf == 1) / dplyr::n(),
    tb_final_prob = sum(tb_final == 1) / dplyr::n(),
    tb_king_prob = sum(tb_champ == 1) / dplyr::n()
  ) |> 
  dplyr::ungroup() |>
  dplyr::arrange(-title_prob, -final_prob, -sf_prob, -tb_king_prob, -tb_final_prob, -tb_sf_prob)

# make the chart
playoff_odds |> 
  gt::gt() |> 
  gtExtras::gt_theme_538() |> 
  gt::cols_label(
    franchise_name = 'team',
    sf_prob = 'SF',
    final_prob = 'Final',
    title_prob = 'Champ',
    tb_sf_prob = 'TB SF',
    tb_final_prob = 'TB Final',
    tb_king_prob = 'TB King'
  ) |>
  gt::cols_align(
    align = "center"
  ) |>
  gt::fmt_percent(
    c(sf_prob:tb_king_prob),
    decimals = 1
  ) |> 
  gt::opt_table_outline(style = 'solid', width = px(3), color = 'black') |>
  gt::opt_row_striping() |>
  gt::tab_style(
    style = cell_borders(
      sides = c('top', 'bottom'),
      color = "black",
      weight = px(3)
    ),
    locations = list(cells_column_labels(), cells_stubhead())
  ) |>
  gtExtras::gt_add_divider(
    columns = title_prob,
    style = 'solid',
    weight = px(3),
    color = 'black'
  ) |>
  gt::data_color(
    columns = c(sf_prob:tb_king_prob),
    method = 'numeric',
    palette = c('#FDFC96', '#2e8b57'),
    domain = c(0.0001,1),
    na_color = "white"
  ) |>
  gt::sub_values(
    columns = everything(),
    values = 0,
    replacement = "-"
  ) |>
  gt::tab_header(
    title = gt::md('2024 Simulated Postseason Outcomes')
  ) |> 
  gt::opt_align_table_header(
    align = 'center'
  ) |>
  gt::gtsave(
    filename = sprintf('%s - Week %s - Probabilities Table.png', toString(league_info$league_name), act_week),
    zoom = 1.5
    )
