library(tidyverse)
library(nflfastR)
library(caret)
library(openxlsx)

# stats <- load_player_stats(seasons = 2022)
# stats<- stats %>% filter(week < 18)


pbp <- nflfastR::load_pbp(2018:2022)
pbp <- pbp %>% filter(week <= 18)

overall <- calculate_player_stats(pbp, weekly = FALSE)

overall_new <- overall %>%
    filter(position %in% c("QB","RB","WR","TE")) %>%
    mutate(fantasy_points_new = 
               case_when(
                   position == "QB" ~ (passing_yards*0.04 + passing_tds*4 + rushing_yards*0.1 + rushing_tds*6),
                   position == "RB" ~ (rushing_yards*0.1 + rushing_tds*6 + receptions +
                                           receiving_yards*0.1 + receiving_tds*6),
                   position == "WR" ~ (receptions + receiving_yards*0.1 + receiving_tds*6 +
                                           rushing_yards*0.1 + rushing_tds*6),
                   position == "TE" ~ (receptions + receiving_yards*0.1 + receiving_tds*6 +
                                           rushing_yards*0.1 + rushing_tds*6)
                       ),
           fantasy_ppg = fantasy_points_new/games
           ) %>%
    arrange(desc(fantasy_ppg))

qb <- overall_new %>% filter(position == "QB")
rb <- overall_new %>% filter(position == "RB")
wr <- overall_new %>% filter(position == "WR")
te <- overall_new %>% filter(position == "TE")


# fn <- "2022_ff_stats"
# u <- paste0("/Users/Jesse/Desktop/",fn,".xlsx")
# 
# wb <- createWorkbook()
# addWorksheet(wb, sheetName = "qb")
# addWorksheet(wb, sheetName = "rb")
# addWorksheet(wb, sheetName = "wr")
# addWorksheet(wb, sheetName = "te")
# writeData(wb, sheet = "qb", x = qb)
# writeData(wb, sheet = "rb", x = rb)
# writeData(wb, sheet = "wr", x = wr)
# writeData(wb, sheet = "te", x = te)
# saveWorkbook(wb, file = u)

pbp <- nflfastR::load_pbp(c(2013:2022))
pbp <- pbp %>% filter(week <= 17)

stats_weekly <- calculate_player_stats(pbp, weekly = TRUE)

stats_df <- stats_weekly %>%
    filter(position %in% c("QB","RB","WR","TE")) %>%
    mutate(fantasy_points_mfl = 
               case_when(
                   position == "QB" ~ (passing_yards*0.04 + passing_tds*4 + rushing_yards*0.1 + rushing_tds*6),
                   position == "RB" ~ (rushing_yards*0.1 + rushing_tds*6 + receptions +
                                           receiving_yards*0.1 + receiving_tds*6),
                   position == "WR" ~ (receptions + receiving_yards*0.1 + receiving_tds*6 +
                                           rushing_yards*0.1 + rushing_tds*6),
                   position == "TE" ~ (receptions + receiving_yards*0.1 + receiving_tds*6 +
                                           rushing_yards*0.1 + rushing_tds*6)
                   )
           ) %>%
    select(player_display_name, position, recent_team, season, week, fantasy_points_mfl) %>%
    group_by(season, player_display_name, position) %>%
    summarise(games_played = n(),
              total_points = sum(fantasy_points_mfl),
              average_points = mean(fantasy_points_mfl),
              sd_dev = sd(fantasy_points_mfl)
    ) %>%
    arrange(desc(total_points))


stats_df_2022 <- stats_df %>%
    filter(season == 2022 & games_played >= 5)

# using total points
stats_df_rm_top_12_tp <- stats_df_2022 %>%
    group_by(season, position) %>%
    slice(19:n()) %>%
    ungroup() %>%
    arrange(desc(total_points))

stats_df_rm_36_tp <- stats_df_rm_top_12_tp %>%
    slice(33:n()) %>%
    arrange(desc(total_points))

stats_df_baseline_tp <- stats_df_rm_36_tp %>%
    group_by(position) %>%
    slice_max(total_points, n = 1) %>%
    ungroup() %>%
    arrange(desc(total_points))
    
stats_df_baseline_tp


stats_df_2022 <- stats_df %>%
    filter(season == 2022 & games_played >= 5)

# using average points
stats_df_rm_top_12_ap <- stats_df_2022 %>%
    group_by(season, position) %>%
    slice(19:n()) %>%
    ungroup() %>%
    arrange(desc(average_points))

stats_df_rm_36_ap <- stats_df_rm_top_12_ap %>%
    slice(33:n()) %>%
    arrange(desc(average_points))

stats_df_baseline_ap <- stats_df_rm_36_ap %>%
    group_by(position) %>%
    slice_max(average_points, n = 1) %>%
    ungroup() %>%
    arrange(desc(average_points))

stats_df_baseline_ap




# fn <- "2019_2022_ff_stats"
# u <- paste0("/Users/Jesse/Desktop/",fn,".xlsx")
# 
# wb <- createWorkbook()
# addWorksheet(wb, sheetName = "stats")
# writeData(wb, sheet = "stats", x = stats_df)
# saveWorkbook(wb, file = u)


stats_df_2019_2022 <- stats_df %>%
    filter(games_played >= 5)

stats_df_top_18 <- stats_df_2019_2022 %>%
    group_by(season, position) %>%
    slice(25:36) %>%
    ungroup() %>%
    arrange(desc(total_points))

stats_df_top_18 %>%
    ggplot(aes(position,total_points)) +
    geom_boxplot() + facet_wrap(~season)

stats_df_top_18 %>%
    ggplot(aes(sd_dev,total_points)) +
    geom_point(aes(color = position)) + facet_wrap(~season)




stats_df_2019_2022 <- stats_df %>%
    filter(games_played >= 5)

stats_df_top_18 <- stats_df_2019_2022 %>%
    group_by(season, position) %>%
    slice(13:24) %>%
    ungroup() %>%
    arrange(desc(average_points))

stats_df_top_18 %>%
    ggplot(aes(position,average_points)) +
    geom_boxplot() + facet_wrap(~season)

stats_df_top_18 %>%
    ggplot(aes(sd_dev,average_points)) +
    geom_point(aes(color = position)) + facet_wrap(~season)


## projections for all players

