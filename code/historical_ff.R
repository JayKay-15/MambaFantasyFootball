#### Historical Fantasy Football ####

library(tidyverse)
library(knitr)
library(nflfastR)
library(ggrepel)
library(ggthemes)
library(gganimate)
library(ggimage)
library(gt)
library(gtExtras)


# pbp <- nflfastR::load_pbp(c(2013:2022))
pbp <- nflfastR::load_pbp(c(2017:2022))
roster <- nflfastR::fast_scraper_roster(2022)
pbp_fantasy <- pbp %>%
    mutate(fantasy_season = if_else((season<=2020 & week<=16) |
                                        (season>2020 & week<=17), TRUE, FALSE)) %>%
    filter(fantasy_season == TRUE)

unique_seasons <- unique(pbp_fantasy$season)
stats_yr <- data.frame()
stats_wk <- data.frame()

for (i in unique_seasons) {
    
    pbp_fantasy_season <- pbp_fantasy %>%
        filter(season == i)
    
    player_stats_yr <- calculate_player_stats(pbp_fantasy_season, weekly = FALSE)
    player_stats_yr$season <- i
    
    stats_yr <- bind_rows(stats_yr, player_stats_yr)
    
    player_stats_wk <- calculate_player_stats(pbp_fantasy_season, weekly = TRUE)
    player_stats_wk$season <- i
    
    stats_wk <- bind_rows(stats_wk, player_stats_wk)
    
}

pass_yds_adj <- 0.04
pass_tds_adj <- 4
rush_yds_adj <- 0.1
rush_tds_adj <- 6
rec_yds_adj <- 0.1
rec_tds_adj <- 6
rec_adj <- 1
int_adj <- -1
fum_adj <- -1

# yearly fantasy points
stats_yearly <- stats_yr %>%
    filter(position %in% c("QB","RB","WR","TE")) %>%
    mutate(games_adj = round((if_else(season <= 2020, (games/15),
                                      (games/16))*16),0),
           total_points =
               case_when(
                   position == "QB" ~ (passing_yards*pass_yds_adj + passing_tds*pass_tds_adj
                                       + rushing_yards*rush_yds_adj + rushing_tds*rush_tds_adj
                                       + interceptions*int_adj + sack_fumbles_lost*fum_adj
                                       + rushing_fumbles_lost*fum_adj),
                   position == "RB" ~ (rushing_yards*rush_yds_adj + rushing_tds*rush_tds_adj
                                       + receiving_yards*rec_yds_adj + receiving_tds*rec_tds_adj
                                       + receptions*rec_adj + rushing_fumbles_lost*fum_adj
                                       + receiving_fumbles_lost*fum_adj),
                   position == "WR" ~ (rushing_yards*rush_yds_adj + rushing_tds*rush_tds_adj
                                       + receiving_yards*rec_yds_adj + receiving_tds*rec_tds_adj
                                       + receptions*rec_adj + rushing_fumbles_lost*fum_adj
                                       + receiving_fumbles_lost*fum_adj),
                   position == "TE" ~ (rushing_yards*rush_yds_adj + rushing_tds*rush_tds_adj
                                       + receiving_yards*rec_yds_adj + receiving_tds*rec_tds_adj
                                       + receptions*rec_adj + rushing_fumbles_lost*fum_adj
                                       + receiving_fumbles_lost*fum_adj)),
           average_points = total_points/games_adj,
           touches = carries + receptions,
           pot_touches = carries + targets,
           points_per_touch = total_points/touches,
           position = factor(position, levels = c("QB","RB","WR","TE"))
    ) %>%
    select(season,player_display_name,position,recent_team,games,games_adj,
           completions:passing_epa,pacr:rushing_epa,receptions:receiving_epa,racr:wopr,
           touches,pot_touches,points_per_touch,total_points,average_points) %>%
    arrange(desc(total_points))


# weekly fantasy points
stats_weekly <- stats_wk %>%
    filter(position %in% c("QB","RB","WR","TE")) %>%
    mutate(total_points =
               case_when(
                   position == "QB" ~ (passing_yards*pass_yds_adj + passing_tds*pass_tds_adj
                                       + rushing_yards*rush_yds_adj + rushing_tds*rush_tds_adj
                                       + interceptions*int_adj + sack_fumbles_lost*fum_adj
                                       + rushing_fumbles_lost*fum_adj),
                   position == "RB" ~ (rushing_yards*rush_yds_adj + rushing_tds*rush_tds_adj
                                       + receiving_yards*rec_yds_adj + receiving_tds*rec_tds_adj
                                       + receptions*rec_adj + rushing_fumbles_lost*fum_adj
                                       + receiving_fumbles_lost*fum_adj),
                   position == "WR" ~ (rushing_yards*rush_yds_adj + rushing_tds*rush_tds_adj
                                       + receiving_yards*rec_yds_adj + receiving_tds*rec_tds_adj
                                       + receptions*rec_adj + rushing_fumbles_lost*fum_adj
                                       + receiving_fumbles_lost*fum_adj),
                   position == "TE" ~ (rushing_yards*rush_yds_adj + rushing_tds*rush_tds_adj
                                       + receiving_yards*rec_yds_adj + receiving_tds*rec_tds_adj
                                       + receptions*rec_adj + rushing_fumbles_lost*fum_adj
                                       + receiving_fumbles_lost*fum_adj)),
           position = factor(position, levels = c("QB","RB","WR","TE"))
    ) %>%
    select(season,player_display_name,position,recent_team,week,
           completions:passing_epa,pacr:rushing_epa,receptions:receiving_epa,racr:wopr,
           total_points) %>%
    arrange(desc(total_points))


stats_weekly_agg <- stats_weekly %>%
    group_by(season,player_display_name,position) %>%
    summarise(games_played = n(),
              average_points = mean(total_points),
              std_dev = sd(total_points),
              total_points = sum(total_points)
    ) %>%
    arrange(desc(total_points)) %>%
    select(season:games_played,total_points,average_points:std_dev) %>%
    left_join(stats_yearly[,c(1:4)], by = c("player_display_name" = "player_display_name",
                                            "season" = "season", "position" = "position"))

rm(list=ls()[! ls() %in% c("stats_yearly","stats_weekly","stats_weekly_agg",
                           "stats_yr","stats_wk","pbp_fantasy","roster")])

glimpse(pbp_fantasy)
glimpse(stats_yearly)
glimpse(stats_weekly)
glimpse(stats_weekly_agg)


# VORP Curve ----
# VORP calculations
vorp_yearly <- stats_yearly %>%
    filter(season %in% c(2017:2022))

unique_vorp_seasons <- unique(vorp_yearly$season)
stats_vorp_final <- data.frame()

# for (i in unique_vorp_seasons) {
#     
#     stats_vorp_yearly <- vorp_yearly %>%
#         filter(season == i)
#     
#     # Filtering top 12 each position and  next top 12 players
#     stats_vorp_filtered <- stats_vorp_yearly %>%
#         group_by(position) %>%
#         slice(19:n()) %>%
#         ungroup() %>%
#         arrange(desc(total_points)) %>%
#         slice(13:n())
#     
#     # Further filtering and selecting top players for QB and flex positions
#     stats_vorp_filtered_qb <- stats_vorp_filtered %>%
#         filter(position == "QB")
#     
#     stats_vorp_filtered_flex <- stats_vorp_filtered %>%
#         filter(position != "QB") %>%
#         slice(25:n())
#     
#     # Final selection of top players by position
#     stats_vorp_replacement <- bind_rows(stats_vorp_filtered_qb,stats_vorp_filtered_flex) %>%
#         arrange(desc(total_points)) %>%
#         group_by(position) %>%
#         slice(1) %>%
#         arrange(position)
#     
#     # Extracting replacement values for each position
#     replacement_values <- data.frame(
#         position = stats_vorp_replacement$position,
#         replacement_points = stats_vorp_replacement$total_points
#     )
#     
#     # Calculating value over replacement player (VORP)
#     stats_vorp <- stats_vorp_yearly %>%
#         mutate(
#             vorp = total_points - replacement_values$replacement_points[match(position,
#                                                           replacement_values$position)])
#     
#     # Calculating VORP total and multiplier
#     value_multiplier <- ((300-21)*12) / sum(stats_vorp %>% filter(vorp >= 0) %>% pull(vorp))
#     
#     # Calculating value based on VORP and creating new columns
#     stats_vorp_value <- stats_vorp %>%
#         mutate(value = round(vorp*value_multiplier, 0)) %>%
#         group_by(position) %>%
#         mutate(pos_rank = round(rank(-total_points, ties.method = "first"))) %>%
#         ungroup() %>%
#         mutate(vorp = round(vorp, 1))
#     
#     stats_vorp_final <- bind_rows(stats_vorp_final, stats_vorp_value)
#     
# }

for (i in unique_vorp_seasons) {
    
    stats_vorp_yearly <- vorp_yearly %>%
        filter(season == i)
    
    stats_vorp_filtered <- stats_vorp_yearly %>%
        group_by(position) %>%
        slice(13:n()) %>%
        ungroup() %>%
        arrange(desc(total_points))
    
    stats_vorp_filtered_qb <- stats_vorp_filtered %>%
        filter(position == "QB")
    
    stats_vorp_filtered_te <- stats_vorp_filtered %>%
        filter(position == "TE")
    
    stats_vorp_filtered_rb_wr <- stats_vorp_filtered %>%
        filter(position == "RB" | position == "WR") %>%
        group_by(position) %>%
        slice(13:n()) %>%
        ungroup() %>%
        arrange(desc(total_points))
    
    stats_vorp_filtered_flex <- stats_vorp_filtered_rb_wr %>%
        bind_rows(stats_vorp_filtered_te) %>%
        slice(19:n())
    
    
    # Final selection of top players by position
    stats_vorp_replacement <- bind_rows(stats_vorp_filtered_qb,stats_vorp_filtered_flex) %>%
        arrange(desc(total_points)) %>%
        group_by(position) %>%
        slice(1) %>%
        arrange(position)
    
    # Extracting replacement values for each position
    replacement_values <- data.frame(
        position = stats_vorp_replacement$position,
        replacement_points = stats_vorp_replacement$total_points
    )
    
    # Calculating value over replacement player (VORP)
    stats_vorp <- stats_vorp_yearly %>%
        mutate(
            vorp = total_points - replacement_values$replacement_points[match(position,
                                                                              replacement_values$position)])
    
    # Calculating VORP total and multiplier
    value_multiplier <- ((300-21)*12) / sum(stats_vorp %>% filter(vorp >= 0) %>% pull(vorp))
    
    # Calculating value based on VORP and creating new columns
    stats_vorp_value <- stats_vorp %>%
        mutate(value = round(vorp*value_multiplier, 0)) %>%
        group_by(position) %>%
        mutate(pos_rank = round(rank(-total_points, ties.method = "first"))) %>%
        ungroup() %>%
        mutate(vorp = round(vorp, 1))
    
    stats_vorp_final <- bind_rows(stats_vorp_final, stats_vorp_value)
    
}

glimpse(stats_vorp_final)

# VORP by season
stats_vorp_final %>%
    filter(vorp >= 0) %>%
    ggplot(aes(pos_rank, vorp, color = position)) +
    geom_line() +
    geom_point() +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    facet_wrap(~season)

# VORP by single year
stats_vorp_final %>%
    filter(vorp >= 0 & season == 2022) %>%
    ggplot(aes(pos_rank, vorp, color = position)) +
    geom_line() +
    geom_point() +
    theme_bw() +
    theme(panel.grid.minor = element_blank())

# Average VORP by position rank
stats_vorp_final %>%
    group_by(position, pos_rank) %>%
    summarise(avg_vorp = mean(vorp)) %>%
    filter(avg_vorp >= 0) %>%
    ggplot(aes(pos_rank, avg_vorp, color = position)) +
    geom_line() +
    geom_point() + 
    theme_bw()

# Totals points by season (+ VORP)
stats_vorp_final %>%
    filter(vorp >= 0) %>%
    ggplot(aes(pos_rank, total_points, color = position)) +
    geom_line() +
    geom_point() +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    facet_wrap(~season)


# Total points by SD ----
stats_weekly_agg %>%
    left_join(stats_vorp_final[,c(1:3,48)], by = c("season","player_display_name","position")) %>%
    filter(vorp >= 0 & season == 2022) %>%
    ggplot(aes(std_dev, total_points)) +
    geom_point() +
    # geom_text_repel(aes(label = player_display_name), show.legend = F) +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    facet_wrap(~position)

stats_weekly_agg %>%
    left_join(stats_vorp_final[,c(1:3,48)], by = c("season","player_display_name","position")) %>%
    filter(vorp >= 0 & position == "RB") %>%
    ggplot(aes(std_dev, total_points)) +
    geom_point() +
    # geom_text_repel(aes(label = player_display_name), show.legend = F) +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    facet_wrap(~season)

stats_weekly_agg %>%
    filter(total_points >= 150) %>%
    ggplot(aes(std_dev, total_points)) +
    geom_point() +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    facet_wrap(~position)


# Cluster curve ----
# Clustering
set.seed(214)
k_max <- 10
selected_season <- 2022
unique_pos <- unique(stats_vorp_final$position)

proj_vorp_tiers <- data.frame()

for (i in unique_pos) {
    
    km_vorp <- stats_vorp_final %>%
        filter(season == selected_season & vorp >= 0 & position == i)
    
    if (i %in% c("TE")) {
        
        km <- kmeans(km_vorp$vorp, centers = 3, nstart = 25, iter.max = 50)
        
        vorp_cluster <- bind_cols(km_vorp, cluster = km$cluster)
        
        unique_clusters <- unique(vorp_cluster$cluster)
        
        for (h in seq_along(unique_clusters)) {
            
            vorp_cluster$cluster[vorp_cluster$cluster == unique_clusters[h]] <- paste0("Tier ", h)
            
            vorp_tiers <- vorp_cluster %>%
                mutate(tier = as_factor(cluster)) %>%
                select(-cluster)
        }
        
        vorp_tiers_viz <- vorp_tiers %>%
            ggplot(aes(pos_rank, vorp, color = tier)) +
            geom_point() +
            geom_text_repel(aes(label = player_display_name), show.legend = F) +
            scale_x_continuous("Position Rank", breaks = seq(1, length(vorp_tiers), 1)) +
            scale_y_continuous("VORP") +
            theme_bw() +
            theme(panel.grid.minor = element_blank())
        
        print(vorp_tiers_viz)
        
        proj_vorp_tiers <- bind_rows(proj_vorp_tiers,vorp_tiers)
        
    } else {
        
        km <- kmeans(km_vorp$vorp, centers = 4, nstart = 25, iter.max = 50)
        
        vorp_cluster <- bind_cols(km_vorp, cluster = km$cluster)
        
        unique_clusters <- unique(vorp_cluster$cluster)
        
        for (h in seq_along(unique_clusters)) {
            
            vorp_cluster$cluster[vorp_cluster$cluster == unique_clusters[h]] <- paste0("Tier ", h)
            
            vorp_tiers <- vorp_cluster %>%
                mutate(tier = as_factor(cluster)) %>%
                select(-cluster)
        }
        
        vorp_tiers_viz <- vorp_tiers %>%
            ggplot(aes(pos_rank, vorp, color = tier)) +
            geom_point() +
            geom_text_repel(aes(label = player_display_name), show.legend = F) +
            scale_x_continuous("Position Rank", breaks = seq(1, length(vorp_tiers), 1)) +
            scale_y_continuous("VORP") +
            theme_bw() +
            theme(panel.grid.minor = element_blank())
        
        print(vorp_tiers_viz)
        
        proj_vorp_tiers <- bind_rows(proj_vorp_tiers,vorp_tiers)
        
    }
}


# HVT RB ----
# https://www.opensourcefootball.com/posts/2020-08-25-open-source-fantasy-football-visualizing-trap-backs/
roster_pos <- roster %>%
    select(gsis_id,position,full_name) %>%
    filter(position %in% c("QB","RB","WR","TE")) %>%
    distinct()

pbp_hvt <- pbp_fantasy %>%
    filter(season_type == "REG", down <= 4, play_type != "no_play" & season == 2022) %>%
    left_join(roster_pos, by = c("receiver_id" = "gsis_id"), na_matches="never") %>%
    rename(receiver_full_name = full_name,
           receiver_position = position) %>%
    left_join(roster_pos, by = c("rusher_id" = "gsis_id"), na_matches="never") %>%
    rename(rusher_full_name = full_name,
           rusher_position = position) %>%
    mutate(ten_zone_rush = if_else(yardline_100 <= 10 & rush_attempt == 1, 1, 0),
           ten_zone_pass = if_else(yardline_100 <= 10 & pass_attempt == 1 & sack == 0, 1, 0),
           ten_zone_rec = if_else(yardline_100 <= 10 & complete_pass == 1, 1, 0),
           field_touch = case_when(
               yardline_100 <= 100 & yardline_100 >= 81 & (rush_attempt == 1 | complete_pass == 1) ~ "touch_100_81",
               yardline_100 <= 80 & yardline_100 >= 61 & (rush_attempt == 1 | complete_pass == 1) ~ "touch_80_61",
               yardline_100 <= 60 & yardline_100 >= 41 & (rush_attempt == 1 | complete_pass == 1) ~ "touch_60_41",
               yardline_100 <= 40 & yardline_100 >= 21 & (rush_attempt == 1 | complete_pass == 1) ~ "touch_40_21",
               yardline_100 <= 20 & yardline_100 >= 0 & (rush_attempt == 1 | complete_pass == 1) ~ "touch_20_1",
               TRUE ~ "other"))

rb_hvt <- pbp_hvt %>%
    filter(rusher_position == "RB" | receiver_position == "RB") %>%
    mutate(player_name = if_else(is.na(rusher_player_name), receiver_player_name, rusher_player_name),
           player_id = if_else(is.na(rusher_player_id), receiver_player_id, rusher_player_id)) %>%
    group_by(player_name,
             player_id) %>%
    summarize(rush_attempts = sum(rush_attempt),
              ten_zone_rushes = sum(ten_zone_rush),
              receptions = sum(complete_pass),
              total_touches = rush_attempts + receptions,
              hvts = receptions + ten_zone_rushes,
              non_hvts = total_touches - hvts,
              hvt_pct = hvts / total_touches,
              non_hvt_pct = non_hvts / total_touches,
              hvt_rec = receptions / total_touches,
              hvt_rush = ten_zone_rushes / total_touches) %>%
    pivot_longer(cols = c(hvt_pct, non_hvt_pct, hvt_rec, hvt_rush),
                 names_to = "hvt_type", values_to = "touch_pct") %>%
    filter(total_touches >= 100)

rb_hvt %>%
    filter(hvt_type == "hvt_pct") %>%
    ggplot(aes(touch_pct, reorder(player_name, touch_pct))) +
    geom_col() +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(x = "Percent of plays",
         fill = "Distance from goal line",
         title = "Visualization of TRAP backs, displaying RB high value touches (carries inside the 10\nand catches) as a % of total touches (min 100 touches)",
         caption = "Figure: @MambaMetrics | Data: @nflfastR") +
    theme(axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_blank())

rb_hvt %>%
    filter(hvt_type %in% c("hvt_rush","hvt_rec")) %>%
    ggplot(aes(touch_pct, reorder(player_name, touch_pct), fill = hvt_type)) +
    geom_bar(position="stack", stat="identity") +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(x = "Percent of plays",
         fill = "Distance from goal line",
         title = "Visualization of TRAP backs, displaying RB high value touches (carries inside the 10\nand catches) as a % of total touches (min 100 touches)",
         caption = "Figure: @SamHoppen | Data: @nflfastR") +
    theme(axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_blank())


# HVT WR ----
# https://www.opensourcefootball.com/posts/2020-08-25-open-source-fantasy-football-visualizing-trap-backs/

wr_hvt <- pbp_hvt %>%
    filter(rusher_position == "WR" | receiver_position == "WR") %>%
    mutate(player_name = if_else(is.na(rusher_player_name), receiver_player_name, rusher_player_name),
           player_id = if_else(is.na(rusher_player_id), receiver_player_id, rusher_player_id)) %>%
    group_by(player_name,
             player_id) %>%
    summarize(rush_attempts = sum(rush_attempt),
              ten_zone_rushes = sum(ten_zone_rush),
              ten_zone_rec = sum(ten_zone_rec),
              receptions = sum(complete_pass),
              total_touches = rush_attempts + receptions,
              hvts = ten_zone_rec + ten_zone_rushes,
              non_hvts = total_touches - hvts,
              hvt_pct = hvts / total_touches,
              non_hvt_pct = non_hvts / total_touches,
              hvt_rec = ten_zone_rec / total_touches,
              hvt_rush = ten_zone_rushes / total_touches) %>%
    pivot_longer(cols = c(hvt_pct, non_hvt_pct, hvt_rec, hvt_rush),
                 names_to = "hvt_type", values_to = "touch_pct") %>%
    filter(total_touches >= 75)

wr_hvt %>%
    filter(hvt_type == "hvt_pct") %>%
    ggplot(aes(touch_pct, reorder(player_name, touch_pct))) +
    geom_col() +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(x = "Percent of plays",
         fill = "Distance from goal line",
         title = "Visualization of TRAP backs, displaying WR high value touches (carries inside the 10\nand catches) as a % of total touches (min 100 touches)",
         caption = "Figure: @MambaMetrics | Data: @nflfastR") +
    theme(axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_blank())

wr_hvt %>%
    filter(hvt_type %in% c("hvt_rush","hvt_rec")) %>%
    ggplot(aes(touch_pct, reorder(player_name, touch_pct), fill = hvt_type)) +
    geom_bar(position="stack", stat="identity") +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(x = "Percent of plays",
         fill = "Distance from goal line",
         title = "Visualization of TRAP backs, displaying WR high value touches (carries inside the 10\nand catches) as a % of total touches (min 100 touches)",
         caption = "Figure: @SamHoppen | Data: @nflfastR") +
    theme(axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_blank())

# Points by total touches ----
stats_yearly %>%
    filter(season == 2022 & position %in% c("RB","WR") & touches > 50) %>%
    select(player_display_name, position, total_points, touches, points_per_touch) %>%
    arrange(desc(points_per_touch)) %>%
    head(10) %>%
    kable()

stats_yearly %>%
    filter(season == 2022 & position == "RB" & touches > 100) %>%
    select(player_display_name, position, total_points, touches, points_per_touch) %>%
    arrange(desc(points_per_touch)) %>%
    head(10) %>%
    kable()






glimpse(pbp_fantasy)

pbp_hvt_wr <- pbp_fantasy %>%
    filter(season_type == "REG", down <= 4, play_type != "no_play" & season == 2022) %>%
    left_join(roster_pos, by = c("receiver_id" = "gsis_id"), na_matches="never") %>%
    rename(receiver_full_name = full_name,
           receiver_position = position) %>%
    left_join(roster_pos, by = c("rusher_id" = "gsis_id"), na_matches="never") %>%
    rename(rusher_full_name = full_name,
           rusher_position = position) %>%
    mutate(ten_zone_rush = if_else(yardline_100 <= 10 & rush_attempt == 1, 1, 0),
           ten_zone_rec = if_else(yardline_100 <= 10 & complete_pass == 1, 1, 0),
           tgt = if_else(complete_pass == 1 | incomplete_pass == 1, 1, 0),
           ten_zone_tgt = if_else(yardline_100 <= 10 & (complete_pass == 1 | incomplete_pass == 1), 1, 0),
           field_touch = case_when(
               yardline_100 <= 100 & yardline_100 >= 81 & (rush_attempt == 1 | complete_pass == 1) ~ "touch_100_81",
               yardline_100 <= 80 & yardline_100 >= 61 & (rush_attempt == 1 | complete_pass == 1) ~ "touch_80_61",
               yardline_100 <= 60 & yardline_100 >= 41 & (rush_attempt == 1 | complete_pass == 1) ~ "touch_60_41",
               yardline_100 <= 40 & yardline_100 >= 21 & (rush_attempt == 1 | complete_pass == 1) ~ "touch_40_21",
               yardline_100 <= 20 & yardline_100 >= 0 & (rush_attempt == 1 | complete_pass == 1) ~ "touch_20_1",
               TRUE ~ "other"))

wr_hvt <- pbp_hvt_wr %>%
    filter(rusher_position == "WR" | receiver_position == "WR") %>%
    mutate(player_name = if_else(is.na(rusher_player_name), receiver_player_name, rusher_player_name),
           player_id = if_else(is.na(rusher_player_id), receiver_player_id, rusher_player_id)) %>%
    group_by(player_name,
             player_id) %>%
    summarize(rush_attempts = sum(rush_attempt),
              ten_zone_rushes = sum(ten_zone_rush),
              tgt = sum(tgt),
              ten_zone_tgt = sum(ten_zone_tgt),
              adot = mean(air_yards, na.rm = T),
              total_pot_touches = sum(rush_attempt) + sum(tgt),
              hvt_pot = ten_zone_tgt + ten_zone_rushes,
              hvt_pot_pct = hvt_pot / total_pot_touches) %>%
    pivot_longer(cols = c(hvt_pot_pct),
                 names_to = "hvt_type", values_to = "touch_pct") %>%
    filter(total_pot_touches >= 75)

wr_hvt %>%
    ggplot(aes(touch_pct, reorder(player_name, touch_pct), fill = adot)) +
    geom_col() +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(x = "Percent of plays",
         fill = "aDot",
         title = "Visualization of TRAP backs, displaying WR high value touches (carries inside the 10\nand catches) as a % of total touches (min 100 touches)",
         caption = "Figure: @MambaMetrics | Data: @nflfastR") +
    theme(axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_blank())

wr_hvt %>%
    ggplot(aes(hvt_pot, reorder(player_name, hvt_pot), fill = adot)) +
    geom_col() +
    scale_x_continuous() +
    labs(x = "Percent of plays",
         fill = "aDot",
         title = "Visualization of TRAP backs, displaying WR high value touches (carries inside the 10\nand catches) as a % of total touches (min 100 touches)",
         caption = "Figure: @MambaMetrics | Data: @nflfastR") +
    theme(axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_blank())




    



# Player viz ----
player <- stats_yearly %>%
    filter(player_display_name == "A.J. Brown" & season == max(season)) %>%
    select(player_display_name, position, recent_team) %>%
    left_join(nflfastR::teams_colors_logos, by = c("recent_team" = "team_abbr"))


# Total and Average Points by Season - include size for games played?
stats_weekly_agg %>%
    filter(player_display_name == player$player_display_name) %>%
    ggplot(aes(season)) +
    geom_line(aes(y = total_points, color = player$team_color)) +
    geom_line(aes(y = average_points * 10, color = player$team_color2)) +
    geom_point(aes(y = total_points, color = player$team_color), show.legend = F) +
    geom_point(aes(y = average_points * 10, color = player$team_color2), show.legend = F) +
    scale_size(range = c(1, 3)) +
    scale_x_continuous(breaks = unique(stats_weekly_agg$season), minor_breaks = NULL) +
    scale_y_continuous(breaks = seq(0, max(stats_weekly_agg$total_points), 50),
                       limits = c(0, max(stats_weekly_agg$total_points)),
                       sec.axis = sec_axis(~ . / 10, name = "Average Points")) +
    scale_color_manual(
        name = "",
        values = c(player$team_color, player$team_color2),
        labels = c("Total Points", "Average Points"),
        guide = "legend") +
    labs(title = "Total & Average Points by Season", subtitle = "Only Includes Fantasy Season") + 
    xlab("Season") +
    ylab("Total Points") +
    theme_bw()


# Week Segments
stats_weekly %>%
    filter(player_display_name == player$player_display_name) %>%
    mutate(week_group = cut(week,
                            breaks = c(0, 4, 8, 12, 17),
                            labels = c("Weeks 1-4", "Weeks 5-8", "Weeks 9-12", "Weeks 13-17"))) %>%
    group_by(season, week_group) %>%
    summarise(group_average = mean(total_points)) %>%
    select(season, week_group, group_average) %>%
    spread(season, group_average) %>%
    gt() %>%
    gt_theme_538() %>%
    tab_options(
        heading.align = "center",
    ) %>%
    tab_header(title = "Points Per Game by Week Segments"
    ) %>%
    cols_label(week_group = "") %>%
    cols_align("center") %>%
    data_color(
        columns = c(-1),
        fn = scales::col_numeric(
            palette = c(player$team_color2, player$team_color),
            domain = NULL
        )
    ) %>%
    fmt_number(decimals = 1)

# Weekly Rank
stats_weekly %>%
    filter(season == max(season)) %>%
    group_by(week, position) %>%
    mutate(week_rank = rank(-total_points)) %>%
    ungroup() %>%
    filter(player_display_name == "Dak Prescott") %>%
    select(week, total_points, week_rank) %>%
    arrange(week) %>%
    gt() %>%
    gt_theme_538() %>%
    tab_options(
        heading.align = "center",
    ) %>%
    tab_header(title = "Points Per Game Rank by Week"
    ) %>%
    cols_align("center"
    ) %>%
    cols_label(
        week = "Week",
        total_points = "Points",
        week_rank = "Position Rank"
    ) %>%
    fmt_number(
        columns = total_points,
        decimals = 1
    )


# 2021 to 2022 Comparison
stats_weekly_agg %>%
    as_tibble() %>%
    filter(player_display_name == player$player_display_name & 
               (season == max(season) | season == max(season)-1)) %>%
    pivot_wider(names_from = season,
                values_from = c(games_played, total_points, average_points, std_dev)) %>%
    select(player_display_name,
           games_played_2021, total_points_2021, average_points_2021, std_dev_2021,
           games_played_2022, total_points_2022, average_points_2022, std_dev_2022) %>%
    gt() %>%
    gt_theme_538() %>%
    tab_options(
        heading.align = "center",
    ) %>%
    tab_header(
        title = "Season Comparison"
    ) %>%
    cols_align(
        "center"
    ) %>%
    cols_label(
        player_display_name = "",
        games_played_2021 = "Games",
        games_played_2022 = "Games",
        total_points_2021 = "Total Points",
        total_points_2022 = "Total Points",
        average_points_2021 = "Avg Points",
        average_points_2022 = "Avg Points",
        std_dev_2021 = "Standard Deviation",
        std_dev_2022 = "Standard Deviation"
    ) %>%
    tab_spanner(
        label = "2021",
        columns = c(games_played_2021:std_dev_2021)
    ) %>%
    tab_spanner(
        label = "2022",
        columns = c(games_played_2022:std_dev_2022)
    ) %>%
    fmt_number(
        columns = c(total_points_2021,total_points_2022,
                    average_points_2021,average_points_2022,
                    std_dev_2021,std_dev_2022),
        decimals = 1
    )

# Actual VORP
stats_vorp_final %>%
    filter(position == player$position & season == 2022 &
               (vorp > 0 | player_display_name == player$player_display_name)) %>%
    ggplot(aes(reorder(player_display_name, vorp), vorp)) +
    geom_bar(stat = "identity", fill = "black", alpha = 0.3) +
    geom_bar(data = subset(stats_vorp_final,
                           player_display_name == player$player_display_name & season == 2022),
             stat = "identity",
             fill = player$team_color) +
    # geom_text(data = subset(stats_vorp_final,
    #                         player_display_name == player$player_display_name & season == 2022),
    #           aes(label = vorp, y = vorp),
    #           position = position_dodge(width = 0.9),
    #           color = player$team_color,
    #           fontface = "bold",
    #           size = 4,
    #           nudge_y = 8) +
    # scale_x_discrete(breaks = player$player_display_name) +
    labs(title = "Value Over Replacement Player") +
    xlab("") +
    ylab("VORP") +
    coord_flip() +
    theme_bw()


# Stats by Season - QB
stats_vorp_final %>%
    filter(player_display_name == player$player_display_name) %>%
    select(season,recent_team,games,total_points,average_points,vorp,pos_rank,
           passing_yards,passing_tds,passing_epa,rushing_yards,rushing_tds) %>%
    arrange(season) %>%
    gt() %>%
    gt_theme_538() %>%
    tab_options(
        heading.align = "center",
    ) %>%
    tab_header(
        title = "Season Stats"
    ) %>%
    cols_align(
        "center"
    ) %>%
    cols_label(
        season = "Season",
        recent_team = "Team",
        games = "Games",
        total_points = "Total Points",
        average_points = "Average Points",
        vorp = "VORP",
        pos_rank = "Position Rank",
        passing_yards = "Pass Yards",
        passing_tds = "Pass TDs",
        passing_epa = "Pass EPA",
        rushing_yards = "Rush Yards",
        rushing_tds = "Rush TDs"
    ) %>%
    fmt_number(
        columns = c(total_points,average_points,vorp,passing_epa),
        decimals = 1
    )

# Stats by Season - RB
stats_vorp_final %>%
    filter(player_display_name == player$player_display_name) %>%
    select(season,recent_team,games,total_points,average_points,vorp,pos_rank,
           rushing_yards,rushing_tds,rushing_epa,
           receptions,receiving_yards,receiving_tds,receiving_epa) %>%
    arrange(season) %>%
    gt() %>%
    gt_theme_538() %>%
    tab_options(
        heading.align = "center",
    ) %>%
    tab_header(
        title = "Season Stats"
    ) %>%
    cols_align(
        "center"
    ) %>%
    cols_label(
        season = "Season",
        recent_team = "Team",
        games = "Games",
        total_points = "Total Points",
        average_points = "Average Points",
        vorp = "VORP",
        pos_rank = "Position Rank",
        rushing_yards = "Rush Yards",
        rushing_tds = "Rush TDs",
        rushing_epa = "Rush EPA",
        receptions = "Receptions",
        receiving_yards = "Receiving Yards",
        receiving_tds = "Receiving TDs",
        receiving_epa = "Receiving EPA"
    ) %>%
    fmt_number(
        columns = c(total_points,average_points,vorp,rushing_epa,receiving_epa),
        decimals = 1
    )

# Stats by Season - WR/TE
stats_vorp_final %>%
    filter(player_display_name == player$player_display_name) %>%
    select(season,recent_team,games,total_points,average_points,vorp,pos_rank,
           receptions,receiving_yards,receiving_tds,receiving_epa,
           rushing_yards,rushing_tds,rushing_epa) %>%
    arrange(season) %>%
    gt() %>%
    gt_theme_538() %>%
    tab_options(
        heading.align = "center",
    ) %>%
    tab_header(
        title = "Season Stats"
    ) %>%
    cols_align(
        "center"
    ) %>%
    cols_label(
        season = "Season",
        recent_team = "Team",
        games = "Games",
        total_points = "Total Points",
        average_points = "Average Points",
        vorp = "VORP",
        pos_rank = "Position Rank",
        receptions = "Receptions",
        receiving_yards = "Receiving Yards",
        receiving_tds = "Receiving TDs",
        receiving_epa = "Receiving EPA",
        rushing_yards = "Rush Yards",
        rushing_tds = "Rush TDs",
        rushing_epa = "Rush EPA"
    ) %>%
    fmt_number(
        columns = c(total_points,average_points,vorp,receiving_epa,rushing_epa),
        decimals = 1
    )


# Rolling Average - viz this
all_combinations <- expand.grid(player_display_name = unique(stats_weekly$player_display_name),
                                position = player$position,
                                week = 1:17)
stats_weekly %>%
    filter(position == player$position & season == max(season)) %>%
    arrange(week) %>%
    group_by(player_display_name) %>%
    mutate(avg_pts = round(cummean(total_points),2)) %>%
    ungroup() %>%
    group_by(week) %>%
    mutate(pos_rank = round(rank(-avg_pts, ties.method = "first"))) %>%
    ungroup() %>%
    complete(all_combinations) %>%
    group_by(player_display_name) %>%
    fill(total_points, avg_pts) %>%
    mutate(pos_rank = round(rank(-avg_pts, ties.method = "first"))) %>%
    ungroup() %>%
    filter(player_display_name == player$player_display_name) %>%
    select(player_display_name, position, week,
           total_points, avg_pts, pos_rank) %>%
    gt()

stats_weekly %>%
    filter(position == player$position & season == max(season)) %>%
    arrange(week) %>%
    group_by(player_display_name) %>%
    mutate(avg_pts = round(cummean(total_points),2)) %>%
    ungroup() %>%
    group_by(week) %>%
    mutate(pos_rank = round(rank(-avg_pts, ties.method = "first"))) %>%
    ungroup() %>%
    complete(all_combinations) %>%
    group_by(player_display_name) %>%
    fill(total_points, avg_pts) %>%
    mutate(pos_rank = round(rank(-avg_pts, ties.method = "first"))) %>%
    ungroup() %>%
    filter(player_display_name == player$player_display_name) %>%
    select(player_display_name, position, week,
           total_points, avg_pts, pos_rank) %>%
    ggplot(aes(x = week)) +
    geom_line(aes(y = total_points, color = player_display_name), linetype = "dashed") +
    geom_line(aes(y = avg_pts, color = player_display_name), linetype = "solid") +
    scale_color_discrete(name = "Player Display Name") +
    ylab("Total Points") +
    scale_y_continuous(
        sec.axis = sec_axis(~., name = "Average Points", labels = function(x) round(x, 2))
    ) +
    theme_minimal()


# Total and average points rank table
stats_yearly %>%
    group_by(season, position) %>%
    mutate(avg_pts_rank = round(rank(-average_points, ties.method = "first")),
           tot_pts_rank = round(rank(-total_points, ties.method = "first"))) %>%
    ungroup() %>%
    filter(player_display_name == player$player_display_name) %>%
    select(season, player_display_name, position,
           total_points, tot_pts_rank, average_points, avg_pts_rank) %>%
    arrange(season) %>%
    gt()


# Total, average points, standard dev rank by overall position (filter vorp > 0) - by season
stats_vorp_final %>%
    filter(vorp > 0) %>%
    left_join(stats_weekly_agg %>% select(season,player_display_name,std_dev),
              by = c("player_display_name" = "player_display_name",
                     "season" = "season")) %>%
    group_by(season, position) %>%
    summarise(avg_pts = round(mean(average_points, ties.method = "first"),1),
              tot_pts = round(mean(total_points, ties.method = "first"),1),
              std_dev = round(mean(std_dev, ties.method = "first"),1)) %>%
    gt()


stats_vorp_final %>%
    filter(vorp > 0) %>%
    left_join(stats_weekly_agg %>% select(season,player_display_name,std_dev),
              by = c("player_display_name" = "player_display_name",
                     "season" = "season")) %>%
    group_by(season, position) %>%
    summarise(avg_pts = round(mean(average_points, ties.method = "first"),1),
              tot_pts = round(mean(total_points, ties.method = "first"),1),
              std_dev = round(mean(std_dev, ties.method = "first"),1)) %>%
    ggplot(aes(x = as.factor(season), y = std_dev, fill = position)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Season", y = "Standard Deviation",
         title = "Comparison of Standard Deviation between Positions over Years") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_discrete(name = "Position")
    

# stats_vorp_final %>%
#     filter(vorp > 0) %>%
#     left_join(stats_weekly_agg %>% select(season,player_display_name,std_dev),
#               by = c("player_display_name" = "player_display_name",
#                      "season" = "season")) %>%
#     group_by(season, position) %>%
#     mutate(avg_pts_rank = round(rank(-average_points, ties.method = "first")),
#            tot_pts_rank = round(rank(-total_points, ties.method = "first")),
#            sd_pts_rank = round(rank(std_dev, ties.method = "first"))) %>%
#     select(season, player_display_name, position, recent_team,
#            total_points, tot_pts_rank, average_points, avg_pts_rank,
#            std_dev, sd_pts_rank) %>%
#     gt()





