#### Historical Fantasy Football ####

library(tidyverse)
library(knitr)
library(nflfastR)
library(ggrepel)
library(ggthemes)
library(gganimate)
library(ggimage)
# library(gridExtra)
# library(patchwork)
library(gt)
library(gtExtras)


# pbp <- nflfastR::load_pbp(c(2013:2022))
pbp_fantasy <- nflfastR::load_pbp(c(2017:2022))
roster <- nflfastR::fast_scraper_roster(2022)
pbp_fantasy <- pbp_fantasy %>%
    mutate(fantasy_season = if_else((season<=2020 & week<=16) |
                                        (season>2020 & week<=17), TRUE, FALSE)) %>%
    filter(fantasy_season == TRUE)

stats_yr <- data.frame()
stats_wk <- data.frame()

for (i in unique(pbp_fantasy$season)) {
    
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
    select(season:games_played,total_points,average_points:std_dev)

# yearly fantasy points
stats_yearly <- stats_yr %>%
    filter(position %in% c("QB","RB","WR","TE")) %>%
    # mutate(games_adj = round((if_else(season <= 2020, (games/15),
    #                                   (games/16))*16),0),
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
           average_points = total_points/games,
           touches = carries + receptions,
           pot_touches = carries + targets,
           points_per_touch = total_points/touches,
           position = factor(position, levels = c("QB","RB","WR","TE"))) %>%
    left_join(stats_weekly_agg %>% select(season,player_display_name,position,std_dev),
              by = c("player_display_name" = "player_display_name",
                     "season" = "season", "position" = "position"))

# VORP calculations
vorp_yearly <- stats_yearly %>%
    filter(season %in% c(2017:2022))

stats_vorp_final <- data.frame()

for (i in unique(vorp_yearly$season)) {
    
    stats_vorp_yearly <- vorp_yearly %>%
        filter(season == i) %>%
        arrange(desc(total_points))
    
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
        slice(13:n())
    
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
        mutate(vorp = total_points - replacement_values$replacement_points[match(position,
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

stats_yearly <- stats_vorp_final


# Adjusted points
vorp_yearly <- stats_yearly %>%
    filter(season %in% c(2017:2022))

stats_vorp_final <- data.frame()

for (i in unique(vorp_yearly$season)) {
    
    stats_vorp_yearly <- vorp_yearly %>%
        filter(season == i) %>%
        arrange(desc(average_points))
    
    stats_vorp_filtered <- stats_vorp_yearly %>%
        group_by(position) %>%
        slice(13:n()) %>%
        ungroup() %>%
        arrange(desc(average_points))
    
    stats_vorp_filtered_qb <- stats_vorp_filtered %>%
        filter(position == "QB")
    
    stats_vorp_filtered_te <- stats_vorp_filtered %>%
        filter(position == "TE")
    
    stats_vorp_filtered_rb_wr <- stats_vorp_filtered %>%
        filter(position == "RB" | position == "WR") %>%
        group_by(position) %>%
        slice(13:n()) %>%
        ungroup() %>%
        arrange(desc(average_points))
    
    stats_vorp_filtered_flex <- stats_vorp_filtered_rb_wr %>%
        bind_rows(stats_vorp_filtered_te) %>%
        slice(13:n())
    
    # Final selection of top players by position
    stats_vorp_replacement <- bind_rows(stats_vorp_filtered_qb,stats_vorp_filtered_flex) %>%
        arrange(desc(average_points)) %>%
        group_by(position) %>%
        slice(1) %>%
        arrange(position)
    
    # Extracting replacement values for each position
    replacement_values <- data.frame(
        position = stats_vorp_replacement$position,
        replacement_points = stats_vorp_replacement$average_points
    )
    
    # Calculating value over replacement player (VORP)
    stats_vorp <- stats_vorp_yearly %>%
        mutate(vorp = average_points - replacement_values$replacement_points[match(position,
                                                                                 replacement_values$position)])
    
    # Calculating VORP total and multiplier
    value_multiplier <- ((300-21)*12) / sum(stats_vorp %>% filter(vorp >= 0) %>% pull(vorp))
    
    # Calculating value based on VORP and creating new columns
    stats_vorp_value <- stats_vorp %>%
        mutate(value = round(vorp*value_multiplier, 0)) %>%
        group_by(position) %>%
        mutate(pos_rank = round(rank(-average_points, ties.method = "first"))) %>%
        ungroup() %>%
        mutate(vorp = round(vorp, 1),
               games_adj = if_else(season <= 2020, TRUE, FALSE),
               missed_games = if_else(games_adj == TRUE, 15-games, 16-games),
               total_points_adj = total_points + (missed_games * replacement_values$replacement_points[match(position,
                                                                                                             replacement_values$position)]),
               average_points_adj = total_points_adj/(if_else(season <= 2020, 15, 16))) %>%
        select(-missed_games, -games_adj)
    
    stats_vorp_final <- bind_rows(stats_vorp_final, stats_vorp_value)
    
}

stats_yearly <- stats_yearly %>%
    left_join(stats_vorp_final %>% select(player_id, season,
                                          total_points_adj, average_points_adj),
              by = c("player_id" = "player_id",
                     "season" = "season"))


rm(list=ls()[! ls() %in% c("stats_yearly","stats_weekly","roster","pbp_fantasy")])

glimpse(stats_yearly)
glimpse(stats_weekly)

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
# 
# stats_yearly <- stats_vorp_final

# Position and strategy analysis ----

# Plot 1: Line plot of VORP vs. pos_rank, colored by position, faceted by season
plot1 <- stats_yearly %>%
    filter(vorp > 0) %>%
    ggplot(aes(pos_rank, vorp, color = position)) +
    geom_line() +
    geom_point() +
    labs(x = "Position Rank", y = "VORP", color = "Position") +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    facet_wrap(~season) +
    ggtitle("VORP vs. Position Rank by Position and Season")
plot1

# Plot 2: Line plot of VORP vs. pos_rank for season 2022, colored by position
plot2 <- stats_yearly %>%
    filter(vorp > 0 & season == 2022) %>%
    ggplot(aes(pos_rank, vorp, color = position)) +
    geom_line() +
    geom_point() +
    labs(x = "Position Rank", y = "VORP", color = "Position") +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    ggtitle("VORP vs. Position Rank for Season 2022")
plot2

# Plot 3: Box plot of total_points vs. position, faceted by season, for players with at least 6 games played
plot3 <- stats_yearly %>%
    filter(games >= 6) %>%
    ggplot(aes(position, total_points)) +
    geom_boxplot() +
    labs(x = "Position", y = "Total Points") +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    facet_wrap(~season) +
    ggtitle("Box Plot of Total Points by Position and Season (Min 6 Games Played)")
plot3

# Plot 4: Scatter plot of total_points vs. std_dev, colored by position, faceted by season, for players with positive VORP
plot4 <- stats_yearly %>%
    filter(vorp > 0 & season == 2022) %>%
    ggplot(aes(std_dev, total_points)) +
    geom_point() +
    geom_text_repel(aes(label = player_display_name), show.legend = F) +
    labs(x = "Standard Deviation", y = "Total Points") +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    facet_wrap(~position) +
    ggtitle("Total Points vs. Standard Deviation (Season 2022)")
plot4

# Plot 5: Bar plot of average standard deviation by position for each season
plot5 <- stats_yearly %>%
    filter(vorp > 0) %>%
    group_by(season, position) %>%
    summarise(avg_pts = round(mean(average_points, ties.method = "first"), 1),
              tot_pts = round(mean(total_points, ties.method = "first"), 1),
              std_dev = round(mean(std_dev, ties.method = "first"), 1)) %>%
    ggplot(aes(x = as.factor(season), y = std_dev, fill = position)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Season", y = "Standard Deviation",
         title = "Comparison of Standard Deviation between Positions over Years") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_discrete(name = "Position")
plot5

# Table 1: Summary table of average and total points by position and season
table1 <- stats_yearly %>%
    filter(vorp > 0) %>%
    group_by(position, season) %>%
    summarise(avg_pts = round(mean(average_points, ties.method = "first"), 1),
              tot_pts = round(mean(total_points, ties.method = "first"), 1),
              std_dev = round(mean(std_dev, ties.method = "first"), 1)) %>%
    gt() %>%
    tab_header(
        title = md("**Summary of Points by Position and Season**"),
        subtitle = "Data summarized based on VORP > 0"
    )
table1

# Arrange plots and table in a grid layout
grid_layout <- gridExtra::grid.arrange(plot1, plot2, plot3, plot4, plot5,
                            ncol = 2)

# Print the grid layout
print(grid_layout)


# Clustering
set.seed(214)
k_max <- 10
selected_season <- 2022

vorp_tiers_final <- data.frame()

for (i in unique(stats_yearly$position)) {
    
    km_vorp <- stats_yearly %>%
        filter(season == selected_season & vorp > 0 & position == i)
    
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
            labs(title = "VORP Tiers",
                 x = "Position Rank",
                 y = "VORP") +
            guides(color = guide_legend(title = NULL)) +
            theme_bw() +
            theme(panel.grid.minor = element_blank())
        
        print(vorp_tiers_viz)
        
        proj_vorp_tiers <- bind_rows(vorp_tiers_final,vorp_tiers)
        
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
            labs(title = "VORP Tiers",
                 x = "Position Rank",
                 y = "VORP") +
            guides(color = guide_legend(title = NULL)) +
            theme_bw() +
            theme(panel.grid.minor = element_blank())
        
        
        print(vorp_tiers_viz)
        
        vorp_tiers_final <- bind_rows(vorp_tiers_final,vorp_tiers)
        
    }
}


rm(list=ls()[! ls() %in% c("stats_yearly","stats_weekly","roster","pbp_fantasy",
                           "plot1","plot2","plot3","plot4","plot5","table1")])



# Points by touches
stats_yearly %>%
    filter(season == 2022 & position == "WR" & touches >= 50) %>%
    select(player_display_name, position, total_points, average_points, touches, points_per_touch) %>%
    arrange(desc(touches)) %>%
    head(10) %>%
    kable()

stats_yearly %>%
    filter(season == 2022 & position == "WR" & touches >= 50) %>%
    select(player_display_name, position, total_points, average_points, touches, points_per_touch) %>%
    arrange(desc(points_per_touch)) %>%
    head(10) %>%
    kable()

stats_yearly %>%
    filter(season == 2022 & position == "RB" & touches >= 150) %>%
    select(player_display_name, position, total_points, average_points, touches, points_per_touch) %>%
    arrange(desc(touches)) %>%
    head(10) %>%
    kable()

stats_yearly %>%
    filter(season == 2022 & position == "RB" & touches >= 150) %>%
    select(player_display_name, position, total_points, average_points, touches, points_per_touch) %>%
    arrange(desc(points_per_touch)) %>%
    head(10) %>%
    kable()

stats_yearly %>%
    filter(season == 2022 & position == "RB" & games >= 6 & average_points >= 12) %>%
    select(player_display_name, recent_team, games, total_points, average_points, std_dev) %>%
    ggplot(aes(std_dev, reorder(player_display_name, std_dev), fill = average_points)) +
    geom_col() +
    scale_x_continuous() +
    scale_fill_gradient(low = "purple", high = "darkgreen") +
    labs(x = "Variance",
         y = "",
         fill = "Average Points",
         title = "Variance in Weekly Point Production",
         caption = "Figure: @MambaMetrics | Data: @nflfastR") +
    theme_bw()

stats_yearly %>%
    filter(season == 2022 & position == "WR" & games >= 6 & average_points >= 12) %>%
    select(player_display_name, recent_team, games, total_points, average_points, std_dev) %>%
    ggplot(aes(std_dev, reorder(player_display_name, std_dev), fill = average_points)) +
    geom_col() +
    scale_x_continuous() +
    scale_fill_gradient(low = "purple", high = "darkgreen") +
    labs(x = "Variance",
         y = "",
         fill = "Average Points",
         title = "Variance in Weekly Point Production",
         caption = "Figure: @MambaMetrics | Data: @nflfastR") +
    theme_bw()


# HVT RB
# https://www.opensourcefootball.com/posts/2020-08-25-open-source-fantasy-football-visualizing-trap-backs/
roster_pos <- roster %>%
    select(gsis_id,position,full_name) %>%
    filter(position %in% c("QB","RB","WR","TE")) %>%
    distinct()

hvt_rb <- pbp_fantasy %>%
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
               TRUE ~ "other")) %>%
    filter(rusher_position == "RB" | receiver_position == "RB") %>%
    mutate(player_name = if_else(is.na(rusher_player_name), receiver_player_name, rusher_player_name),
           player_id = if_else(is.na(rusher_player_id), receiver_player_id, rusher_player_id)) %>%
    group_by(player_name, player_id) %>%
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

hvt_rb %>%
    filter(hvt_type == "hvt_pct") %>%
    ggplot(aes(touch_pct, reorder(player_name, touch_pct), fill = total_touches)) +
    geom_col() +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(x = "Percent of plays",
         fill = "Total Touches",
         title = "Visualization of TRAP backs, displaying RB high value touches (carries inside the 10\nand catches) as a % of total touches (min 100 touches)",
         caption = "Figure: @MambaMetrics | Data: @nflfastR") +
    theme(axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_blank())

hvt_rb %>%
    filter(hvt_type %in% c("hvt_rush","hvt_rec")) %>%
    ggplot(aes(touch_pct, reorder(player_name, touch_pct), fill = hvt_type)) +
    geom_bar(position="stack", stat="identity") +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(x = "Percent of plays",
         fill = "Play Type",
         title = "Visualization of TRAP backs, displaying RB high value touches (carries inside the 10\nand catches) as a % of total touches (min 100 touches)",
         caption = "Figure: @SamHoppen | Data: @nflfastR") +
    theme(axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_blank())


# HVT WR
# https://www.opensourcefootball.com/posts/2020-08-25-open-source-fantasy-football-visualizing-trap-backs/

hvt_wr <- pbp_fantasy %>%
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
               TRUE ~ "other")) %>%
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

hvt_wr %>%
    ggplot(aes(touch_pct, reorder(player_name, touch_pct), fill = adot)) +
    geom_col() +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(x = "Percent of plays",
         fill = "aDot",
         title = "Visualization of TRAP receivers, displaying WR high value touches (carries and targets inside the 10)\nas a % of total touches (min 100 touches)",
         caption = "Figure: @MambaMetrics | Data: @nflfastR") +
    theme(axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_blank())

hvt_wr %>%
    ggplot(aes(hvt_pot, reorder(player_name, hvt_pot), fill = adot)) +
    geom_col() +
    scale_x_continuous() +
    labs(x = "Percent of plays",
         fill = "aDot",
         title = "Visualization of TRAP receivers, displaying WR high value opportunities (carries and targets inside the 10)\nmin 100 touches",
         caption = "Figure: @MambaMetrics | Data: @nflfastR") +
    theme(axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_blank())





# Player viz ----

# Select player
player <- stats_yearly %>%
    filter(player_display_name == "CeeDee Lamb" & season == max(season)) %>%
    select(player_display_name, position, recent_team) %>%
    left_join(nflfastR::teams_colors_logos, by = c("recent_team" = "team_abbr"))

selected_season <- stats_yearly %>%
    select(season) %>%
    max()


# Total and average points by season - add size for games played
stats_yearly %>%
    filter(player_display_name == player$player_display_name) %>%
    ggplot(aes(season)) +
    geom_line(aes(y = total_points, color = player$team_color), linetype = "dashed") +
    geom_line(aes(y = average_points * 10, color = player$team_color2)) +
    geom_point(aes(y = total_points, color = player$team_color)) +
    geom_point(aes(y = average_points * 10,  color = player$team_color2)) +
    scale_x_continuous(breaks = unique(stats_yearly$season), minor_breaks = NULL) +
    scale_y_continuous(breaks = seq(0, max(stats_yearly$total_points), 50),
                       limits = c(0, max(stats_yearly$total_points)),
                       sec.axis = sec_axis(~ . / 10, name = "Average Points")) +
    scale_color_manual(
        name = "",
        values = c(player$team_color, player$team_color2),
        labels = c("Total Points", "Average Points"),
        guide = "legend"
    ) +
    labs(title = "Total and Average Points by Season",
         subtitle = "Only Includes Fantasy Season",
         caption = "Point size is based on number of games played") +
    xlab("Season") +
    ylab("Total Points") +
    theme_bw()

# Week segment table
stats_weekly %>%
    filter(player_display_name == player$player_display_name) %>%
    mutate(week_group = cut(
        week,
        breaks = c(0, 4, 8, 12, 17),
        labels = c("Weeks 1-4", "Weeks 5-8", "Weeks 9-12", "Weeks 13-17"))) %>%
    group_by(season, week_group) %>%
    summarise(group_average = mean(total_points)) %>%
    select(season, week_group, group_average) %>%
    spread(season, group_average) %>%
    gt() %>%
    gt_theme_538() %>%
    tab_options(heading.align = "center") %>%
    tab_header(title = "Average Points by Week Segments") %>%
    cols_label(week_group = "") %>%
    cols_align("center") %>%
    gt_hulk_col_numeric(-week_group, trim = FALSE) %>%
    fmt_number(decimals = 1)

# Weekly rank table
stats_weekly %>%
    filter(season == selected_season) %>%
    group_by(week, position) %>%
    mutate(week_rank = rank(-total_points, ties.method = "first")) %>%
    ungroup() %>%
    filter(player_display_name == player$player_display_name) %>%
    select(week, total_points, week_rank) %>%
    arrange(week) %>%
    gt() %>%
    gt_theme_538() %>%
    tab_options(
        heading.align = "center",
    ) %>%
    tab_header(title = "Points and Rank by Week") %>%
    cols_align("center") %>%
    cols_label(
        week = "Week",
        total_points = "Points",
        week_rank = "Position Rank"
    ) %>%
    fmt_number(
        columns = c(total_points),
        decimals = 1,
    ) %>%
    tab_style(
        style = list(
            cell_fill(color = "white", alpha = 0.7),
            cell_text(weight = "bold")
        ),
        locations = cells_body(columns = c(total_points, week_rank))
    ) %>%
    cols_width(
        columns = everything() ~ px(80)
    )

# Year over year comparison
stats_yearly %>%
    as_tibble() %>%
    filter(
        player_display_name == player$player_display_name & 
            (season == selected_season | season == selected_season - 1)
    ) %>%
    select(player_display_name, season, games, total_points, average_points, std_dev) %>%
    pivot_wider(
        names_from = season,
        values_from = c(games, total_points, average_points, std_dev)
    ) %>%
    select(
        player_display_name,
        games_2021, total_points_2021, average_points_2021, std_dev_2021,
        games_2022, total_points_2022, average_points_2022, std_dev_2022
    ) %>%
    mutate(
        total_points_change = (total_points_2022 - total_points_2021) / total_points_2021 * 100,
        average_points_change = (average_points_2022 - average_points_2021) / average_points_2021 * 100,
        std_dev_change = (std_dev_2022 - std_dev_2021) / std_dev_2021 * 100
    ) %>%
    gt() %>%
    gt_theme_538() %>%
    tab_header(title = "Season Comparison") %>%
    cols_align("center") %>%
    cols_label(
        player_display_name = "",
        games_2021 = "Games",
        games_2022 = "Games",
        total_points_2021 = "Total Points",
        total_points_2022 = "Total Points",
        average_points_2021 = "Avg Points",
        average_points_2022 = "Avg Points",
        std_dev_2021 = "Standard Deviation",
        std_dev_2022 = "Standard Deviation",
        total_points_change = "Total Points % Change",
        average_points_change = "Avg Points % Change",
        std_dev_change = "Standard Deviation %Change"
    ) %>%
    tab_spanner(label = "2021", columns = c(games_2021:std_dev_2021)) %>%
    tab_spanner(label = "2022", columns = c(games_2022:std_dev_2022)) %>%
    tab_spanner(label = "Y/Y Change", columns = c(total_points_change:std_dev_change)) %>%
    fmt_number(
        columns = c(
            total_points_2021,  total_points_2022,
            average_points_2021, average_points_2022,
            std_dev_2021, std_dev_2022,
            total_points_change, average_points_change, std_dev_change
        ),
        decimals = 1
    ) %>%
    tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_body(columns = c(games_2021, games_2022))
    ) %>%
    tab_style(
        style = list(cell_text(color = "black", weight = "bold"),
                     cell_fill(color = player$team_color, alpha = 0.5)),
        locations = cells_body(columns = c(total_points_2021,
                                           average_points_2021,
                                           std_dev_2021))
    ) %>%
    tab_style(
        style = list(cell_text(color = "black", weight = "bold"),
                     cell_fill(color = player$team_color2, alpha = 0.5)),
        locations = cells_body(columns = c(total_points_2022,
                                           average_points_2022,
                                           std_dev_2022))
    ) %>%
    tab_style(
        style = list(cell_text(color = "black", weight = "bold"),
                     cell_fill(color = player$team_color3, alpha = 0.5)),
        locations = cells_body(columns = c(total_points_change,
                                           average_points_change,
                                           std_dev_change))
    ) %>%
    gt_add_divider(
        c(std_dev_2022,std_dev_2021),
        sides = "right",
        color = "grey",
        style = "solid",
        weight = px(2),
        include_labels = TRUE
    )

# Actual VORP
stats_yearly %>%
    filter(position == player$position & season == selected_season &
               (vorp > 0 | player_display_name == player$player_display_name)) %>%
    ggplot(aes(reorder(player_display_name, vorp), vorp)) +
    geom_bar(stat = "identity", fill = "black", alpha = 0.3) +
    geom_bar(data = subset(stats_yearly,
                           player_display_name == player$player_display_name & season == 2022),
             stat = "identity",
             fill = player$team_color) +
    labs(title = "Value Over Replacement Player (VORP)",
         subtitle = "") +
    xlab("") +
    ylab("VORP") +
    coord_flip() +
    theme_bw()


# Stats by Season - QB
stats_yearly %>%
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
stats_yearly %>%
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
stats_yearly %>%
    filter(player_display_name == player$player_display_name) %>%
    select(season, recent_team, games, total_points, average_points, vorp, pos_rank,
           touches, receptions, receiving_yards, receiving_tds, receiving_epa,
           rushing_yards, rushing_tds, rushing_epa) %>%
    arrange(season) %>%
    gt() %>%
    gt_theme_538() %>%
    tab_options(
        heading.align = "center",
    ) %>%
    tab_header(
        title = "All Stats by Season"
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
        touches = "Touches",
        receptions = "Receptions",
        receiving_yards = "Receiving Yards",
        receiving_tds = "Receiving TDs",
        receiving_epa = "Receiving EPA",
        rushing_yards = "Rush Yards",
        rushing_tds = "Rush TDs",
        rushing_epa = "Rush EPA"
    ) %>%
    fmt_number(
        columns = c(total_points, average_points, vorp, receiving_epa, rushing_epa),
        decimals = 1
    )


# Rolling average table
all_combinations <- expand.grid(player_display_name = unique(stats_weekly$player_display_name),
                                position = player$position,
                                week = 1:17)
stats_weekly %>%
    filter(position == player$position & season == selected_season) %>%
    arrange(week) %>%
    group_by(player_display_name) %>%
    mutate(average_points = round(cummean(total_points),2)) %>%
    mutate(run_total_points = round(cumsum(total_points),2)) %>%
    ungroup() %>%
    complete(all_combinations) %>%
    group_by(player_display_name) %>%
    fill(run_total_points, .direction = "down") %>%
    replace_na(list(total_points = 0)) %>%
    fill(total_points, average_points) %>%
    mutate(pos_rank = round(rank(-run_total_points, ties.method = "first"))) %>%
    ungroup() %>%
    group_by(week) %>%
    mutate(pos_rank = round(rank(-run_total_points, ties.method = "first"))) %>%
    ungroup() %>%
    filter(player_display_name == player$player_display_name) %>%
    select(week, total_points, average_points, pos_rank) %>%
    gt() %>%
    gt_theme_538() %>%
    tab_options(
        heading.align = "center",
    ) %>%
    tab_header(
        title = "Weekly Fantasy Performance",
        subtitle = "Rolling Position Rank by Total Points"
    ) %>%
    cols_align(
        "center"
    ) %>%
    cols_label(
        week = "Week",
        total_points = "Weekly Points",
        average_points = "Rolling Average Points",
        pos_rank = "Position Rank",
    ) %>%
    fmt_number(
        columns = c(total_points, average_points),
        decimals = 1
    ) %>%
    cols_width(
        columns = everything() ~ px(80)
    )

# Rolling average line graph
stats_weekly %>%
    filter(position == player$position & season == selected_season) %>%
    arrange(week) %>%
    group_by(player_display_name) %>%
    mutate(average_points = round(cummean(total_points),2)) %>%
    ungroup() %>%
    group_by(week) %>%
    mutate(pos_rank = round(rank(-average_points, ties.method = "first"))) %>%
    ungroup() %>%
    complete(all_combinations) %>%
    group_by(player_display_name) %>%
    fill(total_points, average_points) %>%
    mutate(pos_rank = round(rank(-average_points, ties.method = "first"))) %>%
    ungroup() %>%
    filter(player_display_name == player$player_display_name) %>%
    select(player_display_name, position, week,
           total_points, average_points, pos_rank) %>%
    ggplot(aes(week)) +
    geom_line(aes(y = total_points, color = player$team_color), linetype = "dashed") +
    geom_line(aes(y = average_points, color = player$team_color2), linetype = "solid") +
    geom_point(aes(y = total_points, color = player$team_color)) +
    scale_color_manual(
        name = "",
        values = c(player$team_color, player$team_color2),
        labels = c("Week Points", "Average Points"),
        guide = guide_legend(override.aes = list(linetype = c("dashed", "solid"),
                                                 color = c(player$team_color, player$team_color2)))) +
    labs(title = "Week and Average Points by Week",
         subtitle = "Week points are the points scored in a particular week") +
    ylab("Week Points") +
    xlab("Week") +
    scale_y_continuous(
        breaks = seq(0, max(stats_weekly$total_points), 5),
        sec.axis = sec_axis(~., breaks = seq(0, max(stats_weekly$total_points), 5),
                            name = "Average Points", labels = function(x) round(x, 2))) +
    scale_x_continuous(breaks = seq(min(stats_weekly$week), max(stats_weekly$week), 1)) +
    theme_bw()


# Total and average points rank table
stats_yearly %>%
    group_by(season, position) %>%
    mutate(avg_pts_rank = round(rank(-average_points, ties.method = "first")),
           tot_pts_rank = round(rank(-total_points, ties.method = "first")),
           adj_pts_rank = round(rank(-average_points_adj, ties.method = "first"))) %>%
    ungroup() %>%
    filter(player_display_name == player$player_display_name) %>%
    select(season, games,
           total_points, tot_pts_rank, average_points, avg_pts_rank,
           average_points_adj, adj_pts_rank) %>%
    arrange(season) %>%
    gt() %>%
    gt_theme_538() %>%
    tab_options(
        heading.align = "center",
    ) %>%
    tab_header(
        title = "Fantasy Performance by Season",
        subtitle = "Points and Position Rank"
    ) %>%
    cols_align(
        "center"
    ) %>%
    cols_label(
        season = "Season",
        games = "Games",
        total_points = "Total Points",
        tot_pts_rank = "Total Points Rank",
        average_points = "Average Points",
        avg_pts_rank = "Average Points Rank",
        average_points_adj = "Adjusted Average Points",
        adj_pts_rank = "Adjusted Points Rank"
    ) %>%
    fmt_number(
        columns = c(total_points, average_points, average_points_adj),
        decimals = 1
    )  %>%
    cols_width(
        columns = everything() ~ px(80)
    )



stats_yearly %>%
    filter(player_display_name == player$player_display_name) %>%
    mutate(adjustment_tot = total_points - total_points_adj,
           adjustment_avg = average_points_adj - average_points) %>%
    select(season, games,
           total_points, total_points_adj,
           average_points, average_points_adj) %>%
    gt() %>%
    gt_theme_538() %>%
    tab_options(
        heading.align = "center",
    ) %>%
    tab_header(
        title = "Fantasy Performance by Season",
        subtitle = "Adjustments assuming replacement player for missed games"
    ) %>%
    cols_align(
        "center"
    ) %>%
    cols_label(
        season = "Season",
        games = "Games",
        total_points = "Total Points",
        total_points_adj = "Total Points Adjusted",
        average_points = "Average Points",
        average_points_adj = "Average Points Adjusted",
        # adjustment_avg = "+/-"
    ) %>%
    fmt_number(
        columns = c(total_points, total_points_adj,
                    average_points, average_points_adj),
        decimals = 1
    )  %>%
    cols_width(
        columns = everything() ~ px(80)
    )

    




# ADP analysis ----
adp_season <- stats_yearly %>%
    select(season) %>%
    max()

adp <- dplyr::tbl(DBI::dbConnect(RSQLite::SQLite(), "../nfl_sql_db/nfl_pbp_db"), "adp") %>% 
    collect() %>%
    mutate(name = case_when(
        name == "LeVeon Bell" ~ "Le'Veon Bell",
        name == "D.K. Metcalf" ~ "DK Metcalf",
        TRUE ~ name
    ))

stats_adp <- stats_yearly %>%
    select(season, player_display_name, position, recent_team, games,
           total_points, average_points, pos_rank, vorp, average_points_adj) %>%
    left_join(adp %>% select(name, season, overall), 
              by = c("player_display_name" = "name", "season" = "season")) %>%
    mutate(adp = coalesce(overall, 192)) %>%
    select(-overall)



# Plots 
stats_adp %>%
    filter(games >= 6 & position == "RB" & adp < 192 & season == adp_season) %>%
    ggplot(aes(adp, vorp)) +
    geom_smooth(method = "lm", se = FALSE, color = "skyblue") +
    geom_text_repel(aes(label = player_display_name), show.legend = F) +
    geom_point() +
    scale_x_continuous(breaks = seq(0, 200, 20)) +
    labs(title = "Fantasy Football: ADP vs. VORP",
         x = "Average Draft Position (ADP)",
         y = "Value Over Replacement Player (VORP)")

stats_adp %>%
    filter(games >= 6 & position == "RB" & season == adp_season & 
               (adp < 192 | (adp == 192 & vorp > 0))) %>%
    ggplot(aes(adp, vorp, size = total_points)) +
    geom_point(alpha = 0.7, shape = 21, fill = "skyblue") +
    geom_text_repel(aes(label = player_display_name), hjust = 0, vjust = 0, size = 3) +
    labs(title = "Fantasy Football: ADP vs. VORP",
         x = "Average Draft Position (ADP)",
         y = "Value Over Replacement Player (VORP)",
         size = "Fantasy Points") +
    scale_size_continuous(range = c(3, 15))



stats_adp %>%
    filter(games >= 6 & position == "WR" & adp < 192 & season == adp_season) %>%
    ggplot(aes(adp, average_points)) +
    geom_smooth(method = "lm", se = FALSE, color = "skyblue") +
    geom_text_repel(aes(label = player_display_name), show.legend = F) +
    geom_point() +
    scale_x_continuous(breaks = seq(0, 200, 20)) +
    labs(title = "Fantasy Football: ADP vs. PPG",
         x = "Average Draft Position (ADP)",
         y = "Points per Game (PPG)")

stats_adp %>%
    filter(games >= 6 & position == "WR" & season == adp_season &
               (adp < 192 | (adp == 192 & vorp > 0))) %>%
    ggplot(aes(adp, vorp, size = total_points)) +
    geom_point(alpha = 0.7, shape = 21, fill = "skyblue") +
    geom_text_repel(aes(label = player_display_name), hjust = 0, vjust = 0, size = 3) +
    labs(title = "Fantasy Football: ADP vs. VORP",
         x = "Average Draft Position (ADP)",
         y = "Value Over Replacement Player (VORP)",
         size = "Fantasy Points") +
    scale_size_continuous(range = c(3, 15))



stats_adp %>%
    filter(games >= 6 & position == "TE" & adp < 192 & season == adp_season) %>%
    ggplot(aes(adp, average_points_adj)) +
    geom_smooth(method = "lm", se = FALSE, color = "skyblue") +
    geom_text_repel(aes(label = player_display_name), show.legend = F) +
    geom_point() +
    scale_x_continuous(breaks = seq(0, 200, 20)) +
    labs(title = "Fantasy Football: ADP vs. Adjusted PPG",
         x = "Average Draft Position (ADP)",
         y = "Adjusted Points per Game (PPG)")

stats_adp %>%
    filter(games >= 6 & position == "TE" & season == adp_season &
               (adp < 192 | (adp == 192 & vorp > 0))) %>%
    ggplot(aes(adp, vorp, size = total_points)) +
    geom_point(alpha = 0.7, shape = 21, fill = "skyblue") +
    geom_text_repel(aes(label = player_display_name), hjust = 0, vjust = 0, size = 3) +
    labs(title = "Fantasy Football: ADP vs. VORP",
         x = "Average Draft Position (ADP)",
         y = "Value Over Replacement Player (VORP)",
         size = "Fantasy Points") +
    scale_size_continuous(range = c(3, 15))



stats_adp %>%
    filter(games >= 6 & position == "QB" & adp < 192 & season == adp_season) %>%
    ggplot(aes(adp, average_points_adj)) +
    geom_smooth(method = "lm", se = FALSE, color = "skyblue") +
    geom_text_repel(aes(label = player_display_name), show.legend = F) +
    geom_point() +
    scale_x_continuous(breaks = seq(0, 200, 20)) +
    labs(title = "Fantasy Football: ADP vs. Adjusted PPG",
         x = "Average Draft Position (ADP)",
         y = "Adjusted Points per Game (PPG)")

stats_adp %>%
    filter(games >= 6 & position == "QB" & season == adp_season &
               (adp < 192 | (adp == 192 & vorp > 0))) %>%
    ggplot(aes(adp, vorp, size = total_points)) +
    geom_point(alpha = 0.7, shape = 21, fill = "skyblue") +
    geom_text_repel(aes(label = player_display_name), hjust = 0, vjust = 0, size = 3) +
    labs(title = "Fantasy Football: ADP vs. VORP",
         x = "Average Draft Position (ADP)",
         y = "Value Over Replacement Player (VORP)",
         size = "Fantasy Points") +
    scale_size_continuous(range = c(3, 15))


stats_adp %>%
    filter(games >= 6 & adp < 192 & season == adp_season) %>%
    group_by(position) %>%
    summarize(average_vorp = mean(vorp)) %>%
    ggplot(aes(position, average_vorp)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    labs(title = "Average Value Over Replacement Player (VORP) by Position",
         x = "Position",
         y = "Average VORP")

stats_adp %>%
    filter(games >= 6 & adp < 192 & season == adp_season) %>%
    ggplot(aes(position, y = vorp)) +
    geom_boxplot(fill = "skyblue", color = "black", outlier.shape = NA) +
    labs(title = "Value Over Replacement Player (VORP) Distribution by Position",
         x = "Position",
         y = "Value Over Replacement (VORP)")



