#### Historical Fantasy Football ####

library(tidyverse)
library(nflfastR)
library(ggrepel)
library(gt)
library(gtExtras)
library(ggthemes)
library(gganimate)
library(ggimage)


pbp <- nflfastR::load_pbp(c(2013:2022))
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
int_adj <- 0
fum_adj <- 0

# yearly fantasy points
stats_yearly <- stats_yr %>%
    filter(position %in% c("QB","RB","WR","TE")) %>%
    mutate(games_adj = round((if_else(season <= 2020, (games/15),
                                      (games/16))*16),0),
           total_points_mfl =
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
           average_points_mfl = total_points_mfl/games_adj,
           position = factor(position, levels = c("QB","RB","WR","TE"))
    ) %>%
    select(season,player_display_name,position,recent_team,games,games_adj,
           completions:passing_epa,pacr:rushing_epa,receptions:receiving_epa,racr:wopr,
           total_points_mfl,average_points_mfl) %>%
    arrange(desc(total_points_mfl))


# weekly fantasy points
stats_weekly <- stats_wk %>%
    filter(position %in% c("QB","RB","WR","TE")) %>%
    mutate(total_points_mfl =
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
           total_points_mfl) %>%
    arrange(desc(total_points_mfl))


stats_weekly_agg <- stats_weekly %>%
    group_by(season,player_display_name,position) %>%
    summarise(games_played = n(),
              total_points = sum(total_points_mfl),
              average_points = mean(total_points_mfl),
              sd_dev = sd(total_points_mfl)
    ) %>%
    arrange(desc(total_points)) %>%
    left_join(stats_yearly[,c(1:4)], by = c("player_display_name" = "player_display_name",
                                            "season" = "season", "position" = "position"))

# stats_weekly_agg %>%
#     filter(season >= 2018) %>%
#     group_by(season,position) %>%
#     slice(1:12) %>%
#     ggplot(aes(position,average_points)) +
#     geom_boxplot() +
#     geom_point() +
#     facet_wrap(~season)
# 
# stats_weekly_agg %>%
#     filter(season >= 2018) %>%
#     group_by(season,position) %>%
#     slice(1:36) %>%
#     ggplot(aes(position,average_points)) +
#     geom_boxplot() +
#     facet_wrap(~season)

vorp_yearly <- stats_yearly %>%
    filter(season %in% c(2018:2022))

unique_vorp_seasons <- unique(vorp_yearly$season)
stats_vorp_final <- data.frame()

for (i in unique_vorp_seasons) {
    
    stats_vorp_yearly <- vorp_yearly %>%
        filter(season == i)
    
    # Filtering top 12 each position and  next top 12 players
    stats_vorp_filtered <- stats_vorp_yearly %>%
        group_by(position) %>%
        slice(19:n()) %>%
        ungroup() %>%
        arrange(desc(total_points_mfl)) %>%
        slice(13:n())
    
    # Further filtering and selecting top players for QB and flex positions
    stats_vorp_filtered_qb <- stats_vorp_filtered %>%
        filter(position == "QB")
    
    stats_vorp_filtered_flex <- stats_vorp_filtered %>%
        filter(position != "QB") %>%
        slice(25:n())
    
    # Final selection of top players by position
    stats_vorp_replacement <- bind_rows(stats_vorp_filtered_qb,stats_vorp_filtered_flex) %>%
        arrange(desc(total_points_mfl)) %>%
        group_by(position) %>%
        slice(1) %>%
        arrange(position)
    
    # Extracting replacement values for each position
    replacement_values <- data.frame(
        position = stats_vorp_replacement$position,
        replacement_points = stats_vorp_replacement$total_points_mfl
    )
    
    # Calculating value over replacement player (VORP)
    stats_vorp <- stats_vorp_yearly %>%
        mutate(
            vorp = total_points_mfl - replacement_values$replacement_points[match(position,
                                                          replacement_values$position)])
    
    # Calculating VORP total and multiplier
    value_multiplier <- ((300-21)*12) / sum(stats_vorp %>% filter(vorp >= 0) %>% pull(vorp))
    
    # Calculating value based on VORP and creating new columns
    stats_vorp_value <- stats_vorp %>%
        mutate(value = round(vorp*value_multiplier, 0)) %>%
        group_by(position) %>%
        mutate(pos_rank = round(rank(-total_points_mfl, ties.method = "first"))) %>%
        ungroup() %>%
        mutate(vorp = round(vorp, 1))
    
    stats_vorp_final <- bind_rows(stats_vorp_final, stats_vorp_value)
    
}

# Creating a plot of VORP versus position rank
stats_vorp_final %>%
    filter(vorp >= 0) %>%
    ggplot(aes(pos_rank, vorp, color = position)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = seq(1, 30, 1), limits = c(1, 30)) +
    scale_y_continuous(breaks = seq(0, 225, 25), limits = c(00, 225)) +
    scale_color_discrete(breaks = c("QB", "RB", "WR", "TE")) +
    scale_fill_discrete(breaks = c("QB", "RB", "WR", "TE")) +
    theme_bw() +
    theme(panel.grid.minor = element_blank())

# Yearly by value
stats_vorp_final %>%
    filter(vorp >= 0 & season == 2022) %>%
    ggplot(aes(pos_rank, value, color = position)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = seq(1, 40, 1), limits = c(1, 40)) +
    scale_y_continuous(breaks = seq(0, 125, 25), limits = c(0, 125)) +
    # scale_color_discrete(breaks = c("QB", "RB", "WR", "TE")) +
    # scale_fill_discrete(breaks = c("QB", "RB", "WR", "TE")) +
    theme_bw() +
    theme(panel.grid.minor = element_blank())


stats_vorp_final %>%
    group_by(position, pos_rank) %>%
    summarise(avg_vorp = mean(vorp)) %>%
    filter(avg_vorp >= 0) %>%
    ggplot(aes(pos_rank,avg_vorp, color = position)) +
    geom_line() +
    geom_point()

stats_vorp_final %>%
    filter(vorp >= 0) %>%
    ggplot(aes(pos_rank, vorp, color = position)) +
    geom_line() +
    geom_point() +
    facet_wrap(~season)

stats_vorp_final %>%
    group_by(position,pos_rank) %>%
    summarise(avg_value = mean(value),
              avg_vorp = mean(vorp)) %>%
    filter(avg_vorp >= 0) %>%
    ggplot(aes(pos_rank,avg_value, color = position)) +
    geom_line() +
    geom_point()

stats_vorp_final %>%
    filter(vorp >= 0) %>%
    ggplot(aes(pos_rank, value, color = position)) +
    geom_line() +
    geom_point() +
    facet_wrap(~season)




# Clustering
set.seed(214)
k_max <- 10
unique_pos <- unique(stats_vorp_final$position)

proj_vorp_tiers <- data.frame()

for (i in unique_pos) {
    
    km_vorp <- stats_vorp_final %>%
        filter(season == 2022 & vorp >= 0 & position == i)
    
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
            scale_x_continuous("Position Rank", breaks = seq(1, 30, 1)) +
            scale_y_continuous("Value") +
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
            ggplot(aes(pos_rank, value, color = tier)) +
            geom_point() +
            geom_text_repel(aes(label = player_display_name), show.legend = F) +
            scale_x_continuous("Position Rank", breaks = seq(1, 30, 1)) +
            scale_y_continuous("Value") +
            theme_bw() +
            theme(panel.grid.minor = element_blank())
        
        print(vorp_tiers_viz)
        
        proj_vorp_tiers <- bind_rows(proj_vorp_tiers,vorp_tiers)
        
    }
}


nflfastR::teams_colors_logos

player <- stats_yearly %>%
    filter(player_display_name == "Dak Prescott" & season == max(season)) %>%
    select(player_display_name, position, recent_team) %>%
    left_join(nflfastR::teams_colors_logos, by = c("recent_team" = "team_abbr"))


# Total and Average Points by Season
stats_weekly_agg %>%
    filter(player_display_name == player$player_display_name) %>%
    ggplot(aes(season)) +
    geom_line(aes(y = total_points, color = player$team_color)) +
    geom_line(aes(y = average_points * 10, color = player$team_color2)) +
    geom_point(aes(y = total_points, size = games_played, color = player$team_color), show.legend = F) +
    geom_point(aes(y = average_points * 10, size = games_played, color = player$team_color2), show.legend = F) +
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
    labs(title = "Total & Average Points by Season", subtitle = "Excludes Final Week of Each Season") + 
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
    summarise(group_average = mean(total_points_mfl)) %>%
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
    mutate(week_rank = rank(-total_points_mfl)) %>%
    ungroup() %>%
    filter(player_display_name == "Dak Prescott") %>%
    select(week, total_points_mfl, week_rank) %>%
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
        total_points_mfl = "Points",
        week_rank = "Position Rank"
    ) %>%
    fmt_number(
        columns = total_points_mfl,
        decimals = 1
    )


# 2021 to 2022 Comparison
stats_weekly_agg %>%
    as_tibble() %>%
    filter(player_display_name == player$player_display_name & 
               (season == max(season) | season == max(season)-1)) %>%
    pivot_wider(names_from = season,
                values_from = c(games_played, total_points, average_points, sd_dev)) %>%
    select(player_display_name,
           games_played_2021, total_points_2021, average_points_2021, sd_dev_2021,
           games_played_2022, total_points_2022, average_points_2022, sd_dev_2022) %>%
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
        sd_dev_2021 = "Standard Deviation",
        sd_dev_2022 = "Standard Deviation"
    ) %>%
    tab_spanner(
        label = "2021",
        columns = c(games_played_2021:sd_dev_2021)
    ) %>%
    tab_spanner(
        label = "2022",
        columns = c(games_played_2022:sd_dev_2022)
    ) %>%
    fmt_number(
        columns = c(total_points_2021,total_points_2022,
                    average_points_2021,average_points_2022,
                    sd_dev_2021,sd_dev_2022),
        decimals = 1
    )

# Actual VORP
stats_vorp_final %>%
    filter(position == player$position & season == 2022 & vorp >= 0) %>%
    ggplot(aes(reorder(player_display_name, vorp), vorp)) +
    geom_bar(stat = "identity", fill = "black", alpha = 0.3) +
    geom_bar(data = subset(stats_vorp_final,
                           player_display_name == player$player_display_name & season == 2022),
             stat = "identity",
             fill = player$team_color) +
    geom_text(data = subset(stats_vorp_final,
                            player_display_name == player$player_display_name & season == 2022),
              aes(label = vorp, y = vorp),
              color = player$team_color,
              fontface = "bold",
              size = 4,
              nudge_y = 8) +
    scale_x_discrete(breaks = player$player_display_name) +
    labs(title = "Value Over Replacement Player") +
    xlab("") +
    ylab("VORP") + 
    coord_flip() +
    theme_bw()

# stats_vorp_final %>%
#     filter(position == player$position & season == max(season) & vorp >= 0) %>%
#     ggplot(aes(pos_rank, vorp)) +
#     geom_point(alpha = 0.3, size = 2) +
#     geom_point(data = subset(stats_vorp_final,
#                              player_display_name == player$player_display_name & season == 2022),
#                color = player$team_color,
#                size = 4) +
#     geom_text(
#         data = subset(stats_vorp_final,
#                       player_display_name == player$player_display_name & season == 2022),
#         aes(label = player_display_name),
#         color = player$team_color,
#         fontface = "bold",
#         size = 4,
#         nudge_x = 2) +
#     scale_x_continuous(breaks = unique(stats_vorp_final$pos_rank), minor_breaks = NULL) +
#     scale_y_continuous(breaks = seq(0, max(stats_vorp_final$vorp), 25),
#                        limits = c(0, max(stats_vorp_final$vorp))) +
#     labs(title = "Value Over Replacement Player") +
#     xlab("Position Rank") +
#     ylab("VORP") + 
#     theme_bw()

# Stats by Season
stats_vorp_final %>%
    filter(player_display_name == player$player_display_name) %>%
    select(season,recent_team,games,total_points_mfl,average_points_mfl,vorp,pos_rank,
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
        total_points_mfl = "Total Points",
        average_points_mfl = "Average Points",
        vorp = "VORP",
        pos_rank = "Position Rank",
        passing_yards = "Pass Yards",
        passing_tds = "Pass TDs",
        passing_epa = "Pass EPA",
        rushing_yards = "Rush Yards",
        rushing_tds = "Rush TDs"
    ) %>%
    fmt_number(
        columns = c(total_points_mfl,average_points_mfl,vorp,passing_epa),
        decimals = 1
    )

# Cumulative Average
average_pts <- stats_weekly %>%
    filter(position == player$position & season == max(season)) %>%
    arrange(week) %>%
    group_by(player_display_name) %>%
    mutate(avg_pts = cummean(total_points_mfl)) %>%
    ungroup() %>%
    group_by(week) %>%
    mutate(pos_rank = round(rank(-avg_pts, ties.method = "first"))) %>%
    ungroup()





