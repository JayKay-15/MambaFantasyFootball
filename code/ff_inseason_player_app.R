# Fantasy Football In Season Player App
library(tidyverse)
library(knitr)
library(nflfastR)
library(ggrepel)
library(ggthemes)
# library(gganimate)
# library(ggimage)
# library(gridExtra)
# library(patchwork)
library(gt)
library(gtExtras)
library(DT) 


# Fantasy App Function ----
ff_stats_app <- function(seasons = c(2018:2023), scoring = "ppr", league = "flex10") {
    
    pbp_fantasy <- nflfastR::load_pbp(seasons) %>%
        mutate(fantasy_season = if_else((season<=2020 & week<=16) |
                                            (season>2020 & week<=17), TRUE, FALSE)) %>%
        filter(fantasy_season == TRUE)

    db <- DBI::dbConnect(RSQLite::SQLite(), "../nfl_sql_db/nfl_pbp_db")

    # pbp_fantasy <- RSQLite::dbGetQuery(db,
    #                                    'SELECT * FROM nflfastR_pbp WHERE season >= 2018') %>%
    #     mutate(fantasy_season = if_else((season<=2020 & week<=16) |
    #                                         (season>2020 & week<=17), TRUE, FALSE)) %>%
    #     filter(fantasy_season == TRUE)
    
    roster_pos <- nflfastR::fast_scraper_roster(seasons) %>%
        filter(position %in% c("QB","RB","WR","TE")) %>%
        select(season, gsis_id, position, full_name) %>%
        distinct()
    
    adp <- dplyr::tbl(DBI::dbConnect(RSQLite::SQLite(), "../nfl_sql_db/nfl_pbp_db"), "adp") %>% 
        collect() %>%
        mutate(
            name = case_when(
                name == "LeVeon Bell" ~ "Le'Veon Bell",
                name == "D.K. Metcalf" ~ "DK Metcalf",
                TRUE ~ name
            )
        ) %>%
        arrange(overall)
    
    RSQLite::dbDisconnect(db)
    
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
    
    
    if (scoring == "ppr") {
        
        pass_yds_adj <- 0.04
        pass_tds_adj <- 4
        rush_yds_adj <- 0.1
        rush_tds_adj <- 6
        rec_yds_adj <- 0.1
        rec_tds_adj <- 6
        rec_adj <- 1
        int_adj <- -1
        fum_adj <- -1
        
    } else if (scoring == "half") {
        
        pass_yds_adj <- 0.04
        pass_tds_adj <- 4
        rush_yds_adj <- 0.1
        rush_tds_adj <- 6
        rec_yds_adj <- 0.1
        rec_tds_adj <- 6
        rec_adj <- 0.5
        int_adj <- -1
        fum_adj <- -1
        
    } else if (scoring == "standard") {
        
        pass_yds_adj <- 0.04
        pass_tds_adj <- 4
        rush_yds_adj <- 0.1
        rush_tds_adj <- 6
        rec_yds_adj <- 0.1
        rec_tds_adj <- 6
        rec_adj <- 0
        int_adj <- -1
        fum_adj <- -1
        
    } else if (scoring == "mfl") {
        
        pass_yds_adj <- 0.04
        pass_tds_adj <- 4
        rush_yds_adj <- 0.1
        rush_tds_adj <- 6
        rec_yds_adj <- 0.1
        rec_tds_adj <- 6
        rec_adj <- 1
        int_adj <- 0
        fum_adj <- 0 
        
    } else {
        
        cat("error: no selection made")
    }
    
    
    # weekly fantasy points
    stats_weekly <<- stats_wk %>%
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
                  std_dev = round(sd(total_points),1),
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
                   round(case_when(
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
                                           + receiving_fumbles_lost*fum_adj)),1),
               average_points = round(total_points/games,1),
               touches = carries + receptions,
               pot_touches = carries + targets,
               points_per_touch = total_points/touches,
               position = factor(position, levels = c("QB","RB","WR","TE"))) %>%
        left_join(stats_weekly_agg %>% select(season,player_display_name,position,std_dev),
                  by = c("player_display_name" = "player_display_name",
                         "season" = "season", "position" = "position"))
    
    # VORP calculations
    vorp_yearly <- stats_yearly
    
    stats_vorp_final <- data.frame()
    
    if (league == "flex9") {
        
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
                arrange(desc(total_points)) %>%
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
            value_multiplier <- ((200-15)*12) / sum(stats_vorp %>% filter(vorp >= 0) %>% pull(vorp))
            
            # Calculating value based on VORP and creating new columns
            stats_vorp_value <- stats_vorp %>%
                mutate(value = round(vorp*value_multiplier, 0),
                       vorp = round(vorp, 1))
            
            stats_vorp_final <- bind_rows(stats_vorp_final, stats_vorp_value)
            
        }
        
    } else if (league == "flex10") {
        
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
            
            stats_vorp_filtered_rb <- stats_vorp_filtered_rb_wr %>%
                filter(position == "RB")
            
            stats_vorp_filtered_wr <- stats_vorp_filtered_rb_wr %>%
                filter(position == "WR") %>%
                group_by(position) %>%
                slice(13:n()) %>%
                ungroup() %>%
                arrange(desc(total_points))
            
            stats_vorp_filtered_flex <- stats_vorp_filtered_rb %>%
                bind_rows(stats_vorp_filtered_wr, stats_vorp_filtered_te) %>%
                arrange(desc(total_points)) %>%
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
            value_multiplier <- ((200-15)*12) / sum(stats_vorp %>% filter(vorp >= 0) %>% pull(vorp))
            
            # Calculating value based on VORP and creating new columns
            stats_vorp_value <- stats_vorp %>%
                mutate(value = round(vorp*value_multiplier, 0),
                       vorp = round(vorp, 1))
            
            stats_vorp_final <- bind_rows(stats_vorp_final, stats_vorp_value)
            
        }
        
    } else if (league == "mfl") {
        
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
            
            stats_vorp_filtered_flex <- stats_vorp_filtered %>%
                filter(position == "RB" | position == "WR" | position == "TE") %>%
                slice(37:n()) %>%
                arrange(desc(total_points))
            
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
            value_multiplier <- ((200-21)*12) / sum(stats_vorp %>% filter(vorp >= 0) %>% pull(vorp))
            
            # Calculating value based on VORP and creating new columns
            stats_vorp_value <- stats_vorp %>%
                mutate(value = round(vorp*value_multiplier, 0),
                       vorp = round(vorp, 1))
            
            stats_vorp_final <- bind_rows(stats_vorp_final, stats_vorp_value)
            
        }
        
    } else if (league == "superflex mfl") {
        
        for (i in unique(vorp_yearly$season)) {
            
            stats_vorp_yearly <- vorp_yearly %>%
                filter(season == i) %>%
                arrange(desc(total_points))
            
            stats_vorp_filtered <- stats_vorp_yearly %>%
                group_by(position) %>%
                slice(13:n()) %>%
                ungroup() %>%
                arrange(desc(total_points)) %>%
                slice(13:n())
            
            stats_vorp_filtered_qb <- stats_vorp_filtered %>%
                filter(position == "QB")
            
            stats_vorp_filtered_flex <- stats_vorp_filtered %>%
                filter(position != "QB") %>%
                slice(25:n())
            
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
            value_multiplier <- ((200-21)*12) / sum(stats_vorp %>% filter(vorp >= 0) %>% pull(vorp))
            
            # Calculating value based on VORP and creating new columns
            stats_vorp_value <- stats_vorp %>%
                mutate(value = round(vorp*value_multiplier, 0),
                       vorp = round(vorp, 1))
            
            stats_vorp_final <- bind_rows(stats_vorp_final, stats_vorp_value)
            
        }
        
    } else if (league == "superflex flex10") {
        
        for (i in unique(vorp_yearly$season)) {
            
            stats_vorp_yearly <- vorp_yearly %>%
                filter(season == i) %>%
                arrange(desc(total_points))
            
            stats_vorp_filtered <- stats_vorp_yearly %>%
                group_by(position) %>%
                slice(13:n()) %>%
                ungroup() %>%
                arrange(desc(total_points)) %>%
                slice(13:n())
            
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
            
            stats_vorp_filtered_rb <- stats_vorp_filtered_rb_wr %>%
                filter(position == "RB")
            
            stats_vorp_filtered_wr <- stats_vorp_filtered_rb_wr %>%
                filter(position == "WR") %>%
                group_by(position) %>%
                slice(13:n()) %>%
                ungroup() %>%
                arrange(desc(total_points))
            
            stats_vorp_filtered_flex <- stats_vorp_filtered_rb %>%
                bind_rows(stats_vorp_filtered_wr, stats_vorp_filtered_te) %>%
                arrange(desc(total_points)) %>%
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
            value_multiplier <- ((200-16)*12) / sum(stats_vorp %>% filter(vorp >= 0) %>% pull(vorp))
            
            # Calculating value based on VORP and creating new columns
            stats_vorp_value <- stats_vorp %>%
                mutate(value = round(vorp*value_multiplier, 0),
                       vorp = round(vorp, 1))
            
            stats_vorp_final <- bind_rows(stats_vorp_final, stats_vorp_value)
            
        }
        
    } else {
        
        cat("Selection not recognized.")
        
    }
    
    stats_yearly <- stats_vorp_final
    
    # Adj calculations
    adj_yearly <- stats_yearly
    
    stats_adj_final <- data.frame()
    
    if (league == "flex9") {
        
        for (i in unique(adj_yearly$season)) {
            
            stats_adj_yearly <- adj_yearly %>%
                filter(season == i) %>%
                arrange(desc(average_points))
            
            stats_adj_filtered <- stats_adj_yearly %>%
                group_by(position) %>%
                slice(13:n()) %>%
                ungroup() %>%
                arrange(desc(average_points))
            
            stats_adj_filtered_qb <- stats_adj_filtered %>%
                filter(position == "QB")
            
            stats_adj_filtered_te <- stats_adj_filtered %>%
                filter(position == "TE")
            
            stats_adj_filtered_rb_wr <- stats_adj_filtered %>%
                filter(position == "RB" | position == "WR") %>%
                group_by(position) %>%
                slice(13:n()) %>%
                ungroup() %>%
                arrange(desc(average_points))
            
            stats_adj_filtered_flex <- stats_adj_filtered_rb_wr %>%
                bind_rows(stats_adj_filtered_te) %>%
                arrange(desc(average_points)) %>%
                slice(13:n())
            
            # Final selection of top players by position
            stats_adj_replacement <- bind_rows(stats_adj_filtered_qb,stats_adj_filtered_flex) %>%
                arrange(desc(average_points)) %>%
                group_by(position) %>%
                slice(1) %>%
                arrange(position)
            
            # Extracting replacement values for each position
            replacement_values <- data.frame(
                position = stats_adj_replacement$position,
                replacement_points = stats_adj_replacement$average_points
            )
            
            # Calculating adj
            stats_adj <- stats_adj_yearly %>%
                mutate(adj_adj = average_points - replacement_values$replacement_points[match(position,
                                                                                              replacement_values$position)],
                       missed_games = if_else(season <= 2020, 15-games, 16-games),
                       total_points_adj = total_points + (missed_games * replacement_values$replacement_points[match(position,
                                                                                                                     replacement_values$position)]),
                       average_points_adj = total_points_adj/(if_else(season <= 2020, 15, 16))) %>%
                group_by(position) %>%
                mutate(tot_pos_rank = round(rank(-total_points, ties.method = "first")),
                       avg_pos_rank = round(rank(-average_points, ties.method = "first")),
                       adj_pos_rank = round(rank(-average_points_adj, ties.method = "first"))) %>%
                ungroup() %>%
                select(player_id, season,
                       total_points_adj, average_points_adj,
                       tot_pos_rank, avg_pos_rank, adj_pos_rank)
            
            stats_adj_final <- bind_rows(stats_adj_final, stats_adj)
            
        }
        
    } else if (league == "flex10") {
        
        for (i in unique(adj_yearly$season)) {
            
            stats_adj_yearly <- adj_yearly %>%
                filter(season == i) %>%
                arrange(desc(average_points))
            
            stats_adj_filtered <- stats_adj_yearly %>%
                group_by(position) %>%
                slice(13:n()) %>%
                ungroup() %>%
                arrange(desc(average_points))
            
            stats_adj_filtered_qb <- stats_adj_filtered %>%
                filter(position == "QB")
            
            stats_adj_filtered_te <- stats_adj_filtered %>%
                filter(position == "TE")
            
            stats_adj_filtered_rb_wr <- stats_adj_filtered %>%
                filter(position == "RB" | position == "WR") %>%
                group_by(position) %>%
                slice(13:n()) %>%
                ungroup() %>%
                arrange(desc(average_points))
            
            stats_adj_filtered_rb <- stats_adj_filtered_rb_wr %>%
                filter(position == "RB")
            
            stats_adj_filtered_wr <- stats_adj_filtered_rb_wr %>%
                filter(position == "WR") %>%
                group_by(position) %>%
                slice(13:n()) %>%
                ungroup() %>%
                arrange(desc(average_points))
            
            stats_adj_filtered_flex <- stats_adj_filtered_rb %>%
                bind_rows(stats_adj_filtered_wr, stats_adj_filtered_te) %>%
                arrange(desc(average_points)) %>%
                slice(13:n())
            
            # Final selection of top players by position
            stats_adj_replacement <- bind_rows(stats_adj_filtered_qb,stats_adj_filtered_flex) %>%
                arrange(desc(average_points)) %>%
                group_by(position) %>%
                slice(1) %>%
                arrange(position)
            
            # Extracting replacement values for each position
            replacement_values <- data.frame(
                position = stats_adj_replacement$position,
                replacement_points = stats_adj_replacement$average_points
            )
            
            # Calculating adj
            stats_adj <- stats_adj_yearly %>%
                mutate(adj_adj = average_points - replacement_values$replacement_points[match(position,
                                                                                              replacement_values$position)],
                       missed_games = if_else(season <= 2020, 15-games, 16-games),
                       total_points_adj = total_points + (missed_games * replacement_values$replacement_points[match(position,
                                                                                                                     replacement_values$position)]),
                       average_points_adj = total_points_adj/(if_else(season <= 2020, 15, 16))) %>%
                group_by(position) %>%
                mutate(tot_pos_rank = round(rank(-total_points, ties.method = "first")),
                       avg_pos_rank = round(rank(-average_points, ties.method = "first")),
                       adj_pos_rank = round(rank(-average_points_adj, ties.method = "first"))) %>%
                ungroup() %>%
                select(player_id, season,
                       total_points_adj, average_points_adj,
                       tot_pos_rank, avg_pos_rank, adj_pos_rank)
            
            stats_adj_final <- bind_rows(stats_adj_final, stats_adj)
            
        }
        
    } else if (league == "mfl") {
        
        for (i in unique(adj_yearly$season)) {
            
            stats_adj_yearly <- adj_yearly %>%
                filter(season == i) %>%
                arrange(desc(average_points))
            
            stats_adj_filtered <- stats_adj_yearly %>%
                group_by(position) %>%
                slice(13:n()) %>%
                ungroup() %>%
                arrange(desc(average_points))
            
            stats_adj_filtered_qb <- stats_adj_filtered %>%
                filter(position == "QB")
            
            stats_adj_filtered_flex <- stats_adj_filtered %>%
                filter(position == "RB" | position == "WR" | position == "TE") %>%
                slice(37:n()) %>%
                arrange(desc(average_points))
            
            # Final selection of top players by position
            stats_adj_replacement <- bind_rows(stats_adj_filtered_qb,stats_adj_filtered_flex) %>%
                arrange(desc(average_points)) %>%
                group_by(position) %>%
                slice(1) %>%
                arrange(position)
            
            # Extracting replacement values for each position
            replacement_values <- data.frame(
                position = stats_adj_replacement$position,
                replacement_points = stats_adj_replacement$average_points
            )
            
            # Calculating adj
            stats_adj <- stats_adj_yearly %>%
                mutate(adj_adj = average_points - replacement_values$replacement_points[match(position,
                                                                                              replacement_values$position)],
                       missed_games = if_else(season <= 2020, 15-games, 16-games),
                       total_points_adj = total_points + (missed_games * replacement_values$replacement_points[match(position,
                                                                                                                     replacement_values$position)]),
                       average_points_adj = total_points_adj/(if_else(season <= 2020, 15, 16))) %>%
                group_by(position) %>%
                mutate(tot_pos_rank = round(rank(-total_points, ties.method = "first")),
                       avg_pos_rank = round(rank(-average_points, ties.method = "first")),
                       adj_pos_rank = round(rank(-average_points_adj, ties.method = "first"))) %>%
                ungroup() %>%
                select(player_id, season,
                       total_points_adj, average_points_adj,
                       tot_pos_rank, avg_pos_rank, adj_pos_rank)
            
            stats_adj_final <- bind_rows(stats_adj_final, stats_adj)
        }
        
    } else if (league == "superflex mfl") {
        
        for (i in unique(adj_yearly$season)) {
            
            stats_adj_yearly <- adj_yearly %>%
                filter(season == i) %>%
                arrange(desc(average_points))
            
            stats_adj_filtered <- stats_adj_yearly %>%
                group_by(position) %>%
                slice(13:n()) %>%
                ungroup() %>%
                arrange(desc(average_points)) %>%
                slice(13:n())
            
            stats_adj_filtered_qb <- stats_adj_filtered %>%
                filter(position == "QB")
            
            stats_adj_filtered_flex <- stats_adj_filtered %>%
                filter(position != "QB") %>%
                slice(25:n())
            
            # Final selection of top players by position
            stats_adj_replacement <- bind_rows(stats_adj_filtered_qb,stats_adj_filtered_flex) %>%
                arrange(desc(average_points)) %>%
                group_by(position) %>%
                slice(1) %>%
                arrange(position)
            
            # Extracting replacement values for each position
            replacement_values <- data.frame(
                position = stats_adj_replacement$position,
                replacement_points = stats_adj_replacement$average_points
            )
            
            # Calculating adj
            stats_adj <- stats_adj_yearly %>%
                mutate(adj_adj = average_points - replacement_values$replacement_points[match(position,
                                                                                              replacement_values$position)],
                       missed_games = if_else(season <= 2020, 15-games, 16-games),
                       total_points_adj = total_points + (missed_games * replacement_values$replacement_points[match(position,
                                                                                                                     replacement_values$position)]),
                       average_points_adj = total_points_adj/(if_else(season <= 2020, 15, 16))) %>%
                group_by(position) %>%
                mutate(tot_pos_rank = round(rank(-total_points, ties.method = "first")),
                       avg_pos_rank = round(rank(-average_points, ties.method = "first")),
                       adj_pos_rank = round(rank(-average_points_adj, ties.method = "first"))) %>%
                ungroup() %>%
                select(player_id, season,
                       total_points_adj, average_points_adj,
                       tot_pos_rank, avg_pos_rank, adj_pos_rank)
            
            stats_adj_final <- bind_rows(stats_adj_final, stats_adj)
        }
        
    } else if (league == "superflex flex10") {
        
        for (i in unique(adj_yearly$season)) {
            
            stats_adj_yearly <- adj_yearly %>%
                filter(season == i) %>%
                arrange(desc(average_points))
            
            stats_adj_filtered <- stats_adj_yearly %>%
                group_by(position) %>%
                slice(13:n()) %>%
                ungroup() %>%
                arrange(desc(average_points)) %>%
                slice(13:n())
            
            stats_adj_filtered_qb <- stats_adj_filtered %>%
                filter(position == "QB")
            
            stats_adj_filtered_te <- stats_adj_filtered %>%
                filter(position == "TE")
            
            stats_adj_filtered_rb_wr <- stats_adj_filtered %>%
                filter(position == "RB" | position == "WR") %>%
                group_by(position) %>%
                slice(13:n()) %>%
                ungroup() %>%
                arrange(desc(average_points))
            
            stats_adj_filtered_rb <- stats_adj_filtered_rb_wr %>%
                filter(position == "RB")
            
            stats_adj_filtered_wr <- stats_adj_filtered_rb_wr %>%
                filter(position == "WR") %>%
                group_by(position) %>%
                slice(13:n()) %>%
                ungroup() %>%
                arrange(desc(average_points))
            
            stats_adj_filtered_flex <- stats_adj_filtered_rb %>%
                bind_rows(stats_adj_filtered_wr, stats_adj_filtered_te) %>%
                arrange(desc(average_points)) %>%
                slice(13:n())
            
            # Final selection of top players by position
            stats_adj_replacement <- bind_rows(stats_adj_filtered_qb,stats_adj_filtered_flex) %>%
                arrange(desc(average_points)) %>%
                group_by(position) %>%
                slice(1) %>%
                arrange(position)
            
            # Extracting replacement values for each position
            replacement_values <- data.frame(
                position = stats_adj_replacement$position,
                replacement_points = stats_adj_replacement$average_points
            )
            
            # Calculating adj
            stats_adj <- stats_adj_yearly %>%
                mutate(adj_adj = average_points - replacement_values$replacement_points[match(position,
                                                                                              replacement_values$position)],
                       missed_games = if_else(season <= 2020, 15-games, 16-games),
                       total_points_adj = total_points + (missed_games * replacement_values$replacement_points[match(position,
                                                                                                                     replacement_values$position)]),
                       average_points_adj = total_points_adj/(if_else(season <= 2020, 15, 16))) %>%
                group_by(position) %>%
                mutate(tot_pos_rank = round(rank(-total_points, ties.method = "first")),
                       avg_pos_rank = round(rank(-average_points, ties.method = "first")),
                       adj_pos_rank = round(rank(-average_points_adj, ties.method = "first"))) %>%
                ungroup() %>%
                select(player_id, season,
                       total_points_adj, average_points_adj,
                       tot_pos_rank, avg_pos_rank, adj_pos_rank)
            
            stats_adj_final <- bind_rows(stats_adj_final, stats_adj)
            
        }
        
    } else {
        
        cat("Selection not recognized.")
        
    }
    
    stats_yearly <- stats_yearly %>%
        left_join(stats_adj_final, by = c("player_id" = "player_id", "season" = "season"))
    
    # ADP
    stats_yearly <<- stats_yearly %>%
        left_join(adp %>% select(name, season, overall), 
                  by = c("player_display_name" = "name", "season" = "season")) %>%
        mutate(adp = coalesce(overall, 192)) %>%
        group_by(season, position) %>%
        mutate(adp_pos_rank = dense_rank(adp)) %>%
        ungroup() %>%
        mutate(performance_diff = adp_pos_rank - tot_pos_rank) %>%
        select(-overall)
    
    
    # HVO QB player app
    hvo_qb <<- pbp_fantasy %>%
        filter(season_type == "REG", down <= 4, play_type != "no_play") %>%
        left_join(roster_pos, by = c("passer_id" = "gsis_id", "season" = "season"),
                  na_matches="never") %>%
        rename(passer_full_name = full_name,
               passer_position = position) %>%
        left_join(roster_pos, by = c("rusher_id" = "gsis_id", "season" = "season"),
                  na_matches="never") %>%
        rename(rusher_full_name = full_name,
               rusher_position = position) %>%
        mutate(ten_zone_rush = if_else(yardline_100 <= 10 & rush_attempt == 1, 1, 0),
               ten_zone_pass = if_else(yardline_100 <= 10 & pass_attempt == 1 & sack == 0, 1, 0),
               field_touch = case_when(
                   yardline_100 <= 100 & yardline_100 >= 81 & (rush_attempt == 1 | complete_pass == 1) ~ "touch_100_81",
                   yardline_100 <= 80 & yardline_100 >= 61 & (rush_attempt == 1 | complete_pass == 1) ~ "touch_80_61",
                   yardline_100 <= 60 & yardline_100 >= 41 & (rush_attempt == 1 | complete_pass == 1) ~ "touch_60_41",
                   yardline_100 <= 40 & yardline_100 >= 21 & (rush_attempt == 1 | complete_pass == 1) ~ "touch_40_21",
                   yardline_100 <= 20 & yardline_100 >= 0 & (rush_attempt == 1 | complete_pass == 1) ~ "touch_20_1",
                   TRUE ~ "other")) %>%
        filter(rusher_position == "QB" | passer_position == "QB") %>%
        mutate(player_name = if_else(is.na(rusher_full_name), passer_full_name, rusher_full_name),
               player_id = if_else(is.na(rusher_player_id), passer_player_id, rusher_player_id)) %>%
        group_by(season, player_name, player_id) %>%
        summarize(rush_attempts = sum(rush_attempt),
                  ten_zone_rushes = sum(ten_zone_rush),
                  ten_zone_passes = sum(ten_zone_pass),
                  completions = sum(complete_pass),
                  attempts = sum(pass_attempt),
                  total_touches = rush_attempts + attempts,
                  hvo = rush_attempts + ten_zone_passes,
                  non_hvo = total_touches - hvo,
                  hvo_pct = hvo / total_touches,
                  non_hvo_pct = non_hvo / total_touches) %>%
        ungroup() %>%
        pivot_longer(cols = c(hvo_pct, non_hvo_pct),
                     names_to = "hvo_type", values_to = "touch_pct")
    
    # HVO RB player app
    hvo_rb <<- pbp_fantasy %>%
        filter(season_type == "REG", down <= 4, play_type != "no_play") %>%
        left_join(roster_pos, by = c("receiver_id" = "gsis_id", "season" = "season"),
                  na_matches="never") %>%
        rename(receiver_full_name = full_name,
               receiver_position = position) %>%
        left_join(roster_pos, by = c("rusher_id" = "gsis_id", "season" = "season"),
                  na_matches="never") %>%
        rename(rusher_full_name = full_name,
               rusher_position = position) %>%
        mutate(ten_zone_rush = if_else(yardline_100 <= 10 & rush_attempt == 1, 1, 0),
               ten_zone_pass = if_else(yardline_100 <= 10 & pass_attempt == 1 & sack == 0, 1, 0),
               ten_zone_rec = if_else(yardline_100 <= 10 & complete_pass == 1, 1, 0),
               tgt = if_else(complete_pass == 1 | incomplete_pass == 1, 1, 0),
               field_touch = case_when(
                   yardline_100 <= 100 & yardline_100 >= 81 & (rush_attempt == 1 | complete_pass == 1) ~ "touch_100_81",
                   yardline_100 <= 80 & yardline_100 >= 61 & (rush_attempt == 1 | complete_pass == 1) ~ "touch_80_61",
                   yardline_100 <= 60 & yardline_100 >= 41 & (rush_attempt == 1 | complete_pass == 1) ~ "touch_60_41",
                   yardline_100 <= 40 & yardline_100 >= 21 & (rush_attempt == 1 | complete_pass == 1) ~ "touch_40_21",
                   yardline_100 <= 20 & yardline_100 >= 0 & (rush_attempt == 1 | complete_pass == 1) ~ "touch_20_1",
                   TRUE ~ "other")) %>%
        filter(rusher_position == "RB" | receiver_position == "RB") %>%
        mutate(player_name = if_else(is.na(rusher_full_name), receiver_full_name, rusher_full_name),
               player_id = if_else(is.na(rusher_player_id), receiver_player_id, rusher_player_id)) %>%
        group_by(season, player_name, player_id) %>%
        summarize(rush_attempts = sum(rush_attempt),
                  ten_zone_rushes = sum(ten_zone_rush),
                  receptions = sum(complete_pass),
                  total_touches = rush_attempts + receptions,
                  hvo = receptions + ten_zone_rushes,
                  non_hvo = total_touches - hvo,
                  hvo_pct = hvo / total_touches,
                  non_hvo_pct = non_hvo / total_touches,
                  hvo_rec = receptions / total_touches,
                  hvo_rush = ten_zone_rushes / total_touches,
                  tgt = sum(tgt)) %>%
        ungroup() %>%
        pivot_longer(cols = c(hvo_pct, non_hvo_pct, hvo_rec, hvo_rush),
                     names_to = "hvo_type", values_to = "touch_pct")
    
    # HVO WR player app
    hvo_wr <<- pbp_fantasy %>%
        filter(season_type == "REG", down <= 4, play_type != "no_play") %>%
        left_join(roster_pos, by = c("receiver_id" = "gsis_id", "season" = "season"),
                  na_matches="never") %>%
        rename(receiver_full_name = full_name,
               receiver_position = position) %>%
        left_join(roster_pos, by = c("rusher_id" = "gsis_id", "season" = "season"),
                  na_matches="never") %>%
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
        mutate(player_name = if_else(is.na(rusher_full_name), receiver_full_name, rusher_full_name),
               player_id = if_else(is.na(rusher_player_id), receiver_player_id, rusher_player_id)) %>%
        group_by(season, player_name, player_id) %>%
        summarize(rush_attempts = sum(rush_attempt),
                  ten_zone_rushes = sum(ten_zone_rush),
                  tgt = sum(tgt),
                  ten_zone_tgt = sum(ten_zone_tgt),
                  adot = as.numeric(mean(air_yards, na.rm = T)),
                  total_pot_touches = sum(rush_attempt) + sum(tgt),
                  hvo_pot = ten_zone_tgt + ten_zone_rushes,
                  hvo_pot_pct = hvo_pot / total_pot_touches) %>%
        ungroup() %>%
        pivot_longer(cols = c(hvo_pot_pct),
                     names_to = "hvo_type", values_to = "touch_pct")
    
    # HVO TE player app
    hvo_te <<- pbp_fantasy %>%
        filter(season_type == "REG", down <= 4, play_type != "no_play") %>%
        left_join(roster_pos, by = c("receiver_id" = "gsis_id", "season" = "season"),
                  na_matches="never") %>%
        rename(receiver_full_name = full_name,
               receiver_position = position) %>%
        left_join(roster_pos, by = c("rusher_id" = "gsis_id", "season" = "season"),
                  na_matches="never") %>%
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
        filter(rusher_position == "TE" | receiver_position == "TE") %>%
        mutate(player_name = if_else(is.na(rusher_full_name), receiver_full_name, rusher_full_name),
               player_id = if_else(is.na(rusher_player_id), receiver_player_id, rusher_player_id)) %>%
        group_by(season, player_name, player_id) %>%
        summarize(rush_attempts = sum(rush_attempt),
                  ten_zone_rushes = sum(ten_zone_rush),
                  tgt = sum(tgt),
                  ten_zone_tgt = sum(ten_zone_tgt),
                  adot = as.numeric(mean(air_yards, na.rm = T)),
                  total_pot_touches = sum(rush_attempt) + sum(tgt),
                  hvo_pot = ten_zone_tgt + ten_zone_rushes,
                  hvo_pot_pct = hvo_pot / total_pot_touches) %>%
        ungroup() %>%
        pivot_longer(cols = c(hvo_pot_pct),
                     names_to = "hvo_type", values_to = "touch_pct")
    
    # Clustering
    set.seed(214)
    k_max <- 10
    selected_season <- max(stats_yearly$season)
    
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
                ggplot(aes(tot_pos_rank, vorp, color = tier)) +
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
                ggplot(aes(tot_pos_rank, vorp, color = tier)) +
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
    
    vorp_tiers_final <<- vorp_tiers_final 
}
ff_stats_app(scoring = "mfl", league = "mfl")

save(stats_weekly, stats_yearly, hvo_qb, hvo_rb, hvo_wr, hvo_te,
     file = "ff_data_week_6.RData")

load(file = "ff_data_week_6.RData")

# Select player
player <- stats_yearly %>%
    filter(player_display_name == "D.J. Moore" & season == max(season)) %>%
    select(player_display_name, position, recent_team) %>%
    left_join(nflfastR::teams_colors_logos, by = c("recent_team" = "team_abbr"))

selected_season <- stats_yearly %>%
    select(season) %>%
    max()

max_week <- stats_weekly %>%
    filter(season == selected_season) %>%
    select(week) %>%
    max()

selected_player_position <- player$position



# All stats tables
qb_tbl <- stats_yearly %>%
    filter(position == "QB" & season == 2023) %>%
    select(player_display_name, season, recent_team, position, games,
           total_points, tot_pos_rank, average_points, avg_pos_rank, std_dev,
           vorp, adp, adp_pos_rank, performance_diff,
           completions, attempts, passing_yards, passing_tds, passing_air_yards,
           carries, rushing_yards, rushing_tds)

datatable(qb_tbl)

rb_tbl <- stats_yearly %>%
    filter(position == "RB" & season == 2023) %>%
    select(player_display_name, season, recent_team, position, games,
           total_points, tot_pos_rank, average_points, avg_pos_rank, std_dev,
           vorp, adp, adp_pos_rank, performance_diff,
           carries, rushing_yards, rushing_tds,
           receptions, receiving_yards, receiving_tds, targets, target_share,
           receiving_air_yards, receiving_yards_after_catch, wopr) %>%
    mutate(across(c(target_share,wopr), \(x) round(x, 3)))

datatable(rb_tbl)

wr_tbl <- stats_yearly %>%
    filter(position == "WR" & season == 2023) %>%
    select(player_display_name, season, recent_team, position, games,
           total_points, tot_pos_rank, average_points, avg_pos_rank, std_dev,
           vorp, adp, adp_pos_rank, performance_diff,
           receptions, receiving_yards, receiving_tds, targets, target_share,
           receiving_air_yards, receiving_yards_after_catch, wopr) %>%
    mutate(across(c(target_share,wopr), \(x) round(x, 3)))

datatable(wr_tbl)

te_tbl <- stats_yearly %>%
    filter(position == "TE" & season == 2023) %>%
    select(player_display_name, season, recent_team, position, games,
           total_points, tot_pos_rank, average_points, avg_pos_rank, std_dev,
           vorp, adp, adp_pos_rank, performance_diff,
           receptions, receiving_yards, receiving_tds, targets, target_share,
           receiving_air_yards, receiving_yards_after_catch, wopr) %>%
    mutate(across(c(target_share,wopr), \(x) round(x, 3)))

datatable(te_tbl)


# Rolling average table
all_combinations <- expand.grid(player_display_name = unique(stats_weekly$player_display_name),
                                position = player$position,
                                week = 1:max_week)

finishes_pct_tbl <- stats_weekly %>%
    filter(position == player$position & season == selected_season) %>%
    arrange(week) %>%
    group_by(player_display_name) %>%
    mutate(average_points = round(cummean(total_points),2),
           run_total_points = round(cumsum(total_points),2)) %>%
    # ungroup() %>%
    # complete(all_combinations) %>%
    # group_by(player_display_name) %>%
    fill(run_total_points, .direction = "down") %>%
    replace_na(list(total_points = 0)) %>%
    fill(total_points, average_points) %>%
    mutate(pos_rank = round(rank(-run_total_points, ties.method = "first"))) %>%
    ungroup() %>%
    group_by(week) %>%
    mutate(pos_rank = round(rank(-run_total_points, ties.method = "first")),
           week_rank = if_else(total_points == 0,
                               NA, rank(-total_points, ties.method = "first"))) %>%
    ungroup() %>%
    # filter(player_display_name == player$player_display_name) %>%
    select(player_display_name, week, total_points, week_rank, average_points, pos_rank)

pal_hex <- c("#762a83", "#af8dc3", "#e7d4e8", "#f7f7f7",
             "#d9f0d3", "#7fbf7b", "#1b7837")

pal_hex <- c("#1b7837", "#7fbf7b", "#d9f0d3", "#f7f7f7",
             "#e7d4e8", "#af8dc3", "#762a83")

stats_weekly %>%
    filter(position == player$position & season == selected_season) %>%
    arrange(week) %>%
    group_by(player_display_name) %>%
    mutate(average_points = round(cummean(total_points),2),
           run_total_points = round(cumsum(total_points),2)) %>%
    ungroup() %>%
    complete(all_combinations) %>%
    group_by(player_display_name) %>%
    fill(run_total_points, .direction = "down") %>%
    replace_na(list(total_points = 0)) %>%
    fill(total_points, average_points) %>%
    mutate(pos_rank = round(rank(-run_total_points, ties.method = "first"))) %>%
    ungroup() %>%
    group_by(week) %>%
    mutate(pos_rank = round(rank(-run_total_points, ties.method = "first")),
           week_rank = if_else(total_points == 0,
                               NA, rank(-total_points, ties.method = "first"))) %>%
    ungroup() %>%
    filter(player_display_name == player$player_display_name) %>%
    select(week, total_points, week_rank, average_points, pos_rank) %>%
    gt() %>%
    gt_theme_538() %>%
    tab_options(
        heading.align = "center",
    ) %>%
    tab_header(
        title = "Weekly Fantasy Performance",
        subtitle = "Rolling Position Rank by Total Points",
    ) %>%
    cols_align(
        "center"
    ) %>%
    cols_label(
        week = "Week",
        total_points = "Weekly Points",
        week_rank = "Weekly Points Rank",
        average_points = "Rolling Average Points",
        pos_rank = "Position Rank",
    ) %>%
    fmt_number(
        columns = c(total_points, average_points),
        decimals = 1
    ) %>%
    cols_width(
        columns = everything() ~ px(80)
    ) %>%
    gt_color_rows(
        total_points, 
        palette = pal_hex, 
        domain = range(finishes_pct_tbl %>% pull(total_points), na.rm = T)
    ) %>%
    gt_color_rows(
        week_rank, 
        palette = pal_hex_rev, 
        domain = range(finishes_pct_tbl %>% pull(week_rank), na.rm = T)
    ) %>%
    gt_color_rows(
        average_points, 
        palette = pal_hex, 
        domain = range(finishes_pct_tbl %>% pull(average_points), na.rm = T)
    ) %>%
    gt_color_rows(
        pos_rank, 
        palette = pal_hex_rev, 
        domain = range(finishes_pct_tbl %>% pull(pos_rank), na.rm = T)
    ) %>%
    tab_footnote(
        "Figure: @MambaMetrics | Data: @nflfastR"
    )


# Rolling average line graph
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


hvo_qb %>%
    filter(hvo_type == "hvo_pct" &
               season == selected_season &
               attempts >= floor(median(hvo_qb$attempts)/10)*10) %>%
    ggplot(aes(attempts, reorder(player_name, attempts), fill = rush_attempts)) +
    geom_col() +
    scale_x_continuous() +
    scale_fill_gradientn(colors = pal_hex) +
    labs(x = "Attempts",
         y = "",
         title = paste0("Total Attempts (min. ",floor(median(hvo_qb$attempts)/10)*10," attempts)"),
         caption = "Figure: @MambaMetrics | Data: @nflfastR",
         fill = "Rush Att.")+
    theme(axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_blank()) +
    theme_dark()

hvo_rb %>%
    filter(hvo_type == "hvo_pct" & 
               season == selected_season &
               total_touches >= floor(median(hvo_rb$total_touches)/10)*10) %>%
    ggplot(aes(total_touches, reorder(player_name, total_touches), fill = touch_pct)) +
    geom_col() +
    scale_x_continuous() +
    scale_fill_gradientn(colors = pal_hex, labels=scales::percent) +
    labs(x = "Total Touches",
         y = "",
         title = paste0("Total Touches (min. ",floor(median(hvo_rb$total_touches)/10)*10," touches)"),
         caption = "Figure: @MambaMetrics | Data: @nflfastR",
         fill = "HVO %") +
    theme(axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_blank()) +
    theme_dark()

hvo_wr %>%
    filter(season == selected_season & tgt >= floor(median(hvo_wr$tgt)/10)*10) %>%
    ggplot(aes(tgt, reorder(player_name, tgt), fill = adot)) +
    geom_col() +
    scale_x_continuous() +
    scale_fill_gradientn(colors = pal_hex) +
    labs(x = "Targets",
         y = "",
         title = paste0("Total Targets (min. ",floor(median(hvo_wr$tgt)/10)*10," targets)"),
         caption = "Figure: @MambaMetrics | Data: @nflfastR",
         fill = "aDot") +
    theme(axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_blank()) +
    theme_dark()

hvo_te %>%
    filter(season == selected_season & tgt >= floor(median(hvo_te$tgt)/10)*10) %>%
    ggplot(aes(tgt, reorder(player_name, tgt), fill = adot)) +
    geom_col() +
    scale_x_continuous() +
    scale_fill_gradientn(colors = pal_hex) +
    labs(x = "Targets",
         y = "",
         title = paste0("Total Targets (min. ",floor(median(hvo_wr$tgt)/10)*10," targets)"),
         caption = "Figure: @MambaMetrics | Data: @nflfastR",
         fill = "aDot") +
    theme(axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_blank()) +
    theme_dark()


vorp_tiers_final %>%
    filter(position == "QB") %>%
    select(player_display_name, vorp, tier) %>%
    arrange(tier, desc(vorp)) %>%
    gt() %>%
    gt_theme_538() %>%
    tab_options(
        heading.align = "center",
    ) %>%
    tab_header(
        title = "Position Tiers",
        subtitle = "Tiers for Positive Value Players",
    ) %>%
    cols_align(
        "center"
    ) %>%
    cols_label(
        player_display_name = "Player",
        vorp = "VORP",
        tier = "Tier"
    ) %>%
    fmt_number(
        columns = vorp,
        decimals = 1
    ) %>%
    tab_style(
        style = cell_fill(color = "#7fbf7b"),
        locations = cells_body(
            rows = tier == "Tier 1")
    ) %>%
    tab_style(
        style = cell_fill(color = "#d9f0d3"),
        locations = cells_body(
            rows = tier == "Tier 2")
    ) %>%
    tab_style(
        style = cell_fill(color = "#f7f7f7"),
        locations = cells_body(
            rows = tier == "Tier 3")
    ) %>%
    tab_style(
        style = cell_fill(color = "#e7d4e8"),
        locations = cells_body(
            rows = tier == "Tier 4")
    ) %>%
    tab_footnote(
        "Figure: @MambaMetrics | Data: @nflfastR"
    )
    











# Clustering
set.seed(214)
k_max <- 10
selected_season <- 2023

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
            ggplot(aes(tot_pos_rank, vorp, color = tier)) +
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
            ggplot(aes(tot_pos_rank, vorp, color = tier)) +
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








