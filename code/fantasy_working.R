library(tidyverse)
library(nflfastR)
library(openxlsx)
library(ffanalytics)
library(DescTools)
library(ggrepel)
library(googlesheets4)
library(stringr)
library(fuzzyjoin)
# library(caret)
# library(forecast)
# library(tsibble)
# library(fable)
# library(feasts)
# library(vars)

# players <- overall %>%
#     mutate(player = player_display_name) %>%
#     select(player, player_display_name)

# nfl_db <- DBI::dbConnect(RSQLite::SQLite(), "/Users/Jesse/Documents/MyStuff/Fantasy Football/NFLdb/NFLdb.sqlite")
# DBI::dbWriteTable(nfl_db, "player_names", players)
# DBI::dbDisconnect(nfl_db)

nfl_db <- DBI::dbConnect(RSQLite::SQLite(), "../NFLdb/nfl_db_names.sqlite")
player_names <- DBI::dbGetQuery(nfl_db, 'SELECT * FROM player_names')
DBI::dbDisconnect(nfl_db)

# Scraping data from multiple sources
my_scrape <- scrape_data(src = c("FantasyPros", "FFToday", "FantasySharks",
                                 "ESPN", "CBS", "NFL"),
                         pos = c("QB", "RB", "WR", "TE"),
                         season = 2023,
                         week = 0)

# Processing QB data
proj_qb <- my_scrape$QB %>%
    mutate(
        average_points = (pass_yds * 0.04 + pass_tds * 4 + rush_yds * 0.1 + rush_tds * 6) / 17,
        total_points = average_points * 16,
        player = gsub(" Jr\\.| II| III", "", sub("(^.*),\\s(.*$)", "\\2 \\1", player))
    ) %>%
    select(data_src, player, team, pos, total_points, average_points) %>%
    add_count(player, name = "count")

# Processing RB data
proj_rb <- my_scrape$RB %>%
    mutate(average_points = (rush_yds*0.1 + rush_tds*6 + rec_yds*0.1 + rec_tds*6 + rec)/17,
           total_points = average_points*16,
           player = gsub(" Jr\\.| II| III", "", sub("(^.*),\\s(.*$)", "\\2 \\1", player))
    ) %>%
    select(data_src,player,team,pos,total_points,average_points) %>%
    add_count(player, name = "count")

# Processing WR data
proj_wr <- my_scrape$WR %>%
    mutate(average_points = (rush_yds*0.1 + rush_tds*6 + rec_yds*0.1 + rec_tds*6 + rec)/17,
           total_points = average_points*16,
           player = gsub(" Jr\\.| II| III", "", sub("(^.*),\\s(.*$)", "\\2 \\1", player))
    ) %>%
    select(data_src,player,team,pos,total_points,average_points) %>%
    add_count(player, name = "count")

# Processing TE data
proj_te <- my_scrape$TE %>%
    mutate(average_points = (rec_yds*0.1 + rec_tds*6 + rec)/17,
           total_points = average_points*16,
           player = gsub(" Jr\\.| II| III", "", sub("(^.*),\\s(.*$)", "\\2 \\1", player))
    ) %>%
    select(data_src,player,team,pos,total_points,average_points) %>%
    add_count(player, name = "count")

# Combining and filtering the data
proj_all <- bind_rows(proj_qb, proj_rb, proj_wr, proj_te) %>%
    filter(pos != "FB" & count >= 3) %>%
    select(-count) %>%
    stringdist_join(player_names, by = "player",
                    mode ='left',
                    method = "jw",
                    max_dist = 0.09, 
                    distance_col = 'dist') %>%
    mutate(player_display_name = if_else(is.na(player_display_name), 
                                         player.x, player_display_name),
           player_display_name = case_when(
                       player.x %in% c("Bijan Robinson") ~ "Bijan Robinson",
                       TRUE ~ player_display_name)) %>%
    group_by(player.x) %>%
    slice_min(order_by = dist, n = 1) %>%
    ungroup() %>%
    select(data_src, player_display_name, team, pos, total_points, average_points) %>%
    rename("player" = "player_display_name")

# proj_all <- bind_rows(proj_qb, proj_rb, proj_wr, proj_te) %>%
#     filter(pos != "FB" & count >= 3) %>%
#     select(-count) %>%
#     mutate(player = case_when(
#         player %in% c("A.J. Dillon") ~ "AJ Dillon",
#         player %in% c("DJ Moore") ~ "D.J. Moore",
#         player %in% c("Ced Wilson") ~ "Cedrick Wilson",
#         TRUE ~ player)) %>%
#     arrange(desc(average_points))   


### PPR Super Flex ----
# Calculating robust average
proj_all_robust_avg <- proj_all %>%
    group_by(player, pos) %>%
    reframe(
        est_total = (HodgesLehmann(total_points, conf.level = .95, na.rm = T))[1],
        lo_total = (HodgesLehmann(total_points, conf.level = .95, na.rm = T))[2],
        hi_total = (HodgesLehmann(total_points, conf.level = .95, na.rm = T))[3]
    ) %>%
    mutate(
        est_avg = est_total/16,
        lo_avg = lo_total/16,
        hi_avg = hi_total/16
    ) %>%
    select(player, pos, est_total, lo_total, hi_total, est_avg, lo_avg, hi_avg) %>%
    arrange(desc(est_total))

# Filtering based on position and selecting top players
proj_filtered <- proj_all_robust_avg %>%
    group_by(pos) %>%
    slice(13:n()) %>%
    ungroup() %>%
    arrange(desc(est_total)) %>%
    slice(13:n())

# Further filtering and selecting top players for QB and flex positions
proj_filtered_qb <- proj_filtered %>%
    filter(pos == "QB")

proj_filtered_flex <- proj_filtered %>%
    filter(pos != "QB") %>%
    slice(25:n())

# Final selection of top players by position
proj_final <- bind_rows(proj_filtered_qb,proj_filtered_flex) %>%
    arrange(desc(est_total)) %>%
    group_by(pos) %>%
    slice(1) %>%
    arrange(pos)

# Extracting replacement values for each position
replace_values <- proj_final[1:4, 3:5]
pos_names <- c("QB", "RB", "TE", "WR")

replacement_values <- data.frame(
  pos = pos_names,
  replace_est = replace_values[, 1],
  replace_lo = replace_values[, 2],
  replace_hi = replace_values[, 3]
)

# Calculating value over replacement player (VORP)
proj_vorp_est <- proj_all_robust_avg %>%
    mutate(
        vorp_est = est_total - replacement_values$est_total[match(pos, replacement_values$pos)],
        vorp_lo = lo_total - replacement_values$est_total[match(pos, replacement_values$pos)],
        vorp_hi = hi_total - replacement_values$est_total[match(pos, replacement_values$pos)]
    )

# Calculating VORP total and multiplier
vorp_est_multiplier <- ((300-21)*12) / sum(proj_vorp_est %>% filter(vorp_est >= 0) %>% pull(vorp_est))
vorp_lo_multiplier <- ((300-21)*12) / sum(proj_vorp_est %>% filter(vorp_lo >= 0) %>% pull(vorp_lo))
vorp_hi_multiplier <- ((300-21)*12) / sum(proj_vorp_est %>% filter(vorp_hi >= 0) %>% pull(vorp_hi))

# Calculating value based on VORP and creating new columns
proj_vorp <- proj_vorp_est %>%
    mutate(
        value_est = round(vorp_est * vorp_est_multiplier, 0),
        value_lo = round(vorp_lo * vorp_lo_multiplier, 0),
        value_hi = round(vorp_hi * vorp_hi_multiplier, 0)
    ) %>%
    group_by(pos) %>%
    mutate(pos_rank = round(rank(-value_est, ties.method = "first"))) %>%
    ungroup() %>%
    mutate(across(c(est_total:vorp_hi), round, 1))

# Creating a plot of VORP versus position rank
proj_vorp_plot <- proj_vorp %>%
    filter(vorp_est >= 0) %>%
    ggplot(aes(pos_rank, vorp_est, color = pos)) +
    geom_line() +
    geom_point() +
    geom_ribbon(aes(ymin = vorp_lo, ymax = vorp_hi, fill = pos), alpha = 0.1, linetype = "dashed") +
    scale_x_continuous(breaks = seq(1, 30, 1), limits = c(1, 30)) +
    scale_y_continuous(breaks = seq(-50, 200, 25), limits = c(-50, 200)) +
    scale_color_discrete(breaks = c("QB", "RB", "WR", "TE")) +
    scale_fill_discrete(breaks = c("QB", "RB", "WR", "TE")) +
    theme_bw() +
    theme(panel.grid.minor = element_blank())

proj_vorp_plot

proj_vorp_plot_2 <- proj_vorp %>%
    filter(vorp_est >= 0) %>%
    ggplot(aes(pos_rank, vorp_est, color = pos)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = seq(1, 30, 1)) +
    scale_y_continuous(breaks = seq(0, 200, 25)) +
    scale_color_discrete(breaks = c("QB", "RB", "WR", "TE")) +
    scale_fill_discrete(breaks = c("QB", "RB", "WR", "TE")) +
    theme_bw() +
    theme(panel.grid.minor = element_blank())

proj_vorp_plot_2


# Export to Google Sheets
ss <- "https://docs.google.com/spreadsheets/d/1RAsyEd7ZglSqp3K0qyT_XESwTjFLTEPQLANebwzzE-k/edit#gid=201734091"

# gs4_create("VORP")
sheet_write(proj_vorp, ss = ss, sheet = "VORP")



# Clustering
set.seed(214)
k_max <- 10
unique_pos <- unique(proj_vorp$pos)

proj_vorp_tiers <- data.frame()

for (i in unique_pos) {
    
    km_vorp <- proj_vorp %>%
        filter(vorp_est >= 0 & pos == i)
    
    # wss <- sapply(1:k_max, 
    #               function(k){kmeans(km_vorp$vorp_est, k, nstart=25, iter.max=50)$tot.withinss})
    # wss
    # plot(1:k_max, wss,
    #      type="b", pch = 19, frame = FALSE, 
    #      xlab="Number of clusters K",
    #      ylab="Total within-clusters sum of squares")
    #
    # aggregate(km_vorp$vorp_est, by=list(cluster=km$cluster), mean)
    
    if (i %in% c("QB","TE")) {
        
        km <- kmeans(km_vorp$vorp_est, centers = 3, nstart = 25, iter.max = 50)

        vorp_cluster <- bind_cols(km_vorp, cluster = km$cluster)
        
        unique_clusters <- unique(vorp_cluster$cluster)
        
        for (h in seq_along(unique_clusters)) {
            
            vorp_cluster$cluster[vorp_cluster$cluster == unique_clusters[h]] <- paste0("Tier ", h)
            
            vorp_tiers <- vorp_cluster %>%
                mutate(tier = as_factor(cluster)) %>%
                select(-cluster)
        }
        
        vorp_tiers_viz <- vorp_tiers %>%
            ggplot(aes(pos_rank, vorp_est, color = tier)) +
            geom_point() +
            geom_text_repel(aes(label = player), show.legend = F) +
            scale_x_continuous("Position Rank", breaks = seq(1, 30, 1)) +
            scale_y_continuous("VORP") +
            theme_bw() +
            theme(panel.grid.minor = element_blank())
        
        print(vorp_tiers_viz)
        
        proj_vorp_tiers <- bind_rows(proj_vorp_tiers,vorp_tiers)
        
    } else {
        
        km <- kmeans(km_vorp$vorp_est, centers = 4, nstart = 25, iter.max = 50)
        
        vorp_cluster <- bind_cols(km_vorp, cluster = km$cluster)
        
        unique_clusters <- unique(vorp_cluster$cluster)
        
        for (h in seq_along(unique_clusters)) {
            
            vorp_cluster$cluster[vorp_cluster$cluster == unique_clusters[h]] <- paste0("Tier ", h)
            
            vorp_tiers <- vorp_cluster %>%
                mutate(tier = as_factor(cluster)) %>%
                select(-cluster)
        }
        
        vorp_tiers_viz <- vorp_tiers %>%
            ggplot(aes(pos_rank, vorp_est, color = tier)) +
            geom_point() +
            geom_text_repel(aes(label = player), show.legend = F) +
            scale_x_continuous("Position Rank", breaks = seq(1, 30, 1)) +
            scale_y_continuous("VORP") +
            theme_bw() +
            theme(panel.grid.minor = element_blank())
        
        print(vorp_tiers_viz)
        
        proj_vorp_tiers <- bind_rows(proj_vorp_tiers,vorp_tiers)
        
    }
}



# Calculating average
proj_all_avg <- proj_all %>%
    group_by(player, pos) %>%
    summarize(
        est_total = mean(total_points, na.rm = TRUE),
        lo_total = min(total_points, na.rm = TRUE),
        hi_total = max(total_points, na.rm = TRUE),
        est_avg = est_total / 16,
        lo_avg = lo_total / 16,
        hi_avg = hi_total / 16
    ) %>%
    arrange(desc(est_total))

# Filtering based on position and selecting top players
proj_filtered <- proj_all_avg %>%
    group_by(pos) %>%
    slice(13:n()) %>%
    ungroup() %>%
    arrange(desc(est_total)) %>%
    slice(13:n())

# Further filtering and selecting top players for QB and flex positions
proj_filtered_qb <- proj_filtered %>%
    filter(pos == "QB")

proj_filtered_flex <- proj_filtered %>%
    filter(pos != "QB") %>%
    slice(25:n())

# Final selection of top players by position
proj_final <- bind_rows(proj_filtered_qb, proj_filtered_flex) %>%
    arrange(desc(est_total)) %>%
    group_by(pos) %>%
    slice(1) %>%
    arrange(pos)

# Extracting replacement values for each position
replace_values <- proj_final[1:4, 3:5]
pos_names <- c("QB", "RB", "TE", "WR")

replacement_values <- data.frame(
    pos = pos_names,
    replace_est = replace_values[, 1],
    replace_lo = replace_values[, 2],
    replace_hi = replace_values[, 3]
)

# Calculating value over replacement player (VORP)
proj_vorp <- proj_all_avg %>%
    mutate(
        vorp_est = est_total - replacement_values$est_total[match(pos, replacement_values$pos)],
        vorp_lo = lo_total - replacement_values$lo_total[match(pos, replacement_values$pos)],
        vorp_hi = hi_total - replacement_values$hi_total[match(pos, replacement_values$pos)]
    )

# Calculating VORP total and multiplier
vorp_est_multiplier <- ((300 - 21) * 12) / sum(proj_vorp %>% filter(vorp_est >= 0) %>% pull(vorp_est))
vorp_lo_multiplier <- ((300 - 21) * 12) / sum(proj_vorp %>% filter(vorp_lo >= 0) %>% pull(vorp_lo))
vorp_hi_multiplier <- ((300 - 21) * 12) / sum(proj_vorp %>% filter(vorp_hi >= 0) %>% pull(vorp_hi))

# Calculating value based on VORP and creating new columns
proj_vorp <- proj_vorp %>%
    mutate(
        value_est = round(vorp_est * vorp_est_multiplier, 0),
        value_lo = round(vorp_lo * vorp_lo_multiplier, 0),
        value_hi = round(vorp_hi * vorp_hi_multiplier, 0)
    ) %>%
    group_by(pos) %>%
    mutate(pos_rank = round(rank(-value_est))) %>%
    ungroup()

# Creating a plot of VORP versus position rank
proj_vorp_plot <- proj_vorp %>%
    filter(vorp_est > 0) %>%
    ggplot(aes(pos_rank, vorp_est, color = pos)) +
    geom_line() +
    geom_point() +
    geom_ribbon(aes(ymin = vorp_lo, ymax = vorp_hi, fill = pos), alpha = 0.1, linetype = "dashed") +
    scale_x_continuous(breaks = seq(1, 30, 1), limits = c(1, 30)) +
    scale_y_continuous(breaks = seq(-25, 175, 25), limits = c(-25, 175)) +
    scale_color_discrete(breaks = c("QB", "RB", "WR", "TE")) +
    scale_fill_discrete(breaks = c("QB", "RB", "WR", "TE")) +
    theme_bw() +
    theme(panel.grid.minor = element_blank())

proj_vorp_plot


### PPR ----
# Calculating robust average
proj_all_robust_avg <- proj_all %>%
    group_by(player, pos) %>%
    reframe(
        est_total = (HodgesLehmann(total_points, conf.level = .95, na.rm = T))[1],
        lo_total = (HodgesLehmann(total_points, conf.level = .95, na.rm = T))[2],
        hi_total = (HodgesLehmann(total_points, conf.level = .95, na.rm = T))[3]
    ) %>%
    mutate(
        est_avg = est_total/16,
        lo_avg = lo_total/16,
        hi_avg = hi_total/16
    ) %>%
    select(player, pos, est_total, lo_total, hi_total, est_avg, lo_avg, hi_avg) %>%
    arrange(desc(est_total))

# Filtering based on position and selecting top players
proj_filtered <- proj_all_robust_avg %>%
    group_by(pos) %>%
    slice(13:n()) %>%
    ungroup() %>%
    arrange(desc(est_total))

# Further filtering and selecting top players for QB and flex positions
proj_filtered_qb <- proj_filtered %>%
    filter(pos == "QB")

proj_filtered_flex <- proj_filtered %>%
    filter(pos != "QB") %>%
    slice(37:n())

# Final selection of top players by position
proj_final <- bind_rows(proj_filtered_qb,proj_filtered_flex) %>%
    arrange(desc(est_total)) %>%
    group_by(pos) %>%
    slice(1) %>%
    arrange(pos)

# Extracting replacement values for each position
replace_values <- proj_final[1:4, 3:5]
pos_names <- c("QB", "RB", "TE", "WR")

replacement_values <- data.frame(
    pos = pos_names,
    replace_est = replace_values[, 1],
    replace_lo = replace_values[, 2],
    replace_hi = replace_values[, 3]
)

# Calculating value over replacement player (VORP)
proj_vorp <- proj_all_robust_avg %>%
    mutate(
        vorp_est = est_total - replacement_values$est_total[match(pos, replacement_values$pos)],
        vorp_lo = lo_total - replacement_values$lo_total[match(pos, replacement_values$pos)],
        vorp_hi = hi_total - replacement_values$hi_total[match(pos, replacement_values$pos)]
    )

# Calculating VORP total and multiplier
vorp_est_multiplier <- ((300-21)*12) / sum(proj_vorp %>% filter(vorp_est >= 0) %>% pull(vorp_est))
vorp_lo_multiplier <- ((300-21)*12) / sum(proj_vorp %>% filter(vorp_lo >= 0) %>% pull(vorp_lo))
vorp_hi_multiplier <- ((300-21)*12) / sum(proj_vorp %>% filter(vorp_hi >= 0) %>% pull(vorp_hi))

# Calculating value based on VORP and creating new columns
proj_vorp <- proj_vorp %>%
    mutate(
        value_est = round(vorp_est * vorp_est_multiplier, 0),
        value_lo = round(vorp_lo * vorp_lo_multiplier, 0),
        value_hi = round(vorp_hi * vorp_hi_multiplier, 0)
    ) %>%
    group_by(pos) %>%
    mutate(pos_rank = round(rank(-value_est))) %>%
    ungroup()

# Creating a plot of VORP versus position rank
proj_vorp_plot <- proj_vorp %>%
    filter(vorp_est > 0) %>%
    ggplot(aes(pos_rank, vorp_est, color = pos)) +
    geom_line() +
    geom_point() +
    geom_ribbon(aes(ymin = vorp_lo, ymax = vorp_hi, fill = pos), alpha = 0.1, linetype = "dashed") +
    scale_x_continuous(breaks = seq(1, 30, 1), limits = c(1, 30)) +
    scale_y_continuous(breaks = seq(-25, 175, 25), limits = c(-25, 175)) +
    scale_color_discrete(breaks = c("QB", "RB", "WR", "TE")) +
    scale_fill_discrete(breaks = c("QB", "RB", "WR", "TE")) +
    theme_bw() +
    theme(panel.grid.minor = element_blank())

proj_vorp_plot

# Calculating average
proj_all_avg <- proj_all %>%
    group_by(player, pos) %>%
    summarize(
        est_total = mean(total_points, na.rm = TRUE),
        lo_total = min(total_points, na.rm = TRUE),
        hi_total = max(total_points, na.rm = TRUE),
        est_avg = est_total / 16,
        lo_avg = lo_total / 16,
        hi_avg = hi_total / 16
    ) %>%
    arrange(desc(est_total))

# Filtering based on position and selecting top players
proj_filtered <- proj_all_avg %>%
    group_by(pos) %>%
    slice(13:n()) %>%
    ungroup() %>%
    arrange(desc(est_total))

# Further filtering and selecting top players for QB and flex positions
proj_filtered_qb <- proj_filtered %>%
    filter(pos == "QB")

proj_filtered_flex <- proj_filtered %>%
    filter(pos != "QB") %>%
    slice(37:n())

# Final selection of top players by position
proj_final <- bind_rows(proj_filtered_qb, proj_filtered_flex) %>%
    arrange(desc(est_total)) %>%
    group_by(pos) %>%
    slice(1) %>%
    arrange(pos)

# Extracting replacement values for each position
replace_values <- proj_final[1:4, 3:5]
pos_names <- c("QB", "RB", "TE", "WR")

replacement_values <- data.frame(
    pos = pos_names,
    replace_est = replace_values[, 1],
    replace_lo = replace_values[, 2],
    replace_hi = replace_values[, 3]
)

# Calculating value over replacement player (VORP)
proj_vorp <- proj_all_avg %>%
    mutate(
        vorp_est = est_total - replacement_values$est_total[match(pos, replacement_values$pos)],
        vorp_lo = lo_total - replacement_values$lo_total[match(pos, replacement_values$pos)],
        vorp_hi = hi_total - replacement_values$hi_total[match(pos, replacement_values$pos)]
    )

# Calculating VORP total and multiplier
vorp_est_multiplier <- ((300 - 21) * 12) / sum(proj_vorp %>% filter(vorp_est >= 0) %>% pull(vorp_est))
vorp_lo_multiplier <- ((300 - 21) * 12) / sum(proj_vorp %>% filter(vorp_lo >= 0) %>% pull(vorp_lo))
vorp_hi_multiplier <- ((300 - 21) * 12) / sum(proj_vorp %>% filter(vorp_hi >= 0) %>% pull(vorp_hi))

# Calculating value based on VORP and creating new columns
proj_vorp <- proj_vorp %>%
    mutate(
        value_est = round(vorp_est * vorp_est_multiplier, 0),
        value_lo = round(vorp_lo * vorp_lo_multiplier, 0),
        value_hi = round(vorp_hi * vorp_hi_multiplier, 0)
    ) %>%
    group_by(pos) %>%
    mutate(pos_rank = round(rank(-value_est))) %>%
    ungroup()

# Creating a plot of VORP versus position rank
proj_vorp_plot <- proj_vorp %>%
    filter(vorp_est > 0) %>%
    ggplot(aes(pos_rank, vorp_est, color = pos)) +
    geom_line() +
    geom_point() +
    geom_ribbon(aes(ymin = vorp_lo, ymax = vorp_hi, fill = pos), alpha = 0.1, linetype = "dashed") +
    scale_x_continuous(breaks = seq(1, 30, 1), limits = c(1, 30)) +
    scale_y_continuous(breaks = seq(-25, 175, 25), limits = c(-25, 175)) +
    scale_color_discrete(breaks = c("QB", "RB", "WR", "TE")) +
    scale_fill_discrete(breaks = c("QB", "RB", "WR", "TE")) +
    theme_bw() +
    theme(panel.grid.minor = element_blank())

proj_vorp_plot


# proj_vorp %>%
#     ggplot(aes(pos,vorp_est)) +
#     geom_boxplot()



# fn <- "2023_projections_ppr"
# u <- paste0("/Users/Jesse/Desktop/",fn,".xlsx")
# 
# wb <- createWorkbook()
# addWorksheet(wb, sheetName = "projections")
# writeData(wb, sheet = "projections", x = proj_vorp)
# saveWorkbook(wb, file = u)














# sandbox weighted average projections
unique_players <- unique(stats_qb$player_display_name)

for (i in unique_players) {

    df <- stats_qb %>%
        arrange(season) %>%
        filter(player_display_name == "Dak Prescott")

    weights <- seq_len(length(df$season)) / sum(seq_len(length(df$season)))

    weighted_averages <- sapply(df[, c(5:28)],
                                FUN = function(x) weighted.mean(x, w = weights))

    print(weighted_averages)


}


unique_players <- unique(stats_te$player_display_name)

for (i in unique_players) {

    df <- stats_te %>%
        arrange(season) %>%
        filter(player_display_name == "Travis Kelce")

    weights <- seq_len(length(df$season)) / sum(seq_len(length(df$season)))

    weighted_averages <- sapply(df[, c(5:20)],
                                FUN = function(x) weighted.mean(x, w = weights))

    print(weighted_averages)


}







# sandbox
test <- stats_qb %>%
    as_tsibble(index = "season", key = "player_display_name") %>%
    filter(player_display_name == "Dak Prescott")

test %>%
    autoplot(fantasy_points_mfl)
    
fit <- test %>%
    model(
        ets = ETS(fantasy_points_mfl),
        arima = ARIMA(fantasy_points_mfl)
    )
fit

fc <- fit %>%
    forecast(h = 10)
fc

fc %>%
    autoplot(test, level = NULL)




data <- data.frame(
    Season = c(2020, 2021, 2022, 2020, 2021, 2022, 2020, 2021, 2022, 2020, 2021, 2022, 2020, 2021, 2022),
    Player = c('Tom', 'Tom', 'Tom', 'Paul', 'Paul', 'Paul', 'Joe', 'Joe', 'Joe', 'David', 'David', 'David', 'John', 'John', 'John'),
    Completions = c(555, 695, 502, 539, 672, 674, 583, 597, 568, 572, 616, 619, 637, 606, 553),
    Yards = c(2601, 2427, 3248, 2364, 2977, 2239, 2858, 3904, 2735, 2398, 2368, 3144, 3671, 3788, 2680),
    TDs = c(10, 21, 21, 17, 28, 29, 30, 19, 10, 21, 26, 30, 18, 15, 25)
)

# weights <- rev(seq_len(num_seasons)) / sum(seq_len(num_seasons))


# Filter the data for the desired players
players <- c("Tom", "Paul", "Joe", "David", "John")
filtered_data <- data[data$player %in% players, ]

# Create a multivariate time series object
player_ts <- ts(data[, c(3:5)], start = c(2020, 1), frequency = 1)

# Fit the VARIMA model
var_model <- VAR(player_ts, p = 1)  # Set the order p as desired

# Generate the projected values
forecast_var <- predict(var_model, n.ahead = 3)  # Set the desired forecast horizon

# Extract the projected completions, yards, and touchdowns
projected_completions <- forecast_var$fcst$Completions
projected_yards <- forecast_var$fcst$Yards
projected_td <- forecast_var$fcst$TDs

projected_completions[,1]









# Install and load the required packages
install.packages("rvest")
library(rvest)

# Specify the URL of the webpage
url <- "https://www48.myfantasyleague.com/2023/options?L=17408&O=102"
url <- "https://www48.myfantasyleague.com/2022/options?L=24340&O=102"

# Read the HTML content of the webpage
webpage <- read_html(url)

# Extract the table from the webpage using CSS selector
table <- html_nodes(webpage, "table.report.nocaption")

# Convert the table into a data frame
data <- html_table(table)[[1]]

# View the extracted data
print(data)



# pattern <- "^(.*),\\s*(\\S+)\\s+(\\S+)\\s+(\\S+)$"
pattern <- "^(.*?),\\s*(\\S+)\\s+(\\S+)\\s+(.+)$"


col4 <- extracted[, 2]


df <- data %>%
    mutate(player = paste(str_match(Player, pattern)[, 3], str_match(Player, pattern)[, 2], sep = " "),
           position = str_match(Player, pattern)[, 5],
           team = str_match(Player, pattern)[, 4]) %>%
    select(player, team, position, `Winning Bid`, `Winning Bidder`)


ss <- "https://docs.google.com/spreadsheets/d/1RAsyEd7ZglSqp3K0qyT_XESwTjFLTEPQLANebwzzE-k/edit#gid=872030387"

# gs4_create("2023_auction_sf")
sheet_write(df, ss = ss, sheet = "2022 Auction SF")


