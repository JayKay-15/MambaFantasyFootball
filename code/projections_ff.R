#### Projections Fantasy Football ####

library(tidyverse)
library(ffanalytics)
library(DescTools)
library(ggrepel)
library(googlesheets4)


nfl_db <- DBI::dbConnect(RSQLite::SQLite(), "../nfl_sql_db/nfl_db_names")
player_names <- DBI::dbGetQuery(nfl_db, 'SELECT * FROM player_names')
DBI::dbDisconnect(nfl_db)

# Scraping data from multiple sources
my_scrape <- scrape_data(src = c("FantasyPros", "FFToday", "FantasySharks",
                                 "ESPN", "CBS", "NFL"),
                         pos = c("QB", "RB", "WR", "TE"),
                         season = 2023,
                         week = 0)

# No fumble projections for FFToday

pass_yds_adj <- 0.04
pass_tds_adj <- 4
rush_yds_adj <- 0.1
rush_tds_adj <- 6
rec_yds_adj <- 0.1
rec_tds_adj <- 6
rec_adj <- 1
int_adj <- 0
fum_adj <- 0

# Processing QB data
proj_qb <- my_scrape$QB %>%
    mutate(
        player = gsub(" Jr\\.| II| III", "", sub("(^.*),\\s(.*$)", "\\2 \\1", player)),
        across(where(is.numeric), \(.) replace_na(.,0)),
        average_points = (pass_yds*pass_yds_adj + pass_tds*pass_tds_adj
                          + rush_yds*rush_yds_adj + rush_tds*rush_tds_adj
                          + pass_int*int_adj + fumbles_lost*fum_adj) / 17,
        total_points = average_points*16
    ) %>%
    select(data_src, player, team, pos, total_points, average_points) %>%
    add_count(player, name = "count")

# Processing RB data
proj_rb <- my_scrape$RB %>%
    mutate(
        player = gsub(" Jr\\.| II| III", "", sub("(^.*),\\s(.*$)", "\\2 \\1", player)),
        across(where(is.numeric), \(.) replace_na(.,0)),
        average_points = (rush_yds*rush_yds_adj + rush_tds*rush_tds_adj
                             + rec_yds*rec_yds_adj + rec_tds*rec_tds_adj
                             + rec*rec_adj + fumbles_lost*fum_adj) / 17,
        total_points = average_points*16
    ) %>%
    select(data_src,player,team,pos,total_points,average_points) %>%
    add_count(player, name = "count")

# Processing WR data
proj_wr <- my_scrape$WR %>%
    mutate(
        player = gsub(" Jr\\.| II| III", "", sub("(^.*),\\s(.*$)", "\\2 \\1", player)),
        across(where(is.numeric), \(.) replace_na(.,0)),
        average_points = (rush_yds*rush_yds_adj + rush_tds*rush_tds_adj
                          + rec_yds*rec_yds_adj + rec_tds*rec_tds_adj
                          + rec*rec_adj + fumbles_lost*fum_adj) / 17,
        total_points = average_points*16
    ) %>%
    select(data_src,player,team,pos,total_points,average_points) %>%
    add_count(player, name = "count")

# Processing TE data
proj_te <- my_scrape$TE %>%
    mutate(
        player = gsub(" Jr\\.| II| III", "", sub("(^.*),\\s(.*$)", "\\2 \\1", player)),
        across(where(is.numeric), \(.) replace_na(.,0)),
        average_points = (rec_yds*rec_yds_adj + rec_tds*rec_tds_adj
                          + rec*rec_adj + fumbles_lost*fum_adj) / 17,
        total_points = average_points*16
    ) %>%
    select(data_src,player,team,pos,total_points,average_points) %>%
    add_count(player, name = "count")

# Combining and filtering the data
proj_all <- bind_rows(proj_qb, proj_rb, proj_wr, proj_te) %>%
    filter(pos != "FB" & count >= 3) %>%
    select(-count) %>%
    fuzzyjoin::stringdist_join(player_names, by = "player",
                    mode ='left',
                    method = "jw",
                    max_dist = 0.09, 
                    distance_col = 'dist') %>%
    mutate(
        player_display_name = if_else(is.na(player_display_name), 
                                         player.x, player_display_name),
        player_display_name = case_when(
            player.x %in% c("Bijan Robinson") ~ "Bijan Robinson", TRUE ~ player_display_name),
        pos = factor(pos, levels = c("QB","RB","WR","TE"))
        ) %>%
    group_by(player.x) %>%
    slice_min(order_by = dist, n = 1) %>%
    ungroup() %>%
    select(data_src, player_display_name, team, pos, total_points, average_points) %>%
    rename("player" = "player_display_name")

### PPR Super Flex ----
# Calculating robust average
proj_all_robust_avg <- proj_all %>%
    group_by(player, pos) %>%
    reframe(
        est_total = (HodgesLehmann(total_points, conf.level = 0.95, na.rm = T))[1],
        lo_total = (HodgesLehmann(total_points, conf.level = 0.95, na.rm = T))[2],
        hi_total = (HodgesLehmann(total_points, conf.level = 0.95, na.rm = T))[3],
        standard_dev = sd(total_points),
        range = (max(total_points) - min(total_points))
    ) %>%
    mutate(
        est_avg = est_total/16,
        lo_avg = lo_total/16,
        hi_avg = hi_total/16
    ) %>%
    select(player, pos, est_total, lo_total, hi_total, est_avg, lo_avg, hi_avg, standard_dev, range) %>%
    arrange(desc(est_total))

# Filtering top 12 each position and  next top 12 players
proj_filtered <- proj_all_robust_avg %>%
    group_by(pos) %>%
    slice(19:n()) %>%
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
proj_replacement <- bind_rows(proj_filtered_qb,proj_filtered_flex) %>%
    arrange(desc(est_total)) %>%
    group_by(pos) %>%
    slice(1) %>%
    arrange(pos)

# Extracting replacement values for each position
replacement_values <- data.frame(
    pos = proj_replacement$pos,
    replace_est = proj_replacement$est_total,
    replace_lo = proj_replacement$lo_total,
    replace_hi = proj_replacement$hi_total
)

# Calculating value over replacement player (VORP)
proj_vorp_est <- proj_all_robust_avg %>%
    mutate(
        vorp_est = est_total - replacement_values$replace_est[match(pos, replacement_values$pos)],
        vorp_lo = lo_total - replacement_values$replace_est[match(pos, replacement_values$pos)],
        vorp_hi = hi_total - replacement_values$replace_est[match(pos, replacement_values$pos)]
    )

# Calculating VORP total and multiplier
vorp_est_multiplier <- ((300-21)*12) / sum(proj_vorp_est %>% filter(vorp_est >= 0) %>% pull(vorp_est))
vorp_lo_multiplier <- ((300-21)*12) / sum(proj_vorp_est %>% filter(vorp_lo >= 0) %>% pull(vorp_lo))
vorp_hi_multiplier <- ((300-21)*12) / sum(proj_vorp_est %>% filter(vorp_hi >= 0) %>% pull(vorp_hi))

# Calculating value based on VORP and creating new columns
proj_vorp <- proj_vorp_est %>%
    mutate(
        value_est = round(vorp_est*vorp_est_multiplier, 0),
        value_lo = round(vorp_lo*vorp_lo_multiplier, 0),
        value_hi = round(vorp_hi*vorp_hi_multiplier, 0)
    ) %>%
    group_by(pos) %>%
    mutate(pos_rank = round(rank(-value_est, ties.method = "first"))) %>%
    ungroup() %>%
    mutate(across(est_total:vorp_hi, \(.) round(.,1)))

# Creating a plot of VORP versus position rank
proj_vorp_plot <- proj_vorp %>%
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

proj_vorp_plot

proj_vorp_plot_error <- proj_vorp %>%
    filter(vorp_est >= 0) %>%
    ggplot(aes(pos_rank, vorp_est, color = pos)) +
    geom_line() +
    geom_point() +
    geom_ribbon(aes(ymin = vorp_lo, ymax = vorp_hi, fill = pos), alpha = 0.05, linetype = "dashed") +
    scale_x_continuous(breaks = seq(1, 30, 1), limits = c(1, 30)) +
    scale_y_continuous(breaks = seq(-50, 200, 25), limits = c(-50, 200)) +
    scale_color_discrete(breaks = c("QB", "RB", "WR", "TE")) +
    scale_fill_discrete(breaks = c("QB", "RB", "WR", "TE")) +
    theme_bw() +
    theme(panel.grid.minor = element_blank())

proj_vorp_plot_error


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

# Adding risk score
proj_vorp_risk <- proj_vorp %>%
    group_by(pos) %>%
    mutate(across(standard_dev:range, ~ c(scale(.)))) %>%
    mutate(risk_score = round((standard_dev*0.5) + (range*0.5), 3)) %>%
    ungroup()


# Final projections
proj_vorp_final <- proj_vorp %>%
    select(player:hi_avg, vorp_est, value_est, pos_rank) %>%
    left_join(proj_vorp_tiers %>% select(player,tier), by = "player") %>%
    left_join(proj_vorp_risk %>% select(player,risk_score), by = "player") %>%
    select(player:vorp_est, value_est, pos_rank:risk_score)
    

# Export to Google Sheets
ss_write <- "https://docs.google.com/spreadsheets/d/1RAsyEd7ZglSqp3K0qyT_XESwTjFLTEPQLANebwzzE-k/edit#gid=201734091"

sheet_write(proj_vorp_final, ss = ss_write, sheet = "VORP")

# Read from GOogle Sheets
ss_read <- "https://docs.google.com/spreadsheets/d/1RAsyEd7ZglSqp3K0qyT_XESwTjFLTEPQLANebwzzE-k/edit#gid=545950503"

google <- read_sheet(ss = ss_read, sheet = "Export VORP Adj")

google_adj <- google %>%
    group_by(pos) %>%
    mutate(pos_rank = round(rank(-value_adj, ties.method = "first"))) %>%
    ungroup() %>%
    mutate(pos = factor(pos, levels = c("QB","RB","WR","TE")))

google_adj %>%
    filter(vorp_adj >= 0) %>%
    ggplot(aes(pos_rank, value_adj, color = pos)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = seq(1, 40, 1)) +
    scale_y_continuous(breaks = seq(0, 200, 25)) +
    scale_color_discrete(breaks = c("QB", "RB", "WR", "TE")) +
    scale_fill_discrete(breaks = c("QB", "RB", "WR", "TE")) +
    theme_bw() +
    theme(panel.grid.minor = element_blank())

# Re-Clustering
set.seed(214)
k_max <- 10
unique_pos <- unique(google_adj$pos)

proj_vorp_tiers_adj <- data.frame()

for (i in unique_pos) {
    
    km_vorp <- google_adj %>%
        filter(vorp_adj >= 0 & pos == i)
    
    if (i %in% c("TE")) {
        
        km <- kmeans(km_vorp$vorp_adj, centers = 3, nstart = 25, iter.max = 50)
        
        vorp_cluster <- bind_cols(km_vorp, cluster = km$cluster)
        
        unique_clusters <- unique(vorp_cluster$cluster)
        
        for (h in seq_along(unique_clusters)) {
            
            vorp_cluster$cluster[vorp_cluster$cluster == unique_clusters[h]] <- paste0("Tier ", h)
            
            vorp_tiers_adj <- vorp_cluster %>%
                mutate(tier = as_factor(cluster)) %>%
                select(-cluster)
        }
        
        vorp_tiers_viz_adj <- vorp_tiers_adj %>%
            ggplot(aes(pos_rank, value_adj, color = tier)) +
            geom_point() +
            geom_text_repel(aes(label = player), show.legend = F) +
            scale_x_continuous("Position Rank", breaks = seq(1, 30, 1)) +
            scale_y_continuous("Value") +
            theme_bw() +
            theme(panel.grid.minor = element_blank())
        
        print(vorp_tiers_viz_adj)
        
        proj_vorp_tiers_adj <- bind_rows(proj_vorp_tiers_adj,vorp_tiers_adj)
        
    } else {
        
        km <- kmeans(km_vorp$vorp_adj, centers = 4, nstart = 25, iter.max = 50)
        
        vorp_cluster <- bind_cols(km_vorp, cluster = km$cluster)
        
        unique_clusters <- unique(vorp_cluster$cluster)
        
        for (h in seq_along(unique_clusters)) {
            
            vorp_cluster$cluster[vorp_cluster$cluster == unique_clusters[h]] <- paste0("Tier ", h)
            
            vorp_tiers_adj <- vorp_cluster %>%
                mutate(tier = as_factor(cluster)) %>%
                select(-cluster)
        }
        
        vorp_tiers_viz_adj <- vorp_tiers_adj %>%
            ggplot(aes(pos_rank, value_adj, color = tier)) +
            geom_point() +
            geom_text_repel(aes(label = player), show.legend = F) +
            scale_x_continuous("Position Rank", breaks = seq(1, 30, 1)) +
            scale_y_continuous("Value") +
            theme_bw() +
            theme(panel.grid.minor = element_blank())
        
        print(vorp_tiers_viz_adj)
        
        proj_vorp_tiers_adj <- bind_rows(proj_vorp_tiers_adj,vorp_tiers_adj)
        
    }
}



