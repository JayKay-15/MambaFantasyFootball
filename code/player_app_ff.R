# Fantasy Football Player App
library(shiny)

# Load your stats_yearly and stats_weekly data here
stats_yearly <- stats_yearly %>% arrange(desc(season), player_display_name)
stats_weekly <- stats_weekly %>% arrange(desc(season), player_display_name)

# opportunities
roster_pos <- roster %>%
    filter(position %in% c("QB","RB","WR","TE") & season == 2022) %>%
    select(gsis_id, position, full_name) %>%
    distinct()

# HVO QB player app
hvt_qb <- pbp_fantasy %>%
    filter(season_type == "REG", down <= 4, play_type != "no_play" & season == 2022) %>%
    left_join(roster_pos, by = c("passer_id" = "gsis_id"), na_matches="never") %>%
    rename(passer_full_name = full_name,
           passer_position = position) %>%
    left_join(roster_pos, by = c("rusher_id" = "gsis_id"), na_matches="never") %>%
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
    group_by(player_name, player_id) %>%
    summarize(rush_attempts = sum(rush_attempt),
              ten_zone_rushes = sum(ten_zone_rush),
              ten_zone_passes = sum(ten_zone_pass),
              completions = sum(complete_pass),
              attempts = sum(pass_attempt),
              total_touches = rush_attempts + attempts,
              hvts = rush_attempts + ten_zone_passes,
              non_hvts = total_touches - hvts,
              hvt_pct = hvts / total_touches,
              non_hvt_pct = non_hvts / total_touches) %>%
    pivot_longer(cols = c(hvt_pct, non_hvt_pct),
                 names_to = "hvt_type", values_to = "touch_pct") %>%
    filter(attempts >= 400)

# HVO RB player app
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
    mutate(player_name = if_else(is.na(rusher_full_name), receiver_full_name, rusher_full_name),
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
    filter(total_touches >= 200)

# HVO WR/TE player app
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
    mutate(player_name = if_else(is.na(rusher_full_name), receiver_full_name, rusher_full_name),
           player_id = if_else(is.na(rusher_player_id), receiver_player_id, rusher_player_id)) %>%
    group_by(player_name,
             player_id) %>%
    summarize(rush_attempts = sum(rush_attempt),
              ten_zone_rushes = sum(ten_zone_rush),
              tgt = sum(tgt),
              ten_zone_tgt = sum(ten_zone_tgt),
              adot = as.numeric(mean(air_yards, na.rm = T)),
              total_pot_touches = sum(rush_attempt) + sum(tgt),
              hvt_pot = ten_zone_tgt + ten_zone_rushes,
              hvt_pot_pct = hvt_pot / total_pot_touches) %>%
    pivot_longer(cols = c(hvt_pot_pct),
                 names_to = "hvt_type", values_to = "touch_pct") %>%
    filter(total_pot_touches >= 100)

# Server ----
server <- function(input, output, session) {
    
    # Position filter sync
    selected_position <- reactiveVal("QB")
    
    observeEvent(input$position1, {
        updateSelectInput(session, "position2", selected=input$position1)
        selected_position(input$position1)
    })
    observeEvent(input$position2, {
        updateSelectInput(session, "position1", selected=input$position2)
        selected_position(input$position2)
    })
    observeEvent(input$position2, {
        updateSelectInput(session, "position3", selected=input$position2)
        selected_position(input$position2)
    })
    
    
    # Player filter sync
    selected_player <- reactiveVal(stats_yearly$player_display_name[1])
    
    observeEvent(input$player1, {
        updateSelectInput(session, "player2", selected=input$player1)
        selected_player(input$player1)
    })
    observeEvent(input$player2, {
        updateSelectInput(session, "player1", selected=input$player2)
        selected_player(input$player2)
    })
    observeEvent(input$player2, {
        updateSelectInput(session, "player3", selected=input$player2)
        selected_player(input$player2)
    })
    
    # Update the available players based on the selected position
    observeEvent(selected_position(), {
        players <- stats_yearly %>%
            filter(position == selected_position()) %>%
            pull(player_display_name) %>%
            unique()
        updateSelectInput(session, "player1", choices = players, selected = players[1])
        updateSelectInput(session, "player2", choices = players, selected = players[1])
        updateSelectInput(session, "player3", choices = players, selected = players[1])
    })

    
    # Update the available seasons based on the selected player
    observeEvent(selected_player(), {
        seasons <- stats_yearly %>%
            filter(player_display_name == selected_player()) %>%
            pull(season) %>%
            unique()
        updateSelectInput(session, "year", choices = seasons, selected = max(seasons))
    })

    # Reactive player
    player <- reactive({
        stats_yearly %>%
            filter(player_display_name == selected_player() & season == input$year) %>%
            select(player_display_name, position, recent_team) %>%
            left_join(nflfastR::teams_colors_logos, by = c("recent_team" = "team_abbr"))
    })
    
    # weekly fantasy performance table
    output$plot1 <- render_gt({
        all_combinations <- expand.grid(player_display_name = unique(stats_weekly$player_display_name),
                                        position = player()$position,
                                        week = 1:17)
        
        stats_weekly %>%
            filter(position == player()$position & season == input$year) %>%
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
            mutate(pos_rank = round(rank(-run_total_points, ties.method = "first")),
                   week_rank = if_else(total_points == 0,
                                       NA, rank(-total_points, ties.method = "first"))) %>%
            ungroup() %>%
            filter(player_display_name == player()$player_display_name) %>%
            select(week, total_points, week_rank, average_points, pos_rank) %>%
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
                week_rank = "Weekly Points Rank",
                total_points = "Weekly Points",
                average_points = "Rolling Average Points",
                pos_rank = "Position Rank",
            ) %>%
            fmt_number(
                columns = c(total_points, average_points),
                decimals = 1
            ) %>%
            cols_width(
                columns = everything() ~ px(75)
            ) %>%
            tab_footnote(
                "Figure: @MambaMetrics | Data: @nflfastR"
            )
        
    })
    
    # week and average points by week
    output$plot2 <- renderPlot({
        all_combinations <- expand.grid(player_display_name = unique(stats_weekly$player_display_name),
                                        position = player()$position,
                                        week = 1:17)
        
        stats_weekly %>%
            filter(position == player()$position & season == input$year) %>%
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
            filter(player_display_name == player()$player_display_name) %>%
            select(player_display_name, position, week,
                   total_points, average_points, pos_rank) %>%
            ggplot(aes(week)) +
            geom_line(aes(y = total_points, color = player()$team_color), linetype = "dashed") +
            geom_line(aes(y = average_points, color = player()$team_color2), linetype = "solid") +
            geom_point(aes(y = total_points, color = player()$team_color)) +
            scale_color_manual(
                name = "",
                values = c(player()$team_color, player()$team_color2),
                labels = c("Week Points", "Average Points"),
                guide = guide_legend(override.aes = list(linetype = c("dashed", "solid"),
                                                         color = c(player()$team_color, player()$team_color2)))) +
            labs(title = "Week and Average Points by Week",
                 subtitle = "Week points are the points scored in a particular week",
                 caption = "Figure: @MambaMetrics | Data: @nflfastR") +
            ylab("Week Points") +
            xlab("Week") +
            scale_y_continuous(
                breaks = seq(0, max(stats_weekly$total_points), 5),
                sec.axis = sec_axis(~., breaks = seq(0, max(stats_weekly$total_points), 5),
                                    name = "Average Points", labels = function(x) round(x, 2))) +
            scale_x_continuous(breaks = seq(min(stats_weekly$week), max(stats_weekly$week), 1)) +
            theme_bw()
        
    })
    
    # VORP plot
    output$plot3 <- renderPlot({
        stats_yearly %>%
            filter(
                position == player()$position &
                    season == input$year &
                    (vorp > 0 | player_display_name == player()$player_display_name)
            ) %>%
            ggplot(aes(reorder(player_display_name, vorp), vorp)) +
            geom_bar(stat = "identity", fill = "black", alpha = 0.3) +
            geom_bar(
                data = subset(stats_yearly, player_display_name == player()$player_display_name & season == input$year),
                stat = "identity",
                fill = player()$team_color
            ) +
            labs(title = "Value Over Replacement Player (VORP)",
                 subtitle = "Using Total Points",
                 caption = "Figure: @MambaMetrics | Data: @nflfastR"
            ) +
            xlab("") +
            ylab("VORP") +
            coord_flip() +
            theme_bw()
    })

    # Week segment table
    output$plot4 <- render_gt({
        stats_weekly %>%
            filter(player_display_name == player()$player_display_name) %>%
            mutate(week_group = cut(
                week,
                breaks = c(0, 4, 8, 12, 17),
                labels = c("Weeks 1-4", "Weeks 5-8", "Weeks 9-12", "Weeks 13-17")
            )) %>%
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
            fmt_number(decimals = 1) %>%
            tab_footnote(
                "Figure: @MambaMetrics | Data: @nflfastR"
            )
    })
    
    # Total and average points by season plot
    output$plot5 <- renderPlot({
        stats_yearly %>%
            filter(player_display_name == player()$player_display_name) %>%
            ggplot(aes(season)) +
            geom_line(aes(y = total_points, color = player()$team_color), linetype = "dashed") +
            geom_line(aes(y = average_points * 10, color = player()$team_color2)) +
            geom_point(aes(y = total_points, color = player()$team_color)) +
            geom_point(aes(y = average_points * 10, color = player()$team_color2)) +
            scale_x_continuous(breaks = unique(stats_yearly$season), minor_breaks = NULL) +
            scale_y_continuous(breaks = seq(0, max(stats_yearly$total_points), 50),
                               limits = c(0, max(stats_yearly$total_points)),
                               sec.axis = sec_axis(~ . / 10, name = "Average Points",
                                                   breaks = (seq(0, max(stats_yearly$total_points), 50))/10)) +
            scale_color_manual(
                name = "",
                values = c(player()$team_color, player()$team_color2),
                labels = c("Total Points", "Average Points"),
                guide = "legend"
            ) +
            labs(title = "Total and Average Points by Season",
                 subtitle = "Only Includes Fantasy Season",
                 caption = "Figure: @MambaMetrics | Data: @nflfastR") +
            xlab("Season") +
            ylab("Total Points") +
            theme_bw()
    })
    
    # fantasy performance by season
    output$plot6 <- render_gt({
        stats_yearly %>%
            filter(player_display_name == player()$player_display_name) %>%
            select(season, recent_team, games, vorp, std_dev,
                   total_points, tot_pos_rank, average_points, avg_pos_rank,
                   average_points_adj, adj_pos_rank) %>%
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
                recent_team = "Team",
                games = "Games",
                vorp = "VORP",
                std_dev = "Standard Deviation",
                total_points = "Total Points",
                tot_pos_rank = "Total Points Rank",
                average_points = "Average Points",
                avg_pos_rank = "Average Points Rank",
                average_points_adj = "Adjusted Average Points",
                adj_pos_rank = "Adjusted Points Rank"
            ) %>%
            fmt_number(
                columns = c(total_points, average_points, average_points_adj, std_dev),
                decimals = 1
            )  %>%
            cols_width(
                columns = everything() ~ px(78)
            ) %>%
            tab_footnote(
                "Adjusted average points assumes replacement level performance for missed games"
            ) %>%
            tab_footnote(
                "Figure: @MambaMetrics | Data: @nflfastR"
            ) %>%
            tab_options(footnotes.multiline = TRUE)
    })
    
    # opportunities totals
    output$plot7 <- renderPlot({

        # opportunities by position
        if (player()$position == "QB") {

            # QB total HVOs
            hvt_qb %>%
                filter(
                    hvt_type == "hvt_pct" &
                        (attempts >= 400 | player_name == player()$player_display_name)) %>%
                ggplot(aes(hvts, reorder(player_name, hvts),
                           fill = ifelse(player_name == player()$player_display_name,
                                         "Selected", "Not Selected"))) +
                geom_col() +
                scale_fill_manual(values = c("Selected" = player()$team_color,
                                             "Not Selected" =  alpha("gray", 0.5))) +
                labs(x = "High Value Opportunities",
                     y = "",
                     title = "Number of High Value Opportunities (min. 400 pass attempts)",
                     subtitle = "Pass Attempts Inside the Ten and Rushes",
                     caption = "Figure: @MambaMetrics | Data: @nflfastR") +
                guides(fill = "none") +
                theme(axis.title.y = element_blank(),
                      axis.ticks.y = element_blank(),
                      axis.title.x = element_blank()) +
                theme_bw()


        } else if (player()$position == "RB") {

            # RB total HVOs
            hvt_rb %>%
                filter(
                    hvt_type == "hvt_pct" &
                        (total_touches >= 200 | player_name == player()$player_display_name)) %>%
                ggplot(aes(hvts, reorder(player_name, hvts),
                           fill = ifelse(player_name == player()$player_display_name,
                                         "Selected", "Not Selected"))) +
                geom_col() +
                scale_fill_manual(values = c("Selected" = player()$team_color,
                                             "Not Selected" =  alpha("gray", 0.5))) +
                labs(x = "High Values Opportunities",
                     y = "",
                     title = "Number of High Value Opportunities (min. 200 total touches)",
                     subtitle = "Carries Inside the Ten and Receptions",
                     caption = "Figure: @MambaMetrics | Data: @nflfastR") +
                guides(fill = "none") +
                theme(axis.title.y = element_blank(),
                      axis.ticks.y = element_blank(),
                      axis.title.x = element_blank()) +
                theme_bw()

        } else if (player()$position %in% c("WR", "TE")) {

            # WR/TE targets
            hvt_wr %>%
                ggplot(aes(tgt, reorder(player_name, tgt),
                           fill = ifelse(player_name == player()$player_display_name,
                                         "Selected", "Not Selected"))) +
                geom_col() +
                scale_x_continuous() +
                scale_fill_manual(values = c("Selected" = player()$team_color,
                                             "Not Selected" = alpha("gray", 0.5))) +
                labs(x = "Targets",
                     y = "",
                     title = "Total Targets (min. 100 potential touches)",
                     caption = "Figure: @MambaMetrics | Data: @nflfastR") +
                guides(fill = "none") +
                theme(axis.title.y = element_blank(),
                      axis.ticks.y = element_blank(),
                      axis.title.x = element_blank()) +
                theme_bw()

        } else {
            # Handle the case when the player's position is not recognized
            cat("Selected player's position is not recognized.")
        }

    })

    # opportunities by type
    output$plot8 <- renderPlot({

        # opportunities by position
        if (player()$position == "QB") {

            # QB % HVOs
            hvt_qb %>%
                filter(
                    hvt_type == "hvt_pct" &
                        (attempts >= 400 | player_name == player()$player_display_name)) %>%
                ggplot(aes(touch_pct, reorder(player_name, touch_pct),
                           fill = ifelse(player_name == player()$player_display_name,
                                         "Selected", "Not Selected"))) +
                geom_col() +
                scale_fill_manual(values = c("Selected" = player()$team_color,
                                             "Not Selected" =  alpha("gray", 0.5))) +
                scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
                labs(x = "High Value Opportunities",
                     y = "",
                     title = "Percent of High Value Opportunities (min. 400 pass attempts)",
                     subtitle = "Pass Attempts Inside the Ten and Rushes",
                     caption = "Figure: @MambaMetrics | Data: @nflfastR") +
                guides(fill = "none") +
                theme(axis.title.y = element_blank(),
                      axis.ticks.y = element_blank(),
                      axis.title.x = element_blank()) +
                theme_bw()

        } else if (player()$position == "RB") {

            # RB HVOs by play type
            hvt_rb %>%
                filter(hvt_type %in% c("hvt_rush", "hvt_rec")) %>%
                ggplot(aes(touch_pct, reorder(player_name, touch_pct), fill = hvt_type)) +
                geom_bar(stat = "identity", alpha = 0.3) +
                geom_bar(
                    data = subset(hvt_rb, player_name == player()$player_display_name &
                                      hvt_type %in% c("hvt_rush", "hvt_rec")),
                    stat = "identity") +
                scale_fill_manual(values = c("hvt_rush" = player()$team_color,
                                             "hvt_rec" = player()$team_color2),
                                  labels=c("Rush", "Reception")) +
                scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
                labs(x = "Percent of Plays",
                     y = "",
                     fill = "Play Type",
                     title = "High Value Opportunities as Percentage of Total Touches (min. 200 total touches)",
                     caption = "Figure: @MambaMetrics | Data: @nflfastR") +
                theme(axis.title.y = element_blank(),
                      axis.ticks.y = element_blank(),
                      axis.title.x = element_blank()) +
                theme_bw()

        } else if (player()$position %in% c("WR", "TE")) {

            # WR/TE targets & carries inside the 10
            hvt_wr %>%
                ggplot(aes(touch_pct, reorder(player_name, touch_pct),
                           fill = ifelse(player_name == player()$player_display_name,
                                         "Selected", "Not Selected"))) +
                geom_col() +
                scale_fill_manual(values = c("Selected" = player()$team_color,
                                             "Not Selected" =  alpha("gray", 0.5))) +
                scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
                labs(x = "Percent of High Value Opportunities",
                     y = "",
                     title = "Percentage of High Value Opportunities (min. 100 potential touches)",
                     subtitle = "Targets & Carries Inside the Ten",
                     caption = "Figure: @MambaMetrics | Data: @nflfastR") +
                guides(fill = "none") +
                theme(axis.title.y = element_blank(),
                      axis.ticks.y = element_blank(),
                      axis.title.x = element_blank()) +
                theme_bw()


        } else {
            # Handle the case when the player's position is not recognized
            cat("Selected player's position is not recognized.")
        }

    })
    
}


# UI ----
ui <- fluidPage(
    
    theme = bslib::bs_theme(bootswatch = "darkly"),
    
    titlePanel("Fantasy Football Player Dashboard"),
    
    tabsetPanel(type = "tabs",
                tabPanel("Weekly", fluid = T,
                         sidebarLayout(
                             sidebarPanel(
                                 selectInput("position1", "Select Position:",
                                             choices = c("QB", "RB", "WR", "TE"),
                                             selected = "QB"),
                                 selectInput("player1", "Select Player:",
                                             choices = unique(stats_yearly$player_display_name),
                                             selectize = TRUE),
                                 selectInput("year", "Select Season:",
                                             choices = NULL,
                                             selected = max(stats_yearly$season)),
                                 gt_output("plot1"),
                                 width = 4
                             ),
                             mainPanel(
                                 fluidRow(
                                     column(12, plotOutput("plot2", height = 300)),
                                     h3(textOutput(""), align = "center")
                                 ),
                                 fluidRow(
                                     column(12, plotOutput("plot3", height = 400))
                                 )
                             )
                         )
                ),
                tabPanel("Season", fluid = T,
                         sidebarLayout(
                             sidebarPanel(
                                 selectInput("position2", "Select Position:",
                                             choices = c("QB", "RB", "WR", "TE"),
                                             selected = "QB"),
                                 selectInput("player2", "Select Player:",
                                             choices = unique(stats_yearly$player_display_name),
                                             selectize = TRUE),
                                 gt_output("plot4"),
                                 width = 4
                             ),
                             mainPanel(
                                 fluidRow(
                                     column(12, plotOutput("plot5", height = 300))
                                 ),
                                 fluidRow(
                                     column(12, gt_output("plot6"))
                                 )
                             )
                         )
                ),
                tabPanel("Opportunities", fluid = T,
                         sidebarLayout(
                             sidebarPanel(
                                 selectInput("position3", "Select Position:",
                                             choices = c("QB", "RB", "WR", "TE"),
                                             selected = "QB"),
                                 selectInput("player3", "Select Player:",
                                             choices = unique(stats_yearly$player_display_name),
                                             selectize = TRUE),
                                 width = 4
                             ),
                             mainPanel(
                                 fluidRow(
                                     column(12, plotOutput("plot7", height = 300)),
                                     h3(textOutput(" "), align = "center")
                                 ),
                                 fluidRow(
                                     column(12, plotOutput("plot8"))
                                 )
                             )
                         )
                )
    )
)

# Run the Shiny app
shinyApp(ui, server)



