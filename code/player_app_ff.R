# Fantasy Football Player App
library(shiny)
library(tidyverse)
library(gt)

# Load your stats_yearly and stats_weekly data here
stats_yearly <- stats_yearly %>% arrange(desc(season), player_display_name)
stats_weekly <- stats_weekly %>% arrange(desc(season), player_display_name)

# Server ----
server <- function(input, output, session) {
    
    selected_player <- reactiveVal(stats_yearly$player_display_name[1])
    
    observeEvent(input$player1, {
        updateSelectInput(session, "player2", selected=input$player1)
        selected_player(input$player1)
    })
    observeEvent(input$player2, {
        updateSelectInput(session, "player1", selected=input$player2)
        selected_player(input$player2)
    })
    
    # Update the available seasons based on the selected player
    observeEvent(selected_player(), {
        seasons <- stats_yearly %>%
            filter(player_display_name == selected_player()) %>%
            pull(season) %>%
            unique()
        updateSelectInput(session, "year", choices = seasons, selected = max(seasons))
    })
    
    player <- reactive({
        stats_yearly %>%
            filter(player_display_name == selected_player() & season == input$year) %>%
            select(player_display_name, position, recent_team) %>%
            left_join(nflfastR::teams_colors_logos, by = c("recent_team" = "team_abbr"))
    })

    
    # 
    # output$selectInput1 <- renderUI({
    #     selectInput(inputId = "id1", 
    #                 label = "select", 
    #                 choices = c("a","b","c"), 
    #                 selected = player())
    # })
    # output$selectInput2 <- renderUI({
    #     selectInput(inputId = "id2", 
    #                 label = "select", 
    #                 choices = c("a","b","c"), 
    #                 selected = player())
    # })
    # 
    # observeEvent(input$id2,{
    #     selected_player(input$id2)
    # })
    # 
    # observeEvent(input$id1,{
    #     selected_player(input$id1)
    # })

    # # Update the available seasons based on the selected player
    # observeEvent(input$player, {
    #     seasons <- stats_yearly %>%
    #         filter(player_display_name == input$player) %>%
    #         pull(season) %>%
    #         unique()
    #     updateSelectInput(session, "year", choices = seasons, selected = max(seasons))
    # })
    # 
    # player <- reactive({
    #     stats_yearly %>%
    #         filter(player_display_name == input$player & season == input$year) %>%
    #         select(player_display_name, position, recent_team) %>%
    #         left_join(nflfastR::teams_colors_logos, by = c("recent_team" = "team_abbr"))
    # })
    
    
    
    
    # Total and average points by season plot
    output$plot1 <- renderPlot({
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
                 caption = "Point size is based on the number of games played") +
            xlab("Season") +
            ylab("Total Points") +
            theme_bw()
    })
    
    # Week segment table
    output$plot2 <- render_gt({
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
            fmt_number(decimals = 1)
    })
    
    # Weekly rank table
    output$plot3 <- render_gt({
        stats_weekly %>%
            filter(season == input$year) %>%
            group_by(week, position) %>%
            mutate(week_rank = rank(-total_points, ties.method = "first")) %>%
            ungroup() %>%
            filter(player_display_name == player()$player_display_name) %>%
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
    })
    
    # VORP plot
    output$plot4 <- renderPlot({
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
            labs(title = "Value Over Replacement Player (VORP)", subtitle = "") +
            xlab("") +
            ylab("VORP") +
            coord_flip() +
            theme_bw()
    })
    
    # Stats by season
    output$plot5 <- render_gt({
        if (player()$position == "QB") {
            stats_yearly %>%
                filter(player_display_name == player()$player_display_name) %>%
                select(
                    season, recent_team, games, vorp,
                    passing_yards, passing_tds, passing_epa, rushing_yards, rushing_tds
                ) %>%
                arrange(season) %>%
                gt() %>%
                gt_theme_538() %>%
                tab_options(
                    heading.align = "center",
                ) %>%
                tab_header(title = "Season Stats") %>%
                cols_align("center") %>%
                cols_label(
                    season = "Season",
                    recent_team = "Team",
                    games = "Games",
                    vorp = "VORP",
                    passing_yards = "Pass Yards",
                    passing_tds = "Pass TDs",
                    passing_epa = "Pass EPA",
                    rushing_yards = "Rush Yards",
                    rushing_tds = "Rush TDs"
                ) %>%
                fmt_number(
                    columns = c(vorp, passing_epa),
                    decimals = 1
                )
        } else if (player()$position == "RB") {
            stats_yearly %>%
                filter(player_display_name == player()$player_display_name) %>%
                select(
                    season, recent_team, games, vorp,
                    rushing_yards, rushing_tds, rushing_epa,
                    receptions, receiving_yards, receiving_tds, receiving_epa
                ) %>%
                arrange(season) %>%
                gt() %>%
                gt_theme_538() %>%
                tab_options(
                    heading.align = "center",
                ) %>%
                tab_header(title = "Season Stats") %>%
                cols_align("center") %>%
                cols_label(
                    season = "Season",
                    recent_team = "Team",
                    games = "Games",
                    vorp = "VORP",
                    rushing_yards = "Rush Yards",
                    rushing_tds = "Rush TDs",
                    rushing_epa = "Rush EPA",
                    receptions = "Receptions",
                    receiving_yards = "Receiving Yards",
                    receiving_tds = "Receiving TDs",
                    receiving_epa = "Receiving EPA"
                ) %>%
                fmt_number(
                    columns = c(vorp, rushing_epa, receiving_epa),
                    decimals = 1
                )
        } else if (player()$position %in% c("WR", "TE")) {
            stats_yearly %>%
                filter(player_display_name == player()$player_display_name) %>%
                select(
                    season, recent_team, games,vorp,
                    touches, receptions, receiving_yards, receiving_tds, receiving_epa,
                    rushing_yards, rushing_tds, rushing_epa
                ) %>%
                arrange(season) %>%
                gt() %>%
                gt_theme_538() %>%
                tab_options(
                    heading.align = "center",
                ) %>%
                tab_header(title = "All Stats by Season") %>%
                cols_align("center") %>%
                cols_label(
                    season = "Season",
                    recent_team = "Team",
                    games = "Games",
                    vorp = "VORP",
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
                    columns = c(vorp, receiving_epa, rushing_epa),
                    decimals = 1
                )
        } else {
            # Handle the case when the player's position is not recognized
            cat("Selected player's position is not recognized.")
            gt()  # Return an empty gt table
        }
    })
    
    output$plot6 <- render_gt({
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
        
    })
    
    output$plot7 <- renderPlot({
        all_combinations <- expand.grid(player_display_name = unique(stats_weekly$player_display_name),
                                        position = player()$position,
                                        week = 1:17)
        stats_weekly %>%
            filter(position == player()$position & season == selected_season) %>%
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
                 subtitle = "Week points are the points scored in a particular week") +
            ylab("Week Points") +
            xlab("Week") +
            scale_y_continuous(
                breaks = seq(0, max(stats_weekly$total_points), 5),
                sec.axis = sec_axis(~., breaks = seq(0, max(stats_weekly$total_points), 5),
                                    name = "Average Points", labels = function(x) round(x, 2))) +
            scale_x_continuous(breaks = seq(min(stats_weekly$week), max(stats_weekly$week), 1)) +
            theme_bw()

    })
    
    output$plot8 <- render_gt({
        stats_yearly %>%
            group_by(season, position) %>%
            mutate(avg_pts_rank = round(rank(-average_points, ties.method = "first")),
                   tot_pts_rank = round(rank(-total_points, ties.method = "first")),
                   adj_pts_rank = round(rank(-average_points_adj, ties.method = "first"))) %>%
            ungroup() %>%
            filter(player_display_name == player()$player_display_name) %>%
            select(season, recent_team, games,
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
                recent_team = "Team",
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
            ) %>%
            tab_footnote(
                "Adjusted points assumes replacement level performance for missed games"
            )
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
                                 selectInput("player1", "Select Player:",
                                             choices = unique(stats_yearly$player_display_name),
                                             selectize = TRUE),
                                 selectInput("year", "Select Season:",
                                             choices = NULL,
                                             selected = max(stats_yearly$season)),
                                 gt_output("plot3"),
                                 width = 3
                             ),
                             mainPanel(
                                 fluidRow(
                                     column(12, plotOutput("plot7")),
                                     h3(textOutput("caption"), align = "center")
                                 ),
                                 fluidRow(
                                     column(7, plotOutput("plot4")),
                                     column(5, gt_output("plot6"))
                                 )
                             )
                         )
                ),
                tabPanel("Season", fluid = T,
                         sidebarLayout(
                             sidebarPanel(
                                 selectInput("player2", "Select Player:",
                                             choices = unique(stats_yearly$player_display_name),
                                             selectize = TRUE),
                                 gt_output("plot2"),
                                 width = 4
                             ),
                             mainPanel(
                                 fluidRow(
                                     column(12, plotOutput("plot1"))
                                 ),
                                 fluidRow(
                                     column(12, gt_output("plot8"))
                                 ),
                                 fluidRow(
                                     column(12, gt_output("plot5"))
                                 )
                             )
                         )
                )
    )
)

# Run the Shiny app
shinyApp(ui, server)


