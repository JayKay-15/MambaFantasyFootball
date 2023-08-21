library(shiny)
library(tidyverse)

# UI ----
fluidPage(
    
    theme = bslib::bs_theme(bootswatch = "darkly"),
    
    titlePanel("Fantasy Football Player Dashboard"),
    
    tabsetPanel(type = "tabs",
                tabPanel("Overview", fluid = T,
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
                                 width = 4
                             ),
                             mainPanel(
                                 fluidRow(
                                     column(12, plotOutput("plot1", height = 300)),
                                     h3(textOutput(""), align = "center")
                                 ),
                                 fluidRow(
                                     column(8, gt_output("plot2")),
                                     column(4, gt_output("plot3"))
                                 )
                             )
                         )
                )
    )
)