#### Historical Fantasy Football ####

library(tidyverse)
library(rvest)
library(janitor)

#### Auction Detail
# Specify the URL of the webpage
# url <- "https://www48.myfantasyleague.com/2022/options?L=24340&O=102" # 2022 auction sf details
# url <- "https://www48.myfantasyleague.com/2023/options?L=17408&O=102" # 2023 auction sf details #3
# url <- "https://www48.myfantasyleague.com/2023/options?L=33976&O=102" # 2023 auction sf details #4
# url <- "https://www48.myfantasyleague.com/2023/options?L=66047&O=102" # 2023 auction sf details #5

url <- "https://www48.myfantasyleague.com/2023/options?L=68194&O=102" # 2023 auction sf details #8 - Us

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

# col4 <- extracted[, 2]

df <- data %>%
    mutate(player = paste(str_match(Player, pattern)[, 3], str_match(Player, pattern)[, 2], sep = " "),
           position = str_match(Player, pattern)[, 5],
           team = str_match(Player, pattern)[, 4],
           `Winning Bid` = as.numeric(sub(".", "", `Winning Bid`)),
           position = factor(position, levels = c("QB","RB","WR","TE"))
    ) %>%
    rename(winning_bid = `Winning Bid`,
           winning_bidder = `Winning Bidder`) %>%
    select(player, team, position, winning_bid, winning_bidder) %>%
    arrange(desc(winning_bid))

# Export to Google Sheets
ss_write <- "https://docs.google.com/spreadsheets/d/1RAsyEd7ZglSqp3K0qyT_XESwTjFLTEPQLANebwzzE-k/edit#gid=1014136497"

googlesheets4::sheet_write(df, ss = ss_write, sheet = "Auction Detail")




#### Auction Summary
# url <- "https://www48.myfantasyleague.com/2023/options?L=17408&O=44" # 2023 auction df summary
url <- "https://www48.myfantasyleague.com/2023/options?L=68194&O=44" # 2023 auction #8 summary

# Read the HTML content of the webpage
webpage <- read_html(url)

# Extract the table from the webpage using CSS selector
table <- html_nodes(webpage, "tbody")

# Convert the table into a data frame
data <- html_table(table)[[1]]

# View the extracted data
print(data)


df <- data %>%
    row_to_names(row_number = 1) %>%
    clean_names() %>%
    select(-starting)

colnames(df) <- c("team", "completed_spent", "completed_players", "open_spent", "open_players",
                  "total_spent", "total_players", "total_remaining")

df

# Export to Google Sheets
ss_write <- "https://docs.google.com/spreadsheets/d/1RAsyEd7ZglSqp3K0qyT_XESwTjFLTEPQLANebwzzE-k/edit#gid=1014136497"

googlesheets4::sheet_write(df, ss = ss_write, sheet = "Auction Summary")





#### Current Auctions
url <- "https://www48.myfantasyleague.com/2023/home/68194#0" # current auctions

# Read the HTML content of the webpage
webpage <- read_html(url)

# Extract the table from the webpage using CSS selector
table <- html_nodes(webpage, "#curr_auctions")

# Convert the table into a data frame
data <- html_table(table)[[1]]

# View the extracted data
print(data)

df <- data %>% head(-3)

# pattern <- "^(.*),\\s*(\\S+)\\s+(\\S+)\\s+(\\S+)$"
# pattern <- "^(.*?),\\s*(\\S+)\\s+(\\S+)\\s+(.+)$"

# col4 <- extracted[, 2]

# df <- data %>%
#     mutate(player = paste(str_match(Player, pattern)[, 3], str_match(Player, pattern)[, 2], sep = " "),
#            position = str_match(Player, pattern)[, 5],
#            team = str_match(Player, pattern)[, 4],
#            `Winning Bid` = as.numeric(sub(".", "", `Winning Bid`)),
#            position = factor(position, levels = c("QB","RB","WR","TE"))
#     ) %>%
#     rename(winning_bid = `Winning Bid`,
#            winning_bidder = `Winning Bidder`) %>%
#     select(player, team, position, winning_bid, winning_bidder) %>%
#     arrange(desc(winning_bid))

# Export to Google Sheets
ss_write <- "https://docs.google.com/spreadsheets/d/1RAsyEd7ZglSqp3K0qyT_XESwTjFLTEPQLANebwzzE-k/edit#gid=1014136497"

googlesheets4::sheet_write(df, ss = ss_write, sheet = "Auction Pending")

