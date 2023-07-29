library(tidyverse)
library(rvest)

years <- c(2013:2023)
adp <- data.frame()

for (yr in years) {
   
    url <- paste0("https://fantasyfootballcalculator.com/adp/ppr/12-team/all/", yr)

    # Read the HTML content of the webpage
    webpage <- read_html(url)
    
    # Extract the table from the webpage using CSS selector
    table <- html_nodes(webpage, "table.table.adp")
    
    # Convert the table into a data frame
    data <- html_table(table)[[1]]
    
    data <- data %>%
        select(Pick, Name, Pos, Team, Overall, Std.Dev, High, Low, TimesDrafted) %>%
        mutate(season = yr)

    # View the extracted data
    print(data)
    
    # Append to data frame
    adp <- bind_rows(adp, data)
     
}


# Step 1: Compute string distances between names in "adp" and "stats_yearly"
dist_matrix <- stringdist::stringdistmatrix(adp$Name, stats_yearly$player_display_name)

# Step 2: Find the best matches for each name in "adp"
best_matches <- apply(dist_matrix, 1, which.min)

# Step 3: Create a new data frame with corrected names
corrected_names_df <- data.frame(Name = adp$Name,
                                 corrected_name = stats_yearly$player_display_name[best_matches],
                                 stringsAsFactors = FALSE)

# Step 4: Update the "adp" data frame with corrected names
adp$Name <- corrected_names_df$corrected_name


