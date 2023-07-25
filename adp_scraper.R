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


