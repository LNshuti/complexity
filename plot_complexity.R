#' ---
#' output: github_document
#' ---

library(gganimate)
library(magick)
library(here)
source(here("map-theme.R"))
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(ggrepel)
library(magrittr)
library(ggplot2)
library(viridis)
library(glue)

# Load the country complexity data
country_data <- read.csv('data/Country_Complexity_Rankings_1995_2021.csv')

# Convert year to date format
data_coi_eci_long <- 
  country_data %>% 
  janitor::clean_names() %>% 
  select(country, contains("rank")) %>% 
  as_tibble() %>% 
  gather(key, values,-country) %>% 
  mutate(year = gsub("eci_rank_","",gsub("coi_rank_","",key))) %>%
  arrange(desc(year)) %>%
  mutate(measure = sub("_\\d+$", "", key)) %>%
  select(-key) 

# Plot changes in ECI and COI rank over years
coi_rankings <- 
  data_coi_eci_long %>%
  filter(measure == "eci_rank") %>% 
  ggplot(aes(x = year, y = values, color = country, group=country)) + 
  geom_line() +
  theme_minimal()
  labs(x = "Year", 
       y = "Rank",
       color = "Measure",
       title = "Changes in ECI and COI Rank Over Years",
       subtitle = "Separated by country") +
  theme_minimal()
  
# Get top and bottom 5 countries
top_countries_eci <- 
  data_coi_eci_long %>% 
  filter(measure == "eci_rank") %>% 
  select(country, values) %>% 
  unique() %>%
  arrange(desc(values)) %>% 
  head(3) %>% 
  pull(country) %>% 
  unique()

bottom_countries_eci <-
  data_coi_eci_long %>% 
  filter(measure == "eci_rank") %>% 
  select(country, values) %>% 
  unique() %>%
  arrange(values) %>% 
  head(3) %>% 
  pull(country) %>% 
  unique()

# Filter data for top and bottom countries
data_filtered <- 
  data_coi_eci_long %>% 
  filter(measure == "eci_rank") %>%
  mutate(country_label = glue("{country}, rank {values}"))

# Filter the data for the last year
data_last_year <- 
  data_filtered %>%
  filter(year == max(year)) 

# Create the plot
plot_complexity_ranks <- 
  ggplot(data_filtered, aes(x = year, y = values, color = country, group=country)) +
  geom_point() +
  geom_line() +
  geom_text_repel(data = data_last_year, 
                  aes(label = country_label), 
                  nudge_x = 1.5, 
                  show.legend = FALSE) +
  ggthemes::theme_tufte() + 
  labs(title = "", x = "", y = "Rank", color = "Country") + 
  guides(color = FALSE) 

plot_complexity_ranks #%>% 
  #ggsave("plots/complexity_plot.png", dpi = 100, width = 4, height = 4)

# BRICS
brics <- 
  data_filtered %>% 
  filter(country %in% c("Brazil", "China", "India",  "South Africa", "Russia", "United States of America"))

data_last_year_brics <- 
  brics %>%
  filter(year == max(year)) 

brics_complexity <- 
  ggplot(brics, aes(x = year, y = values, color = country, group=country)) +
  geom_point() +
  geom_line() +
  geom_text_repel(data = data_last_year_brics, 
                  aes(label = country_label), 
                  nudge_x = 1.2, 
                  show.legend = FALSE) +
  ggthemes::theme_tufte() + 
  labs(title = "", x = "", y = "Complexity Rank", color = "Country") + 
  guides(color = FALSE) 

gdp_ppp <- 
  read_csv("data/API_NY.GDP.PCAP.PP.CD_DS2_en_csv_v2_5728866.csv", skip = 3) %>%
  janitor::clean_names() %>% 
  filter(country_name %in% c("Brazil", "China", "India",  "South Africa", "Russia", "United States"))

# Get the maximum year available in your dataset
max_year <- max(as.numeric(gsub("x", "", names(gdp_ppp)[-(1:4)])))  # Extract year from column names and find max

# Select only the columns from 1990 to the max year
selected_columns <- c("country_name", "country_code", "indicator_name", "indicator_code",
                      paste0("x", 1990:max_year))  # Create a vector of column names to keep

# Use the select() function to keep only the selected columns
df_selected <- select(gdp_ppp, all_of(selected_columns))

# Reshape the data to long format
data_long <- gather(df_selected, key = "year", value = "gdp_per_capita", starts_with("x"))

# Convert the 'year' column to numeric (removing the 'x' prefix)
data_long$year <- as.numeric(gsub("x", "", data_long$year))

# Plot the time series
gdp_percap_brics <- 
  ggplot(data_long, aes(x = year, y = gdp_per_capita, color = country_name)) +
  geom_line() +
  labs(x = "Year", y = "GDP per Capita", color = "Country") +
  theme_minimal()

library(gridExtra)

# Assuming you have ggplot1 and ggplot2 as your two ggplot objects with different titles

# Create the combined plot
combined_plot <- grid.arrange(
  brics_complexity + labs(title = "BRICS complexity"),
  gdp_percap_brics + labs(title = "BICS GDP per Capita PPP"),
  ncol = 1  # Set the number of columns in the grid to 1 (stack the plots vertically)
)

# Print or display the combined plot
print(combined_plot)
