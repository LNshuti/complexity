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
  head(5) %>% 
  pull(country) %>% 
  unique()

bottom_countries_eci <-
  data_coi_eci_long %>% 
  filter(measure == "eci_rank") %>% 
  select(country, values) %>% 
  unique() %>%
  arrange(values) %>% 
  head(9) %>% 
  pull(country) %>% 
  unique()

  
# Filter data for top and bottom countries
data_filtered <- 
  data_coi_eci_long %>% 
  filter(measure == "eci_rank") %>%
  filter(country %in% c(top_countries_eci, bottom_countries_eci))

# Filter the data for the last year
data_last_year <- 
  data_filtered %>%
  filter(year == max(year))

# Create the plot
plot_complexity_ranks <- 
  ggplot(data_filtered, aes(x = year, y = values, color = country, group=country)) +
  geom_line() +
  geom_text_repel(data = data_last_year, 
                  aes(label = country), 
                  nudge_x = 1.5, 
                  show.legend = FALSE) +
  scale_color_viridis(discrete = TRUE) +
  theme_minimal() + 
  labs(title = "Ranking Countries by Economic Complexity Over Time",
       x = "Year",
       y = "Rank",
       color = "Country") + 
  guides(color = FALSE) 


plot_complexity_ranks %>% 
  ggsave("plots/complexity_plot.png", dpi = 100, width = 4, height = 4)





  