library(tidyverse)
library(geofacet)


all_countries <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-12/all_countries.csv')
country_regions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-12/country_regions.csv')
global_human_day <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-12/global_human_day.csv')
global_economic_activity <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-12/global_economic_activity.csv')


#explore the data

table(all_countries$country_iso3)


#' ideas:
#' 1. Make a world map with plots on country locations?
#' 2. Make a plot for central-eastern european countries?


# fooling around:
all_countries %>%
  filter(country_iso3 == "POL") %>%
  ggplot(aes(x = Category, fill = Category)) +
  geom_bar() +
  coord_polar()

geofacet::europe_countries_grid1
geofacet::eu_grid1
