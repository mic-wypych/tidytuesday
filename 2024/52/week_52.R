#week 52
library(tidyverse)
library(zoo)
library(geofacet)
global_holidays <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-24/global_holidays.csv')
monthly_passengers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-24/monthly_passengers.csv')




monthly_passengers <- monthly_passengers %>%
  mutate(Months = ifelse(Month < 10, paste0("0", Month), Month)) %>%
  unite(col = "Date", c(Year, Months), sep = "-", remove = F) %>%
  mutate(Date = as_date(Date, format = "%Y-%m")) %>%
  mutate(Date = zoo::as.yearmon(Date))

global_holidays$Date <- zoo::as.yearmon(global_holidays$Date)

d <- global_holidays %>%
  inner_join(monthly_passengers, by = c("ISO3", "Date"))

monthly_passengers %>%
  ggplot(aes(x = Date, y = Total_OS)) +
  geom_line() +
  geofacet::facet_geo(~ISO3, grid = "europe_countries_grid2")
  
get_grid_names()
  
