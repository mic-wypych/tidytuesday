library(tidyverse)

parfumo_data_clean <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-10/parfumo_data_clean.csv')

glimpse(parfumo_data_clean)


parfumo_data_clean %>%
  ggplot(aes(y = Rating_Value, x = factor(Release_Year))) +
  geom_count(alpha=.1)
commas_base <- str_count(parfumo_data_clean$Base_Notes, ",")

max(commas_base, na.rm = T)

table(commas_base)


#### look at main notes --------------------------------------------------------

parfumo_data_clean %>%
  separate_rows(Base_Notes, sep = ", ") %>%
  count(Base_Notes) %>%
  arrange(desc(n))
