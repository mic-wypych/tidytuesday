library(tidyverse)
library(ggstream)
library(MetBrewer)
library(extrafont)

loadfonts(device = 'win')

data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-17/artists.csv')

glimpse(data)

table(data$artist_nationality_other)


plot <- data %>%
  filter(book == 'Gardner') %>%
  group_by(year) %>%
  count(artist_nationality_other) %>%
  ggplot(aes(x = year, y = n, fill = artist_nationality_other)) +
  geom_stream() +
  theme_void() +
  scale_x_continuous(breaks = seq(1920, 2020, 10)) +
  scale_fill_met_d('Tsimshian') +
  labs(title = 'Number of artists of various nationalities in\n Gardners Art through the ages', fill = 'nationality') +
  theme(axis.text.x = element_text(),
        panel.grid.major.x = element_line(color = 'grey20', linetype = 'dashed'),
        plot.background = element_rect(fill = '#f1faee'),
        plot.title = element_text(face = 'bold', hjust = .5, size = 20, family = 'Montserrat', margin = margin(20,20,20,20)),
        text = element_text(family = 'Montserrat'),
        legend.position = 'top')


ggsave('GitHub/tidytuesday/2023/week3.png', plot, width = 15, height = 8)
