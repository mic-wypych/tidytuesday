library(tidyverse)
library(showtext)
#get data
bob_ross <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-21/bob_ross.csv')

#fonts
font_add_google('lato')
showtext_auto()

#get the hexes
colors_seped <- bob_ross %>%
  mutate(color_hex = str_remove_all(color_hex, '\\['),
         color_hex = str_remove_all(color_hex, '\\]'),
         color_hex = str_remove_all(color_hex, "'")) %>%
  separate_rows(color_hex, sep = ',')

#hexes for the fill
hexes <- unique(colors_seped$color_hex) %>% str_remove_all(' ')

#chappy little accident (I literally got this plot by accident at first lol)
colors_seped %>%
  group_by(season) %>%
  count(color_hex) %>%
  ggplot(aes(x = season, y = n, size = n*5, color = color_hex))  +
  geom_count(alpha = .4) +
  annotate(geom = 'text', x = 1, y = 25, label = 'Season 1', size = 7) +
  geom_segment(aes(x = 1, y = 22, xend = 10, yend = 22),
               arrow = arrow(length = unit(0.1, "cm")), color = 'grey20', size = .5) +
  scale_color_manual(values = hexes) +
  coord_polar() +
  scale_size(range = c(.1, 10)) +
  theme_void() +
  labs(title = 'Colors in bob Ross') +
  theme(legend.position = 'none',
        plot.title = element_text(hjust = .5, size = 50))
  

ggsave('bob_ross', plot, width = 7, height = 7)

