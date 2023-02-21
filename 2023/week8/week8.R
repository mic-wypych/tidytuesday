library(tidyverse)
library(showtext)
#get data
bob_ross <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-21/bob_ross.csv')

#fonts
font_add_google('Spectral')
showtext_auto()

#get the hexes
colors_seped <- bob_ross %>%
  mutate(color_hex = str_remove_all(color_hex, '\\['),
         color_hex = str_remove_all(color_hex, '\\]'),
         color_hex = str_remove_all(color_hex, "'")) %>%
  separate_rows(color_hex, sep = ',')

#hexes for the fill
hexes <- unique(colors_seped$color_hex) %>% str_remove_all(' ')

font <- 'Spectral'

#chappy little accident (I literally got this plot by accident at first lol)
plot <- colors_seped %>%
  group_by(season) %>%
  count(color_hex) %>%
  ggplot(aes(x = season, y = n, size = n*5, color = color_hex))  +
  geom_linerange(aes(x=1,xmin=1,xmax=1,y=0, ymin=0,ymax=26),inherit.aes = F, alpha = .7, linetype = 'dashed', color = '#6d6875') +
  geom_count(alpha = .4) +
  annotate(geom = 'text', x = 1, y = 28, label = 'Season 1', size = 20, family = font, color = '#6d6875') +
  geom_segment(aes(x = 1, y = 22, xend = 10, yend = 22),
               arrow = arrow(length = unit(0.2, "cm")), color = '#6d6875', size = .75) +
  scale_color_manual(values = hexes) +
  coord_polar() +
  scale_size(range = c(.1, 10)) +
  theme_void() +
  labs(title = 'Colors of Bob Ross') +
  theme(legend.position = 'none',
        plot.title = element_text(hjust = .5,vjust = -1.25, size = 100, family = font, color = '#6d6875'),
        plot.background = element_rect(fill = 'white', color = 'white'))
plot  

ggsave('C:/Users/wypyc/Documents/GitHub/tidytuesday/2023/week8/bob_ross.png', plot, width = 7, height = 7)

