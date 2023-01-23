library(tidyverse)
library(showtext)
library(widyr)
library(igraph)
library(ggraph)

font_add_google("Merriweather", "Merriweather")

showtext_auto()

loadouts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/loadouts.csv')



plot2 <- loadouts %>%
  pairwise_count(item, name, sort = TRUE, upper = FALSE) %>%
  graph_from_data_frame() %>% 
  ggraph(layout = "fr") + 
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = '#656d4a') +
  geom_node_point(size = 4, shape = 18, color = 'grey20') +
  geom_node_text(aes(label = name), repel = TRUE,
                 size = 9, family='Merriweather') +
  labs(title = 'Items in Alone Series',
       subtitle = 'The plot shows which items were most commonly taken together',
       caption = "#Tidytuesday 2023 Week 4 | data from Alone R package | @michal_wypych"
  ) +
  theme_void() +
  theme(text = element_text(family='Merriweather', size = 20, color = 'grey20'),
        plot.title = element_text(size = 80, family='Merriweather', hjust = .5),
        plot.subtitle =  element_text(size = 20, family='Merriweather', color = 'grey20', lineheight = .3, hjust = .5, margin = margin(0,0,20,0)),
        plot.caption = element_text(size = 10, family = "Merriweather", color = "grey60"),
        plot.background = element_rect(fill='#edede9'),
        legend.position = 'none',
        panel.grid = element_blank(),
        plot.margin = margin(20,20,20,20))
plot2  
ggsave('C:/Users/wypyc/Documents/GitHub/tidytuesday/2023/week4/week4.png', plot2, width = 8, height = 6)
