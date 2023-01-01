library(tidytuesdayR)
library(tidyverse)
library(treemapify)
library(extrafont)
library(patchwork)

loadfonts(device = "win")

#### load data ####
directors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/directors.csv')
episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/episodes.csv')
writers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/writers.csv')
imdb <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/imdb.csv')

#join data
full_data <- episodes %>%
  inner_join(imdb, by = c("season_number"="season", "episode_number" = "ep_num")) %>%
  inner_join(writers, by = "story_number")



#### writers plot ####

#get mean imdb ratings for writers
full_data %>%
  group_by(writer) %>%
  summarize(mean_rating = mean(rating.y, na.rm = T)) %>%
  ungroup() -> writers_ratings

#count number of episodes for each writer
writers %>%
  count(writer) -> writer_counts

#join counts and mean ratings
writer_counts %>%
  inner_join(writers_ratings) -> writers_full

#plot
writer_plot <- writers_full %>%
  ggplot(aes(area = n, group = writer, fill = mean_rating, label = writer)) +
  geom_treemap() +
  geom_treemap_text(place = "middle", color = "grey20", family = "Roboto", fontface = "bold", size = 25) +
  scale_fill_viridis_c(option = "rocket", begin = .3, end = .8, direction = -1) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "grey20", color = NA))

writer_plot

#title and caption
final <- writer_plot + plot_annotation(
  title = "**WHO WROTE THE MOST DR WHO EPISODES AND WHOSE EPISODES HAVE THE BEST IMDB RATINGS?**",
  subtitle = "Color represents mean imdb ratings for writers from <span style = 'color:#F69C73'>**lowest**</span> to <span style = 'color:#751F58'>**highest**</span>",
  caption = "Visualization Michał Wypych | #Tidytuesday week 48 | data: TidyTuesday week 48",
  theme = theme(plot.title = ggtext::element_markdown(size = 40, color = "white", family = "Roboto",margin=margin(b=15)),
                plot.subtitle = ggtext::element_markdown(size = 30,color = "white",  family = "Roboto",margin=margin(b=15)),
                plot.caption = element_text(size = 5,color = "white"),
                plot.background = element_rect(fill = "grey20", color = NA),
                plot.title.position = "plot"))

final


ggsave(filename = "drwho.png", plot = final, dpi = 320, width = 30,height = 15, units = "in")
