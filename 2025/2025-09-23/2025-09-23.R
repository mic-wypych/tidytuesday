library(tidyverse)
fide_ratings_august <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-23/fide_ratings_august.csv')
fide_ratings_september <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-23/fide_ratings_september.csv')


# ok option 1 is bday vs rating
# how can we make this more interesting?
#' add an animation across birh year to make it a wave?
#' or across ratings?
#' we can change shapes to something more chessy

plot_1 <- fide_ratings_august %>%
  drop_na(title) %>%
  ggplot(aes(x = bday, y = rating, color = title)) +
  geom_count(alpha = .5, shape = 23) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"))


ggsave("C:/Users/wypyc/Desktop/plot_1.png", plot_1, width = 10, height = 10, dpi = 300)
