#week4
library(tidyverse)
library(patchwork)
library(ggtext)
library(gghighlight)
library(ggthemes)

ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/ratings.csv')
details <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/details.csv')

head(details)


#prepare the boardgamecategory variable by removing non-necesary characters
#and separating rows
details <- details %>%
  mutate(boardgamecategory = str_remove(boardgamecategory, "[\\[']")) %>%
  mutate(boardgamecategory = str_remove(boardgamecategory, "[\\]]")) %>%
  mutate(boardgamecategory = str_remove_all(boardgamecategory, "'")) %>%
  separate_rows(boardgamecategory, sep = ",") %>%
  mutate(boardgamecategory = trimws(boardgamecategory))

#prepare the boardgamemechanic variable by removing non-necesary characters
#and separating rows
details <- details %>%
  mutate(boardgamemechanic = str_remove(boardgamemechanic, "[\\[']")) %>%
  mutate(boardgamemechanic = str_remove(boardgamemechanic, "[\\]]")) %>%
  mutate(boardgamemechanic = str_remove_all(boardgamemechanic, "'")) %>%
  separate_rows(boardgamemechanic, sep = ",") %>%
  mutate(boardgamemechanic = trimws(boardgamemechanic))

#main plot of game categories
main <- details %>%
  group_by(yearpublished) %>%
  count(boardgamecategory) %>%
  filter(yearpublished > 1970, yearpublished < 2022, !is.na(boardgamecategory)) %>%
  top_n(1) %>%
  ggplot() +
  geom_vline(aes(xintercept = 2000), linetype = "dashed",color = "grey20", size = 1) +
  geom_segment(aes(x = yearpublished, xend = yearpublished, y = 0, yend = n, color = boardgamecategory)) +
  geom_point(aes(x = yearpublished, y = n, color = boardgamecategory), size = 5) +
  annotate(geom = "text", x = 1995, y = 1300, label = "2000 marks a change \nin the dominant game type", size = 7, color = "grey20") +
  geom_curve(aes(x = 1995, xend = 2000, y = 1100, yend = 1000),arrow = arrow(length = unit(0.03, "npc")), color = "grey20", curvature = .2) +
  scale_x_continuous(breaks = seq(1970, 2021, 5)) +
  scale_color_manual(values = c("Wargame" = "#001219",
                                "Card Game" = "#ae2012")) +
  theme_tufte() +
  theme(legend.position = "none",
        axis.text = element_text(size = 20, family = "roboto"),
        axis.title = element_text(size = 20, family = "roboto")) +
  labs(x = "", y = "number of games")
main

#plot of game mechanics
mechanics <- details %>%
  filter(boardgamecategory %in% c("Wargame", "Card Game"), yearpublished > 1970, yearpublished < 2022) %>%
  mutate(boardgamecategory = recode(boardgamecategory, "Wargame" = "Most common mechanics in wargames",
                                    "Card Game" = "Most common mechanics in card games")) %>%
  group_by(boardgamecategory, yearpublished) %>%
  count(boardgamemechanic) %>%
  top_n(1) %>%
  ggplot(aes(x = yearpublished, y = n, color = boardgamemechanic)) +
  geom_vline(aes(xintercept = 2000), linetype = "dashed", color = "grey20", size = 1) +
  geom_segment(aes(x = yearpublished, xend = yearpublished, y = 0, yend = n)) +
  geom_point(size = 5) +
  scale_x_continuous(breaks = seq(1970, 2021, 5)) +
  scale_color_manual(values = c("Hexagon Grid" = "#0a9396",
                                "Dice Rolling" = "#005f73",
                                "Hand Management" = "#bb3e03")) +
  facet_wrap(~boardgamecategory, ncol = 1) +
  gghighlight(boardgamemechanic %in% c("Hand Management", "Hexagon Grid", "Dice Rolling"), calculate_per_facet = TRUE) +
  theme_tufte() +
  labs(x = "", y = "number of games") +
  theme(axis.text = element_text(size = 20, family = "roboto"),
        strip.text = element_text(size=30, hjust = -.001, family = "roboto"),
        axis.title = element_text(size = 20, family = "roboto"))
mechanics


#final plot
final <- main/mechanics +
  plot_annotation(title = "Wargames toppled by card games",
       subtitle = "Up until 2000 <span style = 'color:#001219'>**wargames**</span> were the most common type of games
       but after that <span style = 'color:#ae2012'>**card games**</span> start to rule. <br>This change coincides with the rise of 
       <span style = 'color:#bb3e03'>**hand management**</span> mechanic in <span style = 'color:#ae2012'>**card games**</span> and a change from <span style = 'color:#0a9396'>**Hexagon grid**</span> to <span style = 'color:#005f73'>**Dice rolling**</span> in <span style = 'color:#001219'>**wargames**</span>",
       caption = "Visualization Michał Wypych | #Tidytuesday 2022 week 4 | data: Kaggle",
       theme = theme(
         plot.title = ggtext::element_markdown(size = 40, family = "roboto"),
         plot.subtitle = ggtext::element_markdown(size = 30, family = "roboto"),
         plot.background = element_rect(color = "#e9d8a6", fill = "#e9d8a6"))
       )

final

#save plot
ggsave(filename = "games.png", plot = final, path = "C:/Users/user/Desktop/dataviz exercise/Tidytuesday/2022/week4", dpi = 320, width = 30,height = 15, units = "in")


