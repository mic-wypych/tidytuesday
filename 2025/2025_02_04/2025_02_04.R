library(tidyverse)
library(showtext)

font_add_google("Lobster")
showtext_opts(dpi = 300)
showtext_auto(enable = T)

simpsons_characters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-04/simpsons_characters.csv')
simpsons_episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-04/simpsons_episodes.csv')
simpsons_locations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-04/simpsons_locations.csv')
simpsons_script_lines <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-04/simpsons_script_lines.csv')



simpsons_spoken <- simpsons_script_lines %>% filter(speaking_line == TRUE)



#make a donut plot? for how much each simpson character speaks

colors <- c("#1d8183", "#d2c85f", "#bc6364","#998ac9", "#a05f13", "#322759", "#891e30", "#c5a775", "#b9719d")

donut_plot <- simpsons_spoken %>%
  mutate(simpson = str_detect(raw_character_text, "Simpson")) %>%
  filter(simpson == TRUE) %>%
  group_by(raw_character_text) %>%
  summarize(sum_words = sum(word_count, na.rm = T)) %>%
  mutate(percentage = sum_words/sum(sum_words),
         ymax = cumsum(percentage),
         ymin = c(0, head(ymax, n=-1)),
         label_pos = (ymax + ymin) / 2,
         label = paste0(raw_character_text, " : ", round(percentage, 2), "%")) %>%
  ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=raw_character_text)) +
  geom_rect() +
  geom_textbox(aes( x=3.5, y=label_pos, label=label), size = 1, family = "Lobster", inherit.aes = FALSE) +
  scale_fill_manual(values = colors) +
  coord_polar(theta="y") + 
  xlim(c(0, 4)) + 
  labs(title = "Who spoke the most out of all Simpsons") +
  theme_void() +
  theme(legend.position = "none",
        text = element_text(family = "Lobster"),
        plot.title.position = "plot",
        plot.background = element_rect(fill = "grey80", color = "grey80"))
  
#to do: get the background to work
# get the labels to work?


# add a table with percentages?

perc_table <- simpsons_spoken %>%
  mutate(simpson = str_detect(raw_character_text, "Simpson")) %>%
  filter(simpson == TRUE) %>%
  group_by(raw_character_text) %>%
  summarize(sum_words = sum(word_count, na.rm = T)) %>%
  mutate(percentage = paste0(round(sum_words/sum(sum_words)*100, 2), "%")) %>%
  gt::gt()

grid.arrange(donut_plot,tableGrob(perc_table)) 
