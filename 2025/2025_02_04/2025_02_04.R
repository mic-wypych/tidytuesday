library(tidyverse)
library(showtext)
library(ggrepel)
font_add_google("Lobster")
showtext_opts(dpi = 300)
showtext_auto(enable = T)


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
         label = paste0(raw_character_text, " : ", round(percentage*100, 2), "%")) %>%
  ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=raw_character_text)) +
  geom_rect() +
  geom_text_repel(aes( x=3.5, y=label_pos, label=label), color = "white", size = 5, family = "Lobster", inherit.aes = FALSE) +
  scale_fill_manual(values = colors) +
  coord_polar(theta="y") + 
  xlim(c(0, 4)) + 
  labs(title = "Who spoke the most out of all Simpsons") +
  theme_void() +
  theme(legend.position = "none",
        text = element_text(family = "Lobster"),
        plot.title.position = "plot",
        plot.background = element_rect(fill = "pink3", color = "pink3"),
        panel.background = element_rect(fill = "pink3", color = "pink3"),
        plot.title = element_text(color = "white", size = 25)) 



ggsave("simpsons.png", donut_plot, width = 6, height = 6)

