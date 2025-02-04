library(tidyverse)


simpsons_characters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-04/simpsons_characters.csv')
simpsons_episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-04/simpsons_episodes.csv')
simpsons_locations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-04/simpsons_locations.csv')
simpsons_script_lines <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-04/simpsons_script_lines.csv')



simpsons_spoken <- simpsons_script_lines %>% filter(speaking_line == TRUE)



#make a donut plot? for how much each simpson character speaks


simpsons_spoken %>%
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
  geom_text_repel( x=3.5, aes(y=label_pos, label=label), size=3) +
  coord_polar(theta="y") + 
  xlim(c(0, 4)) + 
  theme_void() +
  theme(legend.position = "none")
  
 