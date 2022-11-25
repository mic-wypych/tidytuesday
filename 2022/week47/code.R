# Week47 ####
library(tidyverse)
library(ggstream)
library(extrafont)
library(ggtext)

#Please note that the plot uses Lato font so make sure you have it installed
loadfonts(device="win")
museums <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-22/museums.csv')



# Get year variable
museums <- museums %>%
  mutate(year = str_extract(Year_opened, "\\d{4}"))


# extract the general types, and count each type for each year
museums_type <- museums %>%
  mutate(type = str_extract(Subject_Matter, "\\w+")) %>%
  group_by(year) %>%
  count(type)

  
# Main plot
final <- museums_type %>%
  filter(year >= 1900, type %in% c("Arts", "War_and_conflict", "Local_Histories", "Buildings", "Industry_and_manufacture")) %>%
  ggplot(aes(as.numeric(year), n, fill = type, color = type, label = type)) +
  geom_stream() +
  annotate(geom = "text", x = 1930, y = -12, size = 5, family = "Lato", label = "War museums first got popular\nin 1930s") +
  annotate(geom = "text", x = 1940, y = 20, size = 5, family = "Lato", label = "Building museums\nboomed from 40s to 50s") +
  annotate(geom = "text", x = 1985, y = -7.5, size = 5, family = "Lato", label = "Industry and local museums\nwere most popular in the 80s") +
  annotate(geom = "text", x = 2010, y = -20, size = 5,family = "Lato", label = "War museums got more\npopular again in 1980s") +
  annotate(geom = "text", x = 2010, y = 20, size = 5,family = "Lato", label = "Number of art museum openings\nwas growing steadily") +
  geom_segment(aes(x = 1930, y = -10, xend = 1930, yend = -5), color = "grey20") +
  geom_point(aes(x = 1930, y = -5), size = 2, alpha = .2, fill = NA, color = "grey20") +
  geom_segment(aes(x = 1942, y = 18, xend = 1942, yend = 5), color = "grey20") +
  geom_segment(aes(x = 1942, y = 5, xend = 1945, yend = 2), color = "grey20") +
  geom_point(aes(x = 1945, y = 2), size = 2, alpha = .2, fill = NA, color = "grey20") +
  geom_segment(aes(x = 2003, y = -20, xend = 1985, yend = -20), color = "grey20") +
  geom_point(aes(x = 1985, y = -20), size = 2, alpha = .2, fill = NA, color = "grey20") +
  geom_segment(aes(x = 1985, y = -6, xend = 1985, yend = 7), color = "grey20") +
  geom_segment(aes(x = 1985, y = -3, xend = 1988, yend = 0), color = "grey20") +
  geom_segment(aes(x = 1985, y = 7, xend = 1988, yend = 10), color = "grey20") +
  geom_point(aes(x = 1988, y = 0), size = 2, alpha = .2, fill = NA, color = "grey20") +
  geom_point(aes(x = 1988, y = 10), size = 2, alpha = .2, fill = NA, color = "grey20") +
  geom_segment(aes(x = 2000, y = 20, xend = 1995, yend = 20), color = "grey20") +
  geom_point(aes(x = 1995, y = 20), size = 2, alpha = .2, fill = NA, color = "grey20") +
  theme_void() +
  scale_x_continuous(breaks = seq(1900, 2020, 10), labels = seq(1900, 2020, 10)) +
  scale_fill_manual(values = c("#5f0f40", "#9a031e","#e36414","#fb8b24","#0f4c5c")) +
  scale_color_manual(values = c("#5f0f40", "#9a031e","#e36414","#fb8b24","#0f4c5c")) +
  labs(title = "Museum openings in the UK in XX and XXI Century",
       caption = "#Tidytuesday 2022 Week 47 | data from https://museweb.dcs.bbk.ac.uk/data | @michal_wypych",
       subtitle = "The general boom for museum openings was in the 1970s and 1980s.<br><span style = 'color:#0f4c5c'>**War museums**</span> first boomed in the 1930s and later in 1980s. Museums devoted to <span style = 'color:#9a031e'><b>buildings</b></span> were most often opened in the 1940s and 1950s.<br><span style = 'color:#fb8b24'><b>Local Histories</b></span> and <span style = 'color:#e36414'><b>industry and manufactures</b></span> museums were most popular during the general museum boom in the 1980s.<br><span style = 'color:#5f0f40'><b>Arts</b></span> did not have a single boom but were gradually growing in numbers of openings until the 1990s.<br><br><br>")+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 15, face = "bold", family = "Lato"),
        plot.title = element_text(size = 40, family = "Lato", hjust = 0.5),
        plot.subtitle = element_markdown(size = 15, hjust = 0.5, family = "Lato", color = "grey40"),
        plot.caption = element_text(size = 10, family = "Lato", color = "grey60"),
        text = element_text(size = 15, family = "Lato", face = "bold"),
        plot.background = element_rect(fill = "#e3d5ca", color = "#e3d5ca"),
        panel.grid.major.x = element_line(color = "grey60", linetype = "dashed"))

final

ggsave(plot = final, filename = "museums.png", dpi = "print", width = 20, height = 10)
