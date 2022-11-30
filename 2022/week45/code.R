# Week 45 ####
library(tidyverse)
library(geofacet)
library(extrafont)
library(ggtext)

# The plot uses Roboto font
loadfonts(device = "win")

state_stations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-08/state_stations.csv')


#palette for today:

palette <- c("#457b9d", "#e63946")

countryland <- state_stations %>%
  select(state, format) %>%
  drop_na() %>%
  mutate(state = str_replace_all(state, "_", " ")) %>%
  group_by(state) %>%
  count(format) %>%
  arrange(desc(n), .by_group = T) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(format = case_when(
    format == "Contemporary Christian" ~ "Contemp.\nChristian",
    format == "News Talk Information" ~ "News Talk\nInformation",
    format == "Country" ~ "Country",
    format == "Variety" ~ "Variety",
    format == "News/Talk" ~ "News/Talk",
    format == "Classic hits" ~ "Classic hits",
    format == "College radio" ~ "College radio",
    format == "Religious" ~ "Religious"
  )) %>%
  ggplot() +
  geom_text(aes(x = "", y = "", label = format, color = format == "Country", size = n), fontface ="bold", lineheight = .6, family = "Roboto") +
  facet_geo(~ state, grid = "us_state_grid2", label = "name", scales = "free") +
  scale_color_manual(values = palette) +
  scale_size_continuous(limits = c(0, 150), 
                        breaks = c(10, 50, 100, 150)) +
  guides(size=guide_legend(title = "No. of stations"), color = "none") +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  labs(title = "Countryland",
       caption = "#Tidytuesday 2022 Week 45 | data from Wikipedia | @michal_wypych",
       subtitle = "In majority of states the largest share of radio stations have <span style = 'color:#e63946'>**Country**</span> format.", x = "", y = "") +
  theme(panel.grid.major = element_blank(),
        legend.position = "top",
        axis.text = element_blank(),
        legend.title = element_text(family = "Roboto"),
        legend.text = element_text(family = "Roboto"),
        plot.title = element_text(family = "Roboto", size = 40, face = "bold"),
        plot.subtitle = element_markdown(family = "Roboto", size = 12, face = "bold"),
        plot.caption = element_text(size = 10, family = "Roboto", color = "grey60"),
        strip.text = element_text(face = "bold", family = "Roboto", size = 7),
        plot.background = element_rect(fill = "#edf2f4", color = "#edf2f4"))
  
countryland

ggsave(filename = "radio_states.png", plot = countryland, width = 12, height = 8, dpi = "print")

