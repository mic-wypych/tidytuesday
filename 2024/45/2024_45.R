#2024 week 45 tidytuesday
library(dplyr)
library(tidyr)
library(ggplot2)
library(showtext)
library(patchwork)
library(geomtextpath)


font_add_google("Ubuntu Condensed")
font_add_google("Cabin Condensed")
showtext_auto(enable=T)

showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)


democracy_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-05/democracy_data.csv')

font <- "Ubuntu Condensed"
font_title <- "Cabin Condensed"
theme_45 <- function() {
  theme_minimal(base_size = 13, base_family = font) +
    theme(plot.title = element_text(family = font_title, face = "bold", size = rel(2)),
          plot.subtitle = element_text(family = font, color = "grey20"),
          plot.caption = element_text(family = font, size = rel(.5)),
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    legend.position       = "plot",
    plot.background       = element_rect(fill = "#edede9", color = "#edede9"),
    panel.background      = element_rect(fill = "#edede9", color = "#edede9"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey80", linetype = "dashed", linewidth = .5),
    panel.grid.major.x = element_blank())
}

democracy_data %>%
  glimpse()


# first female presidewnt in datasetdemocracy_data %>%
  filter(is_presidential == TRUE) %>%
  drop_na(is_female_president) %>%
  group_by(year) %>%
  mutate(is_female = sum(is_female_president == TRUE)) %>%
  filter(is_female_president == TRUE) %>%
  ungroup() %>%
  slice_min(year, n = 1) %>%
  select(president_name, country_name, year)

#suffrage vs female president

suffrage_plot <- democracy_data %>%
  filter(is_presidential == T) %>%
  drop_na(has_full_suffrage) %>%
  group_by(year) %>%
  mutate(suffrage = sum(has_full_suffrage == TRUE),
         no_suffrage = sum(has_full_suffrage == FALSE)) %>%
  ggplot() +
  geom_textline(
    aes(x = year, y = suffrage, label = "Has suffrage", color = "Has suffrage"),
    linewidth = .5,
    family = font,
    size = 3,
    text_smoothing = 30
  )+
  geom_textline(
    aes(x = year, y = no_suffrage, label = "No suffrage", color = "No suffrage"),
    linewidth = .5,
    family = font,
    size = 3,
    text_smoothing = 30,fontface = "bold",
  ) +
  scale_color_manual(values = c(
    "Has suffrage" = "#ff006e",
    "No suffrage" = "#8338ec"
  )) +
  labs(x = NULL, y = "number of countries") +
  theme_45()

female_pres_plot <- democracy_data %>%
  filter(is_presidential == T) %>%
  drop_na(is_female_president) %>%
  group_by(year) %>%
  mutate(is_female = sum(is_female_president == TRUE),
         not_female = sum(is_female_president == FALSE)) %>%
  ggplot() +
  geom_textline(
    aes(x = year, y = is_female, label = "female president", color = "Has female president"),
    linewidth = .5,
    family = font,
    size = 3,
    text_smoothing = 30
  )+
  geom_textline(
    aes(x = year, y = not_female, label = "no female president", color = "No female president"),
    linewidth = .5,
    family = font,
    size = 3,
    text_smoothing = 30
  ) +
  annotate(geom = "label", x = 1975, y = 50, label = "Isabel Peron elected\nin 1975 in Argentina", family = font, fill = "#edede9", label.size = 0, size = 3) +
  geom_segment(aes(x = 1975, y = 40, xend = 1975, yend = 3),
               arrow = arrow(length = unit(0.1, "cm")), color = '#6d6875', linewidth = .25) +
  scale_color_manual(values = c(
    "Has female president" = "#ff006e",
    "No female president" = "#8338ec"
  )) +
  labs(x = NULL, y = "number of countries") +
  theme_45()

final_plot <- suffrage_plot/female_pres_plot + plot_annotation(title = "Having suffrage vs having women presidents", subtitle = str_wrap("Among presidential political systems suffrage has become widespread in the second half of 20th Century but the number of women presidents remains very low. the first female president in the dataset is Isabel Peron elected in Argentina in 1975.", width = 100), caption = "#Tidytuesday 2024 Week 45 | data from {democracyData} R package | @michal_wypych", theme = theme_45())


ggsave(glue("C:/Users/User/Documents/GitHub/tidytuesday/2024/45/week_45.png"), final_plot, width = 8, height = 6, units = "in", dpi = 320)

