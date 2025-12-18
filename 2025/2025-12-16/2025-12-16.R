## Tidytuesday 2025-12-16

library(dplyr)
library(ggplot2)
library(tidyr)
library(countrycode)
library(showtext)
library(patchwork)
library(grid)
library(ggnewscale)
font <- "Lato"
font_add_google(font)
#you need to download solid free fa fonts from fontawesome website
font_add("fa", "fa_free_solid_900.otf")
showtext_auto()
# load data ----

roundabouts_clean <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-16/roundabouts_clean.csv')

roundabouts_clean$continent <- countrycode(sourcevar = roundabouts_clean$country,
                            origin = "country.name",
                            destination = "continent")


# plotting ----

palette2 <- c("#b7872fff", "#d81159", "#8f2d56", "#218380", "#73d2de")


#' title etc
#' title: 
#' Finish the subtitle with insights
title <- "Roundabouts around the world"

title_note <- textGrob(
  "Roundabouts around the world",just = "left", x = 0,
  gp = gpar( fontsize   = 60,
    fontface = "bold", fontfamily = font, lineheight = .3)
)

#subtitle
subtitle_note <- textGrob(
  stringr::str_wrap("The plot shows information on roundabouts across countries. The each circle shows number of roundabouts in a given country and category.
             The innermost ring shows types of roundabouts: rotary, roundabout, signalized, traffic calming circle or other/unknown.
             The middle ring shows number of roundabouts with different status: existing, removed or unknown.
             The outermost ring shows number of approaches from 1 to XX.
             USA and France have the largest diversity of approaches while USA and Canada have the largest diversity of types. USA also has the most existing and removed roundabouts.", 120),
  just = "left", x = 0,
  gp = gpar(fontface = "bold", fontsize = 20, fontfamily = font,lineheight = .5)
)

caption <- "Tidytuesday 2025-12-16 | Data {roundabouts} | Michał Wypych"



#count df
#count type
#recode to numeric like 14-20
# use icons instead of shapes
# Need to add icons
type_count <-roundabouts_clean |>
  filter(country != "Kosovo") |>
  arrange(continent, country) |>
  mutate(country = forcats::fct_inorder(country)) |>
  group_by(country, continent) |>
  count(type) |>
  mutate(type_recoded = case_when(
    type == "Rotary" ~ 1,
    type == "Roundabout" ~ 2,
    type == "Signalized Roundabout/Circle" ~ 3,
    type == "Traffic Calming Circle" ~ 4,
    type == "Other" ~ 5,
    type == "Unknown" ~ 6
  ),
  icon = case_when(
    type == "Rotary" ~ "\uf1b9",
    type == "Roundabout" ~ "\uf0e2",
    type == "Signalized Roundabout/Circle" ~ "\uf637",
    type == "Traffic Calming Circle" ~ "\uf207",
    type == "Other" ~ "\uf0d1",
    type == "Unknown" ~ "\uf554"
  )
)


status_count <-roundabouts_clean |>
  filter(country != "Kosovo") |>
  arrange(continent, country) |>
  mutate(country = forcats::fct_inorder(country)) |>
  group_by(country, continent) |>
  count(status) |>
  mutate(status_recoded = case_when(
    status == "Existing" ~ 8,
    status == "Removed" ~ 9,
    status == "Unknown" ~ 10
  ),
icon = case_when(
    status == "Existing" ~ "\uf00c",
    status == "Removed" ~ "\u58",
    status == "Unknown" ~ "\u3f"
  ))


#legend
status_count_usa <- status_count |>
  filter(country == "Canada") |>
  mutate(status = paste0(" ", status))

type_count_usa <- type_count |>
  filter(country == "Canada") |>
  mutate(type = paste0(" ", type))

approaches_usa <- roundabouts_clean |>
  filter(country != "Kosovo") |>
  arrange(continent, country) |>
  mutate(country = forcats::fct_inorder(country),
         approaches = 12 + approaches) |>
  filter(country == "Canada") |>
  count(approaches)

# add names for layers: type, status and number of approaches
multiplier <- 1.5

legend_plot <- type_count_usa |>
  ggplot() +
  geom_text(aes(x = .5, y = type_recoded*multiplier, size = n, label = icon), family = "fa", color ="#d81159") +
  geom_text(aes(x = .5, y = type_recoded*multiplier, label = type), hjust = 0, family = font, size = 6, lineheight = .5) +
  geom_text(data = status_count_usa, aes(x = .5, y = status_recoded*multiplier, size = n, label = icon), family ="fa", color = "#d81159") +
  geom_text(data = status_count_usa,aes(x = .5, y = status_recoded*multiplier, label = status), hjust = 0, family = font, size = 6) +
  scale_size_continuous(range = c(4,4)) +
  new_scale("size") +
  geom_count(data = approaches_usa, aes(x = .5, y = approaches*multiplier), color = "#d81159") +
  geom_text(data = approaches_usa,aes(x = .5, y = approaches*multiplier, label = c("min: 1", " ", " ", " ", "max: 12")), hjust = 0, family = font, size = 6) +
  geom_line(data = data.frame(x = c(0,1, 0,1, 0, 1), y = c(7,7, 11,11, 20,20)*multiplier), aes(x = x, y = y, group = y), color = "grey60", linewidth = .2, linetype = "dashed") +
  geom_text(data = data.frame(x = c(.5,.5,.5), y = c(4,9, 16)*multiplier, label = c("Type", "Status", "Number of\napproaches")),
            aes(x = x, y = y, label = label), nudge_x = -.5,
             inherit.aes = FALSE, family = font, size = 6.5, lineheight = .3) +
  scale_size_continuous(range = c(.5, 1)) +
  coord_radial(expand = FALSE, start = -.5*pi, end = .5*pi, inner.radius = .01) +
  theme_minimal() +
  labs(x = NULL, y = NULL) +
  theme(axis.text = element_blank(),
        panel.grid = element_blank(),
      legend.position = "none",
    plot.margin = margin(0,0,0,0),
  panel.spacing = unit(0, "pt"),
  strip.background = element_blank(),
  strip.text = element_blank(),
  axis.ticks = element_blank(),
  axis.title = element_blank())

legend_plot
## main plot ----

main_plot <- roundabouts_clean |>
  filter(country != "Kosovo") |>
  arrange(continent, country) |>
  mutate(country = forcats::fct_inorder(country),
         approaches = 12 + approaches) |>
  ggplot(aes(x = country, y = approaches, color = continent)) +
  geom_count(alpha = .5) +
  scale_size_continuous(range = c(1, 5)) +
  new_scale("size") +
  geom_text(aes(x = country, y = 26, label = country), angle = 90, hjust = 0, color = "black", size = 6) +
  geom_line(aes(x = country, y = 25.6, color = continent, group = continent), inherit.aes = FALSE, linewidth = 1, lineend = "round") +
  geom_line(aes(x = country, y = 11, group = 1),color = "grey60", inherit.aes = FALSE, linewidth = .5, linetype = "dashed") +
  geom_text(data = type_count, aes(x = country, y = type_recoded, size = n, color = continent, label = icon), family = "fa", inherit.aes = FALSE, alpha = .7) +
  geom_line(aes(x = country, y = 7, group = 1),color = "grey60", inherit.aes = FALSE, linewidth = .5, linetype = "dashed") +
  geom_text(data = status_count, aes(x = country, y = status_recoded, size = n*1.2, label = icon, color = continent), family = "fa", inherit.aes = FALSE, alpha = .7) +
  coord_radial(inner.radius = .2, expand = TRUE, rotate.angle = TRUE) +
  guides(
    theta = guide_axis_theta(angle = 90),
    r     = guide_axis(angle = 0)
  ) +
  labs(x = NULL, y = NULL) +
  scale_y_continuous(labels = NULL ,expand = FALSE) +
  scale_color_manual(values = palette2) +
  scale_size_continuous(range = c(3, 10)) +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family = font),
        axis.text.x = element_blank(),
        plot.title.position = "plot",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey80", linewidth = .3),
      plot.margin = margin(0,0,0,0),
  panel.spacing = unit(0, "pt"),
  strip.background = element_blank(),
  strip.text = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  axis.title = element_blank(), aspect.ratio = 1)

#combine with legend and title
canvas <- ggplot() + theme_void() + theme(plot.margin = margin(0,0,0,0),
  panel.spacing = unit(0, "pt"),
  strip.background = element_blank(),
  strip.text = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  axis.title = element_blank(), aspect.ratio = 1)

final <- canvas +
  inset_element(wrap_elements(full = main_plot), 
                left = .1, bottom = 0, right = .9, top = .75, align_to = "full") +
  inset_element(wrap_elements(full = legend_plot), 
                left = 0.65, bottom = .75, right = 1, top = 1, align_to = "full") +
  inset_element(wrap_elements(full = title_note),
                left =.05, bottom = .9, right = .55, top = .97, align_to = "full") +
   inset_element(wrap_elements(full = subtitle_note),
                 left = .05, bottom = .75, right = .65, top = .9, align_to = "full") +
  plot_annotation(caption = caption, theme = ggplot2::theme(plot.margin = margin(0,0,0,0),
                                     plot.caption = element_text(family = font, size = 20)))

ggsave("C:/Users/wypyc/Desktop/roundabouts_test.png", final, width = 8, height = 8)

