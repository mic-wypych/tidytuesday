## Tidytuesday 2025-12-16

library(dplyr)
library(ggplot2)
library(tidyr)
library(countrycode)
library(showtext)
library(patchwork)
library(grid)

font <- "Lato"
font_add_google(font)

showtext_auto()
# load data ----

roundabouts_clean <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-16/roundabouts_clean.csv')

roundabouts_clean$continent <- countrycode(sourcevar = roundabouts_clean$country,
                            origin = "country.name",
                            destination = "continent")

#ideas

#' simple
#' showing roundabouts across countries and info on them
#' with some raw data + aggregates?



# EDA ----


# plotting ----

palette <- c("#5f0f40", "#9a031e", "#fbd024ff", "#e36414", "#0f4c5c")
palette2 <- c("#b7872fff", "#d81159", "#8f2d56", "#218380", "#73d2de")

#title etc



# adding labels, texts, annotations


#' title etc
#' title: 
title <- "Roundabouts around the world"
subtitle <- "The plot shows information on roundabouts 
            - USA and France have the largest diversity of approaches.
            - US has the most existing roundabouts
            - Something about types e.g. largest diversity of types
            - Most removed roundabouts
            -
            "
caption <- "Tidytuesday 2025-12-16 | Data {roundabouts} | Michał Wypych"
#' include additional variables on the outer ring?


#count df
#count type
#recode to numeric like 14-20
# add shape to legend
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
  ))

# use icons instead of shapes
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
  ))

#make a small legend
#that explains each circle
df_legend <- data.frame(
  y = rep(c(0,6,15, 25),2),
  x = sort(rep(c(0,1), 4)),
  label = rep(c(" ", "status", "type", "n of\napproaches"),2))

df_status <- data.frame(
  x = c(1,1,1),
  y = c(1,3,5),
  status = c("Existing", "Removed", "Unknown"),
  icon = c('\uf015', '\uf15b', '\uf007')
)

df_type <- data.frame(
  x = rep(1, 6),
  y = 8:13,
  type = unique(roundabouts_clean$type))


df_aproaches <- data.frame(
  x = rep(1,9),
  y = 16:24,
  n = 1:9
)

# need to work on the icons
#then I can make the legend, add proper title and subtitle
#and combine with patchwork into final plot
df_legend |>
  ggplot(aes(x = x, y = y, label = label)) +
  geom_text(vjust = 0, nudge_y = -.5) +
  geom_line(aes(x = x, y = y, group = y), color = "grey60", linetype = "dashed") +
  geom_text(data = df_status, aes(x =x ,y = y, label = icon), inherit.aes = FALSE) +
  geom_text(data = df_status, aes(x =x ,y = y, label = status), hjust = -.1) +
  geom_point(data = df_type, aes(x =x ,y = y, shape = type), inherit.aes = FALSE) +
  geom_text(data = df_type, aes(x =x ,y = y, label = type), hjust = -.1) +
  coord_radial(expand = FALSE, inner.radius = .2) +
  theme_minimal() +
  labs(x = NULL, y = NULL) +
  theme(axis.text = element_blank(),
        panel.grid = element_blank(),
      legend.position = "none")

#trying to make it inside the single plot
# add whether it exists or not? Eg. by shape?
test_plot <- roundabouts_clean |>
  filter(country != "Kosovo") |>
  arrange(continent, country) |>
  mutate(country = forcats::fct_inorder(country),
         approaches = 12 + approaches) |>
  ggplot(aes(x = country, y = approaches, color = continent)) +
  geom_count(alpha = .5) +
  geom_text(aes(x = country, y = 26, label = country), angle = 90, hjust = 0, color = "black", size = 4) +
  geom_line(aes(x = country, y = 25, color = continent, group = continent), inherit.aes = FALSE, linewidth = 1, lineend = "round") +
  geom_line(aes(x = country, y = 11, group = 1),color = "grey60", inherit.aes = FALSE, linewidth = .5, linetype = "dashed") +
  geom_count(data = type_count, aes(x = country, y = type_recoded, size = n, color = continent, shape = type), inherit.aes = FALSE, alpha = .7) +
  geom_line(aes(x = country, y = 7, group = 1),color = "grey60", inherit.aes = FALSE, linewidth = .5, linetype = "dashed") +
  geom_count(data = status_count, aes(x = country, y = status_recoded, size = n, color = continent, stroke = status), inherit.aes = FALSE, alpha = .7) +
  coord_radial(inner.radius = .2, expand = TRUE, rotate.angle = TRUE) +
  guides(
    theta = guide_axis_theta(angle = 90),
    r     = guide_axis(angle = 0)
  ) +
  labs(x = NULL, y = NULL) +
  scale_y_continuous(labels = NULL ,expand = FALSE) +
  scale_color_manual(values = palette2) +
  scale_size_continuous(range = c(.5, 5)) +
  #labs(title = title, subtitle = subtitle, caption = caption) +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family = font),
        axis.text.x = element_blank(),
        plot.title.position = "plot")

test_plot

ggsave("C:/Users/wypyc/Desktop/roundabouts_test.png", test_plot, width = 8, height = 8)

