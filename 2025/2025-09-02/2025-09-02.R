library(tidyverse)
library(ggforce)
library(showtext)

frogID_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-02/frogID_data.csv')
frog_names <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-02/frog_names.csv')

# f-ot

font <- "Poppins"
font_add_google(font)
showtext_auto()

#trying on a small subset
#try the most prolific recorder?

top_recorders <- frogID_data %>%
  count(recordedBy) %>%
  slice_max(n, n = 5) %>%
  pull(recordedBy)


lag_data <- frogID_data %>%
 # filter(recordedBy %in% top_recorders) %>%
  inner_join(frog_names, by = "scientificName") %>%
  group_by(tribe, recordedBy) %>%
  mutate(event_datetime = ymd_hms(paste(eventDate, eventTime))) %>%
  dplyr::arrange(event_datetime) %>%
  mutate(event_datetime_lag = lead(event_datetime)) %>%
  select(tribe, event_datetime, event_datetime_lag) %>%
  distinct(event_datetime_lag, .keep_all = T) %>%
  ungroup() %>%
  mutate(row = 1,
         y = cumsum(row), .by = c("tribe", "recordedBy"))



#titles and stuff

title <- "Australia's most active frog spotters"

subtitle <- str_wrap("Citizen scientists across Australia contribute frog call recordings to FrogID,
a nationwide project tracking the continent’s unique and threatened frog fauna. This plot
highlights frog tribes and the five most prolific recorders (IDs: 35320, 5742, 35717, 12664, 41581),
with each ‘hop’ representing the time gap between consecutive recordings. 
                     Y axis shows cumulative number of frogs from a given tribe spotted", 100)
caption <- "Tidytuesday 2025-09-02 | data from FrogID data | Michał Wypych"

test_1.1 <-
  lag_data %>% drop_na() %>% arrange(desc(event_datetime)) %>%
  ggplot() +
  # arcs
  geom_curve(
    aes(x = event_datetime, xend = event_datetime_lag, y = y, yend = y + 1),
    curvature = -0.01,
    ncp = 5,         
    alpha = 0.1,
    linewidth = 1
  ) +
  geom_curve(
    data = lag_data %>% drop_na() %>%filter(recordedBy %in% top_recorders) %>% arrange(desc(event_datetime)),
    aes(x = event_datetime, xend = event_datetime_lag, y = y, yend = y + 1, color = factor(recordedBy)),
    curvature = -0.5,
    ncp = 5,         
    alpha = 0.8,
    linewidth = 1
  ) +
  geom_text(aes(x = as_datetime("2023-02-01 00:00:01"), y = 600, label = tribe),
            family = font, size = 9, inherit.aes= FALSE) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%B") +
  scale_color_manual(values = c("#3c1642", "#086375", "#1dd3b0","#16db65", "#affc41")) +
  scale_y_continuous(position = "right") +
  guides(color = "none") +
  facet_grid(rows = vars(tribe), scales = "free_x") +
  labs(title = title, subtitle = subtitle, caption = caption) +
  theme_void(base_size = 10, base_family = font) +
  theme(axis.text.x = element_text(family = font, size = 20),
        axis.text.y = element_text(family = font, size = 20),
        strip.text = element_blank(),
        panel.grid.major.y = element_line(color = "grey80", linetype = "dashed"),
        plot.title.position = "plot",
        plot.title = element_text(family = font, size = 60),
        plot.subtitle = element_text(family = font, size = 20, lineheight = .4),
        plot.margin = margin(10,10,10,10))




ggsave("C:/Users/wypyc/Desktop/test2.png", test_1.1, width = 10, height = 10, bg = "white")


