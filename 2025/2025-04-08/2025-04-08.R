library(tidyverse)
library(geofacet)
library(ggiraph)
library(showtext)

font_add_google("Poppins")
showtext_auto()

gdtools::register_gfont("Poppins")


font <- "Poppins"

care_stashowtextcare_state <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-08/care_state.csv')

pal<- c("#204632", "#719489","#307179",
                 "#579AA2", "#8a1f2f", "#ab3148", "#8A5133", "#C3834B", "#552b3f", "#7f4464")


## make a scrolly?
## make an interactive map?
## make some rank plot?

## idea 1:
#' a ggiraph plot showing ranks of states in various categories
#' 
#' 

care_state %>%
  filter(grepl("spent", measure_name)) %>%
  mutate(measure_name = str_remove(measure_name, "A lower number of minutes is better")) %>%
  group_by(measure_id) %>%
  arrange(score) %>%
  mutate(rank = row_number()) %>%
  ungroup() %>%
  summarise(mean_rank = mean(rank), .by = state) %>%
  arrange(desc(mean_rank))

p_base <- care_state %>%
  filter(grepl("spent", measure_name)) %>%
  mutate(measure_name = str_remove(measure_name, "A lower number of minutes is better")) %>%
  group_by(measure_id) %>%
  arrange(score) %>%
  mutate(rank = row_number()) %>%
  select(measure_name, state, score, rank) %>%
  ggplot(aes(x = measure_name, y = rank, color = measure_name)) +
  geom_point_interactive(aes(tooltip = paste0(measure_name, "; rank: ", rank))) +
  facet_geo(~ state)  +
  scale_x_discrete(labels = NULL) +
  scale_y_reverse(breaks = c(1, 20, 40)) +
  scale_color_manual(values = pal) +
  labs(x = NULL, y = NULL, title = "State Ranks in medicaid waiting time measures",
       subtitle = str_wrap("The plot shows ranks of each state in various waiting times. This includes waiting times for Psychiatric/Mental health patients, time before leaving from the visit, time before being sent home. Mississippi has the best average rank and DC has the worst average rank", 100),
       caption = "Tidytuesday 2025-04-08 | Data: Centers for Medicaid and Medicare services | Michał Wypych") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(family = font),
        axis.text = element_text(size = 3, family = font),
        plot.title = element_text(size = 13, family = font),
        plot.subtitle = element_text(size = 7, family = font),
        plot.caption = element_text(size = 2, family = font))

tooltip_css <- 'font-family:"Poppins";background-color:#94d2bd;color:black;padding:5px;border-radius:3px;'
plot_int <- girafe(
  ggobj = p_base,
  options = list(
    opts_tooltip(css = tooltip_css, opacity = 1),
    opts_sizing(width = .7)
  )
)

htmltools::save_html(plot_int, "state_medicaid.html")
