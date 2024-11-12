library(tidyverse)
library(ggridges)
library(showtext)
library(geofacet)

font_add_google("Asap Condensed")
theme_set(theme_minimal(base_family = "Asap Condensed", base_size = 40))
theme_update(
  plot.title.position = "plot",
  plot.title = element_text(size = 50),
  panel.grid.minor = element_blank(),
  plot.subtitle = element_text(lineheight = .3)
)


house <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-11-07/house.csv')

house$percent <- house$candidatevotes/house$totalvotes

#make comparison in a number of swing states?

#' subtitle: swong states are considered real battlegrounds where neither party
#' can be certain to win. The plot shows democrat and republican shares of votes in districts in swing states
#' from 1976 to 2022 in House elections.
#' add some interpretation


#' to do:
#'
subtitle <- "North Carolina is consideted to be one of the swing states in the US\nThe plot shows distributions of percentage fo votes from 1976 to 2022\nin districts of North Carolina."

georgia_plot<- house %>%
  filter(state %in% c("NORTH CAROLINA"), party %in% c("REPUBLICAN", "DEMOCRAT")) %>%
  ggplot(aes(x = percent, y = as_factor(year), fill = as.factor(party))) +
  ggridges::geom_density_ridges(alpha = .4, color = "white") +
  scale_x_continuous(limits = c(0,1), labels = scales::label_percent(), expand = c(0,0)) +
  scale_fill_manual(values = c("DEMOCRAT" = "#1d3557", "REPUBLICAN" = "#c1121f")) +
  labs(title = "North Carolina votes across the years", y = NULL, x = "vote share",
       subtitle = subtitle) +
  theme(legend.title = element_blank(),
        legend.position = "top",
        plot.background = element_rect(fill = "white"))
  
ggsave(georgia_plot, filename =  "C:/Users/User/Desktop/georgia.png", width = 8, height = 10)
  
  
#### lets try that with geofacet?? -----

house %>%
  filter(state %in% c("GEORGIA"), party %in% c("REPUBLICAN", "DEMOCRAT")) %>%
  ggplot(aes(x = percent, y = as_factor(year), fill = as.factor(party))) +
  ggdist::stat_halfeye(.width = 0, slab_alpha = .5, shape = 21) +
  scale_x_continuous(limits = c(0,1), labels = scales::label_percent()) +
  scale_fill_manual(values = c("DEMOCRAT" = "#1d3557", "REPUBLICAN" = "#c1121f"), name = NULL) +
  labs(title = "Swing states votes across the years", y = NULL, x = "vote share") +
  labs(title = "Texas House votes across the years")

