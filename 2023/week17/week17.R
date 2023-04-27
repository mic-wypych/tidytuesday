library(tidyverse)
library(showtext)
library(gganimate)
library(ggchicklet)

font_add_google("Oswald")
showtext_auto()

winners <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-25/winners.csv')
london_marathon <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-25/london_marathon.csv')

#main plot
times <- winners %>%
  filter(Category %in% c("Men", "Women")) %>%
  ggplot(aes(x = Time, xend = Time, y = 0, yend = 10)) +
  geom_segment(linewidth = 1) +
  geom_rect(aes(xmin = 0, xmax = Time, ymin = 0, ymax =10, fill = Category), alpha = .4) +
  geom_rect(aes(xmax = 43200, xmin = 0, ymin = 9.9, ymax = 11), fill = "grey20") +
  geom_segment(aes(x=0,xend=0,y=10,yend = 12), linewidth = 2, color = "grey20") +
  geom_segment(aes(x=0,xend=0,y=11.5,yend = 13), linewidth = 5, color = "grey40") +
  geom_segment(aes(x=3600,xend=3600,y=10,yend = 11.5), linewidth = 2, color = "grey20") +
  geom_segment(aes(x=3600,xend=3615,y=11.5,yend = 12), linewidth = 3, color = "grey40") +
  geom_segment(aes(x=39600,xend=39600,y=10,yend = 11.5), linewidth = 2, color = "grey20") +
  geom_segment(aes(x=39600,xend=39615,y=11.5,yend = 12), linewidth = 3, color = "grey40") +
  geom_text(aes(x = 21600, y = 1, label = Time), size = 10, family = "Oswald", vjust = 1) +
  scale_x_time(limits = c(0,43200), labels = NULL) +
  scale_fill_manual(values = c("firebrick4", "navy")) +
  coord_polar() +
  facet_wrap(~Year + Category) +
  theme_void() +
  labs(y = NULL,x = NULL, title = "Winning times in London Marathon",
       caption = "#Tidytuesday 2023 Week 17 | data @nrennie35 | @michal_wypych") +
  theme(axis.text.y = element_blank(),
        legend.position = "none",
        plot.background = element_rect(fill = "white", color = "white"),
        strip.text = element_text(size = 20, family = "Oswald"),
        plot.caption = element_text(size = 20, color = "grey20", family = "Oswald"),
        plot.title = element_text(size = 60, hjust = .5, family = "Oswald", margin = margin(t = 10, b = 10)))

  
ggsave("C:/Users/User/Documents/GitHub/tidytuesday/2023/week17/week17.png", times, width = 10, height = 10)


#wheelchair marathon

times2 <- winners %>%
  filter(Category %in% c("Wheelchair Men", "Wheelchair Women")) %>%
  ggplot(aes(x = Time, xend = Time, y = 0, yend = 10)) +
  geom_segment(linewidth = 1) +
  geom_rect(aes(xmin = 0, xmax = Time, ymin = 0, ymax =10, fill = Category), alpha = .4) +
  geom_rect(aes(xmax = 43200, xmin = 0, ymin = 9.9, ymax = 11), fill = "grey20") +
  geom_segment(aes(x=0,xend=0,y=10,yend = 12), linewidth = 2, color = "grey20") +
  geom_segment(aes(x=0,xend=0,y=11.5,yend = 13), linewidth = 5, color = "grey40") +
  geom_segment(aes(x=3600,xend=3600,y=10,yend = 11.5), linewidth = 2, color = "grey20") +
  geom_segment(aes(x=3600,xend=3615,y=11.5,yend = 12), linewidth = 3, color = "grey40") +
  geom_segment(aes(x=39600,xend=39600,y=10,yend = 11.5), linewidth = 2, color = "grey20") +
  geom_segment(aes(x=39600,xend=39615,y=11.5,yend = 12), linewidth = 3, color = "grey40") +
  geom_text(aes(x = 21600, y = 1, label = Time), size = 10, family = "Oswald", vjust = 1) +
  scale_x_time(limits = c(0,43200), labels = NULL) +
  scale_fill_manual(values = c("#ae2012", "#005f73")) +
  coord_polar() +
  facet_wrap(~Year + Category) +
  theme_void() +
  labs(y = NULL,x = NULL, title = "Finishing times of winners of London Marathon",
       caption = "#Tidytuesday 2023 Week 17 | data @nrennie35 | @michal_wypych") +
  theme(axis.text.y = element_blank(),
        legend.position = "none",
        plot.background = element_rect(fill = "white", color = "white"),
        strip.text = element_text(size = 20, family = "Oswald"),
        plot.caption = element_text(size = 20, color = "grey20", family = "Oswald"),
        plot.title = element_text(size = 60, hjust = .5, family = "Oswald", margin = margin(t = 10, b = 10)))


ggsave("C:/Users/User/Documents/GitHub/tidytuesday/2023/week17/week17_2.png", times2, width = 10, height = 10)

