library(tidyverse)
library(ggtext)
library(showtext)

font <- "Poppins"
font_add_google(font)
showtext_auto()

movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-29/movies.csv')
shows <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-29/shows.csv')




shows <- shows %>%
  mutate(time = str_remove_all(runtime, "[A-Z]")) %>%
  separate(time, into = c("hours", "minutes", "seconds")) %>%
  mutate(across(hours:seconds, as.numeric)) %>%
  mutate(hours = replace_na(hours, 0) *3600,
         minutes = replace_na(minutes, 0 ) * 60)

shows$runtime_sec <- rowSums(shows[,c("hours", "minutes", "seconds")], na.rm = T)

title <- "Did shows on Netflix get longer with time?\nNot really but they cap at 60 hours"

label_df <- data.frame(x = as.Date("2015-01-01"), y = 100, label = title)

final <- shows %>%
  mutate(runtime_hour = runtime_sec/60/60) %>%
  ggplot(aes(x = release_date, y = runtime_hour, color = runtime_hour > 60)) +
  geom_hline(yintercept = 60, linewidth = .6, color = "grey",alpha = .6, linetype = "dashed") +
  geom_point(alpha = .1) +
  geom_point(data = shows %>% 
               mutate(runtime_hour = runtime_sec/60/60) %>%
               filter(runtime_hour > 60), aes(x = release_date, y = runtime_hour), color = "red") +
  geom_text(data = label_df, aes(x = x, y = y, label = label), color = "#E50914", fontface = "bold",
                size = 15, family = font, lineheight = .4, inherit.aes = FALSE) +
  #annotate(geom = "label", x = as.Date("2020-01-01"), y = 150, label= long_shows, size = 5, family = font, lineheight = .4) +
  scale_y_continuous(limits = c(0, 120)) +
  scale_x_date(expand = c(0,0)) +
  scale_size_area(max_size = 3) +
  scale_color_manual(values = c("#e5e5e5", "#E50914")) +
  labs(x = "Release date", y = "runtime in hours") +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family = font),
        axis.title =  element_text(family = font, size = 20, face = "bold", color = "grey90"),
        axis.text = element_text(family = font, size = 17, face = "bold", color = "grey90"),
        plot.background = element_rect(fill = "#14213d", color = "#14213d"),
        panel.grid = element_blank())


ggsave("netflix.png", final, width = 8, height = 5, bg = "#14213d")


#add marginal distribution at the side?