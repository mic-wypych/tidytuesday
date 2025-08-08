library(tidyverse)
library(geofacet)
library(showtext)
library(ggforce)

font <- "Inter"
font_add_google(font)
showtext_auto()


income_inequality_processed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-08-05/income_inequality_processed.csv')
income_inequality_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-08-05/income_inequality_raw.csv')



lines <- data.frame(
  x = c(1989, 1999, 2004, 2008, 2020),
  y = c(.2, .23, .15, .2, .2),
  xend = c(1989, 1999, 2004, 2008, 2020),
  yend = c(.27,.29, .32, .31, .29),
  label = c("1989\nend of communism", "1999\nPoland joins NATO", "2004\nPoland joins EU", "2008\nfinancial crisis", "2020\ncovid pandemic")
  
)

legend_data <- data.frame(x =c(1986.5, 1986.5, 1986.5, 1986.5),
                          y = c(.2,.25, .3, .35),
                          label = c("- .20","- .25", "- .30", "- .35"))

caption <- "Tidytuesday 2025-08-05 | data from Our World in Data | Michał Wypych"

final <- income_inequality_processed %>%
  filter(Entity == "Poland") %>%
  ggplot(aes(x = Year, y = gini_dhi_eq)) +
  geom_area(fill = "#0a0908") +
  annotate(geom = "text", x = 2000, y = .9, label = "Gini coefficient in Poland from 1986 to 2023",
           color = "#0a0908", size = 20, family = font, fontface = "bold") +
  annotate(geom = "text", x = 2018.5, y = .01, label = caption,
           color = "firebrick4", size = 4, family = font, fontface = "bold") +
  geom_text(data = legend_data2, aes(x = x, y = y, label = label, color = y > .27), family = font, fontface = "bold", size = 7) +
  geom_link(data = lines, aes(x = x, y = y, xend = xend, yend = yend,
                alpha = stat(index), size = after_stat(index)), color = "firebrick4") +
  geom_text(data = lines, aes(x = x, y = y - .03, label = label),
            family = font, size = 10, lineheight = .3, color = "firebrick4", fontface = "bold") +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_size_continuous(range = c(1,3)) +
  scale_color_manual(values = c("firebrick4", "#0a0908")) +
  theme_void() +
  theme(panel.background = element_rect(fill = "firebrick4"),
        legend.position = "none")


ggsave("gini3.png", final, width =8, height = 8)  
