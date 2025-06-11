library(tidyverse)
library(showtext)
library(readxl)


font <- "Montserrat"

font_add_google(font)
showtext_auto()


judges_appointments <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-10/judges_appointments.csv')
judges_people <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-10/judges_people.csv')


uspres <- read_csv("GitHub/tidytuesday/2025/2025-06-10/us_presidents.csv")

uspres$start <- as_date(uspres$`Took office`, format = "%m/%d/%Y")
uspres$end <- as_date(uspres$`Left office`, format = "%m/%d/%Y")

uspres <- uspres %>%
  rowwise() %>%
  mutate(mid = mean(c(start, end), na.rm = T))


title <- "US Judge appointments by president and party"
subtitle <- str_wrap("Presidents have the right to appoint judges in the US 
                     based on the Article II, Section 2 of the U.S. Constitution. 
                     The plot shows nominations from Washington to Obama and 
                     the parties that the presidents are from. First half of 
                     XIX Century has seen much less appointments than later periods.
                     Ronald Reagan appointed the most judges (in both terms combined) 
                     - 387, closely followed by Bill Clinton (383), 
                     George W. Bush (328), Barack Obama (281) and Jimmy Carter (267)", 150)
caption <- "Tidytuesday 2025-06-10 | data on juges from {historydata}; data on US president terms from Kaggle US president dataset | Michał Wypych "


final <- judges_appointments %>%
  filter(president_party %in% c("Democratic", "Republican", "Federalist", "Jeffersonian Republican", "Whig")) %>%
  ggplot() +
  geom_linerange(aes(x = as_date(nomination_date, format = "%m/%d/%Y"),
                     ymin = 1, ymax = 2, color = president_party), alpha = .2) +
  geom_text(data = uspres, aes(x = mid, y = 2.1, label = President), 
            angle = 90, size = 3, hjust = 0, family = font) + 
  geom_linerange(data = uspres, aes(x = start, ymin = 2, ymax = 2.5), linetype = "dashed", linewidth = .2, alpha = .3) +
  geom_linerange(data = uspres, aes(x = end, ymin = 2, ymax = 2.5), linetype = "dashed", linewidth = .2, alpha = .3) +
  scale_color_manual(values = c("Democratic" = "navyblue",
                                "Republican" = "firebrick4",
                                "Federalist" = "#ffb627",
                                "Jeffersonian Republican" = "#3d2645",
                                "Whig" = "#832161")) +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y", expand = c(0,0),
               limits = c(as_date("1780-01-01"),as_date("2017-01-01"))) +
  scale_y_continuous(expand = c(0,0), limits = c(1,3)) +
  coord_cartesian(xlim = c(as_date("1790-01-01"),as_date("2020-01-01")), ylim = c(1, 2.8)) +
  labs(title = title, subtitle = subtitle, caption = caption, x = NULL) +
  theme_classic() +
  guides(color = guide_legend(override.aes = list(linewidth = 1, alpha = 1))) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 15, family = font),
        legend.justification='left',
        legend.key.height = unit(3, 'mm'),
        legend.spacing = unit(3, 'mm'),
        panel.grid = element_blank(),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(family = font, size = 12),
        axis.line.x = element_line(linewidth = .2),
        axis.ticks.x = element_blank(),
        text = element_text(family = font),
        plot.title = element_text(size = 35, family = font, face = "bold"),
        plot.subtitle = element_text(size = 15, family = font, lineheight = .5))




ggsave("C:/Users/wypyc/Desktop/judges.png", final, width = 8, height = 3)

