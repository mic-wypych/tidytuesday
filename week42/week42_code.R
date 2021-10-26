#### week 42 tidytuesday####

#libraries
library(tidytuesdayR)
library(tidyverse)
library(ggdark)
library(ggtext)
library(extrafont)

#data
fishery <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/global-fishery-catch-by-sector.csv')

#you need roboto font family
font_import("C:/Users/user/AppData/Local/Microsoft/Windows/Fonts")
loadfonts(device = "win")

#prepare the dataset and recode some names
fish.data <- fishery %>%
  pivot_longer(cols = c("Artisanal (small-scale commercial)":"Subsistence"), names_to = "type", values_to = "amount") %>%
  mutate(amount = amount/1000000)
fish.data$type <- recode(fish.data$type, "Artisanal (small-scale commercial)" = "Artisanal", "Industrial (large-scale commercial)" = "Industrial")


#plot
fish.plot <- fish.data %>%
  ggplot() +
  geom_area(aes(x = Year, y = amount, fill = fct_reorder(type, desc(amount))), position = position_stack()) +
  scale_fill_brewer(palette = "PuBuGn") +
  geom_vline(xintercept = 1958, color = "grey20", linetype = "dashed") +
  geom_vline(xintercept = 1996, color = "grey20", linetype = "dashed") +
  annotate("label", x = 1970, y = 50.520801, label = "sharp rise in industrial\n fishing from 1959", color = "grey10", fill = "grey60", size = 8, family = "Roboto Light") +
  annotate("label", x = 1988, y = 90, label = "industrial fishing \nstarted to decline from 1996?", color = "grey10", fill = "grey60", size = 8, family = "Roboto Light") +
  annotate("label", x = 1970, y = 120, label = "One of the reasons behind rapid growth\nin industrial fishing may be the introductionof nylon nets.\nSynthethic fibers were introduced in the 1960s\nand banned in 1993", color = "grey10", fill = "grey60", size = 8, family = "Roboto Light") +
  geom_curve(
    aes(x = 1966, y = 50.520801, xend = 1958, yend = 43.000000),
    data = fishery,
    arrow = arrow(length = unit(0.03, "npc")), color = "black", curvature = -.1) +
  geom_curve(
    aes(x = 1993, y = 94.500000, xend = 1996, yend = 129.000000),
    data = fishery,
    arrow = arrow(length = unit(0.03, "npc")), color = "black", curvature = .2) +
  scale_x_continuous(limits = c(1950,2010), breaks = c(1950, 1960, 1970, 1980, 1990, 2000, 2010)) +
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100,110,120)) +
  ggdark::dark_theme_classic() +
  labs(x = "", y = "amount of fish\nin million", title = "1959-1996: the golden era of industrial fishing?", 
       subtitle = "From 1959 to 1996 we have seen the sharpest rise in industrial fishing in the world", caption = "Visualization: Michał Wypych | Data from: OurWorldinData.org", family = "Roboto") +
  theme(legend.title = element_blank(),
        legend.text = element_text(family = "Roboto", size = 30),
        plot.title = element_text(family = "Roboto", size = 40),
        plot.subtitle = element_text(family = "Roboto", size = 35),
        axis.text = element_text(family = "Roboto", size = 20),
        axis.title = element_text(family = "Roboto", size = 20),
        plot.caption = element_text(family = "Roboto", size = 15)) +
  theme(plot.background = element_rect(fill = "grey20", color = NA),
        legend.background = element_rect(fill = "grey20", color = NA),
        panel.background = element_rect(fill = "grey20", color = NA))

#exporting the file

#set your working directory to save the file
setwd()

#save the file
ggsave(filename = "seafood.jpg", plot = fish.plot, path = "C:/Users/user/Desktop/dataviz exercise/Tidytuesday/2021/week 42", dpi = 320, width = 30,height = 15, units = "in")
