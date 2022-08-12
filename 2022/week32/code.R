## week 32 ##


library(tidyverse)
library(patchwork)
library(ggforce)
library(ggtext)
library(extrafont)
library(ggthemes)
library(hrbrthemes)
library(ggrepel)

#PLEASE NOTE YOU NEED ROBOTO FONT FOR HIS CODE TO RUN SMOOTHLY

wheels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-09/wheels.csv')



# getting the cost as number
wheels <- wheels %>%
  mutate(num_val = as.numeric(str_extract(construction_cost, pattern = "[0-9]+.?[0-9]*")))



# PLOT FOR HEIGHT AND COST
cost_to_height <- wheels %>%
  filter(!is.na(hourly_capacity)) %>%
  ggplot() +
  geom_point(aes(x=num_val, y = height),data = wheels %>% filter(country != "China", country != "USA", !is.na(hourly_capacity)), size = 7, alpha = .7) +
  geom_point(aes(x=num_val, y = height, color = country), data = wheels %>% filter(country %in% c("USA", "China"), !is.na(hourly_capacity)), size = 7, alpha = .7) +
  geom_text_repel(aes(x = num_val, y = height, label = name), segment.color = 'transparent',
                  family = "Roboto", color = "grey20", fontface = "bold",nudge_y = 20,
                  data = wheels %>% filter(!is.na(hourly_capacity) & num_val >= 200 & height >= 540)) +
  scale_y_continuous(breaks = c(100, 200, 300, 400, 500, 600), expand = c(0,0), limits = c(0, 750)) +
  scale_color_manual(values = c("#0a9396", "#780000", "#003049")) +
  theme_gdocs(base_family = "Roboto") +
  labs(x = "cost of ferris wheel in milions") +
  theme(legend.position = "none",
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        axis.title = element_text(color = "grey20", face = "bold"),
        axis.text = element_text(color = "grey20", face = "bold"),
        panel.grid.major = element_line(color = "grey40"))
cost_to_height


# PLOT FOT CAPACITY AND COST
cost_to_capacity <- wheels %>%
  filter(!is.na(height)) %>%
  ggplot() +
  geom_point(aes(x=num_val, y = hourly_capacity), data = wheels %>% filter(country != "China", country != "USA", !is.na(height)), size = 7, alpha = .7) +
  geom_point(aes(x=num_val, y = hourly_capacity, color = country), data = wheels %>% filter(country %in% c("USA", "China"), !is.na(height)), size = 7, alpha = .7) +
  geom_text_repel(aes(x = num_val, y = hourly_capacity, label = name), segment.color = 'transparent',
                  family = "Roboto", color = "grey20", fontface = "bold", nudge_y = 20,
                  data = wheels %>% filter(num_val >= 200 & hourly_capacity >= 2000)) +
  scale_color_manual(values = c("#0a9396", "#780000", "#003049")) +
  scale_y_continuous(breaks = c(0, 1000, 2000, 3000, 4000, 5000, 6000), expand = c(0,0), limits = c(0,7500)) +
  theme_gdocs(base_family = "Roboto") +
  labs(x = "cost of ferris wheel in milions", y = "hourly capacity") +
  theme(legend.position = "none",
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        axis.title = element_text(color = "grey20", face = "bold"),
        axis.text = element_text(color = "grey20", face = "bold"),
        panel.grid.major = element_line(color = "grey40"))
cost_to_capacity


# COMBINING
final <- cost_to_height + cost_to_capacity +
  plot_annotation(title = "Does more expensive mean better?",
                  subtitle = "More expensive ferris wheels are higher but not necessarily have higher capacity.<br>
                  The most expensive wheel is in the <span style = 'color:#780000'><b>USA</b></span> but the highest and with the largest capacity is in <span style = 'color:#0a9396'><b>China</b></span>",
                  caption = "Note: plots show only those ferris wheels for which there was data about cost, height and hourly capacity.<br>#Tidytuesday 2022 week 32 | data: ferriswheels R package",
                  theme = theme(
                    plot.title = ggtext::element_markdown(size = 40, color = "grey20", family = "Roboto"),
                    plot.subtitle = ggtext::element_markdown(size = 25, color = "grey20", family = "Roboto"),
                    plot.caption = ggtext::element_markdown(color = "grey20", family = "Roboto"),
                    plot.background = element_rect(color = "#fdf0d5", fill = "#fdf0d5"))
  )


final

# DEFINE YOUR PATH TO SAVE
path = "your_path"

ggsave(filename = "wheels.png", plot = final, path = path, dpi = 320, width = 20,height = 10, units = "in")

