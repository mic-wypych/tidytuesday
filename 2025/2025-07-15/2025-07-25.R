library(tidyverse)
library(ggtext)
library(showtext)

font <- "Neuton"
font_add_google(font)
showtext_auto()


bl_funding <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-15/bl_funding.csv')

palette <- c("#5f0f40", "#9a031e", "#ff4d6d", "#e36414", "#0f4c5c")
#moke some simple but well styled plot

subtitle <- 'funding of British library adjusted to 2000 GBP has been steadily dropping since the peak in 2006.<br>
Funding  was highest in 2006 with total funds of 144.77 mln£. It went down to 83.46 mln£ in 2023. <br>The funding for all categories: <span style="color: #5f0f40"> GIA </span>, <span style="color: #9a031e"> investment </span>,
<span style="color: #ff4d6d"> other </span>, <span style="color: #e36414"> services </span>, <span style="color: #0f4c5c"> voluntary </span> dropped.<br> Biggest drop was recorded in service funding closely followed by GIA and voluntary'


max_fund <- bl_funding %>%
  pivot_longer(cols = gia_y2000_gbp_millions:other_y2000_gbp_millions,
               names_to = "type", values_to = "value") %>%
  mutate(type = str_remove(type, "_y2000_gbp_millions")) %>%
  slice_max(value, n = 1, by = type) %>%
  select(year, type, value)

funding_2025 <- bl_funding %>%
  pivot_longer(cols = gia_y2000_gbp_millions:other_y2000_gbp_millions,
               names_to = "type", values_to = "value") %>%
  mutate(type = str_remove(type, "_y2000_gbp_millions")) %>%
  filter(year == 2023) %>%
  select(year, type, value)

fund_change <- max_fund %>%
  inner_join(funding_2025, by = "type") %>%
  arrange(type) %>%
  mutate(year.y = c(2024, 2025.5, 2024.5,2025, 2023.5),
         diff = value.y - value.x) %>%
  mutate(pos = mean(c(value.y, value.x)), .by = type)

final <- bl_funding %>%
  pivot_longer(cols = gia_y2000_gbp_millions:other_y2000_gbp_millions,
               names_to = "type", values_to = "value") %>%
  mutate(type = str_remove(type, "_y2000_gbp_millions")) %>%
  ggplot(aes(x = year, y = value, color = type)) +
  geom_area(aes(fill = type), alpha = .2, linewidth = .5, position = "identity") +
  geom_segment(data = fund_change, aes(x = year.x, xend = year.y, y = value.x, color = type),
               position = "identity", linetype = "dashed", linewidth = .3) +
  geom_segment(data = fund_change, aes(x = 2023, xend = year.y, y = value.y,  color = type),
               position = "identity", linetype = "dashed", linewidth = .3) +
  geom_segment(data = fund_change, aes(x = year.y, y = value.x, yend = value.y, color = type),
               position = "identity", linetype = "dashed", linewidth = .3) +
  geom_text(data = fund_change, aes(x = year.y + .7, y = pos, label = paste(round(diff, 1), "mln£")), 
            inherit.aes = FALSE, family = font, size = 6) +
  scale_x_continuous(breaks = 1998:2023, expand = c(0,0), limits = c(1998, 2028)) +
  scale_color_manual(values = palette) +
  scale_fill_manual(values = palette) +
  labs(title = "Change in funding of British library from 1998 to 2023",
       subtitle = subtitle,
       x = NULL,
       y = "mln£") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linetype = "dashed", linewidth = .2),
        plot.subtitle = element_markdown(family = font, size = 25, lineheight = .4),
        plot.title = element_text(family = font, size = 35),
        plot.title.position = "plot",
        axis.text = element_text(family = font, size = 20),
        axis.title = element_text(family = font, size = 20, face = "bold"),
        legend.position = "none")


ggsave("C:/Users/wypyc/Desktop/b_lob.png", final, width = 8, height = 4, bg = "white")

