library(tidyverse)
library(showtext)
library(ggforce)
library(grid)
library(patchwork)
library(readxl)
library(ggtext)
library(ggimage)

font <- "Ubuntu"
font_add_google(font)
showtext_auto()


#data on EU gas prices: https://energy.ec.europa.eu/document/download/906e60ca-8b6a-44e7-8589-652854d2fd3f_en?filename=Weekly_Oil_Bulletin_Prices_History_maticni_4web.xlsx

weekly_gas_prices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-01/weekly_gas_prices.csv')

eu_gas_prices <- read_excel("Weekly_Oil_Bulletin_Prices_History_maticni_4web.xlsx", sheet = 1, range = "A3:C1023")

eu_gas_prices$price_eu <- eu_gas_prices$`1000 l`/1000


my_theme <- function() {
  theme_minimal(base_size = 15, base_family = font) +
    theme(strip.background = element_rect(fill = "grey80", color = "grey80"),
          plot.title = element_text(size = 25, family = font, face = "bold"),
          plot.subtitle = element_text(size = 15, family = font, color = "grey40", lineheight = .4),
          axis.text = element_text(family = font, face = "bold", color = "grey20"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "grey90", linewidth = .2),
          plot.title.position = "plot")
}


title <- "US and EU gasoline weekly prices after the Russian invasions on Ukraine"
subtitle <- str_wrap("Russia attacked Ukraine twice in the past decade. The first attack came in 2014 with annexation of Crimea and parts of Donetsk and Luhansk and the second time in 2022 with attacks on Kiev, Kharkov, Eastern and Southern Ukraine. 
Following Russia's annexation of Crimea in early 2014, global oil markets remained relatively stable.
                     U.S. gasoline prices gradually declined throughout the year.
                     In the EU, prices were more stable but trended slightly downward as well. 
                     Russia's February 2022 invasion of Ukraine caused a sharp spike in global oil prices, driven by fears of supply disruptions and sanctions on Russian exports. 
                     U.S. gasoline prices rose rapidly, peaking in mid-2022 at record highs. 
                     In the EU, the impact was even more severe, especially in countries reliant on Russian energy, with fuel prices surging sharply due to both crude oil and natural gas disruptions.
                     Be careful with comparisons of prices between EU and US as they are on different scales (dollars vs euro)! This plot mainly shows comparisons of trends and not of prices!",
                     200)

us_max_price <- weekly_gas_prices %>%
  filter(formulation == "all", grade == "regular", fuel == "gasoline", date > as.Date("2012-12-01")) %>%
  slice_max(price, n = 1)

us_flag_df <- data.frame(
  x = as.Date("2013-01-01"),
  y = 3.4, 
  image = "https://upload.wikimedia.org/wikipedia/commons/thumb/a/a4/Flag_of_the_United_States.svg/1200px-Flag_of_the_United_States.svg.png")


plot_us <- weekly_gas_prices %>%
  filter(date > as.Date("2012-12-01")) %>%
  filter(formulation == "all", grade == "regular", fuel == "gasoline", date > as.Date("2012-12-01")) %>%
  ggplot(aes(x = date, y = price)) +
  geom_ribbon(data = weekly_gas_prices %>% filter(formulation == "all", grade == "regular", fuel == "gasoline", date > as.Date("2014-02-20")),
            aes(x = date, ymin = 1, ymax = price), fill = "#c9184a", alpha = .6) +
  geom_ribbon(data = weekly_gas_prices %>% filter(formulation == "all", grade == "regular", fuel == "gasoline", date > as.Date("2022-02-24")),
            aes(x = date, ymin = 1, ymax = price), fill = "#800f2f", alpha = .5) +
  geom_line() +
  geom_segment(x = as.Date("2014-02-20"), y = 0, yend = 4, color = "firebrick4", linetype = "dashed") +
  geom_segment(x = as.Date("2022-02-24"), y = 0, yend = 5.1, color = "firebrick4", linetype = "dashed") +
  annotate(geom = "text", x = as.Date("2014-02-20"), y = 4.5, label = "Russia annexation\nof Crimea", family = font, size = 4, fontface = "bold", lineheight = .4) +
  annotate(geom = "text", x = as.Date("2022-02-24"), y = 5.5, label = "Russia full scale invasion\nof Ukraine", family = font, size = 4, fontface = "bold", lineheight = .4) +
  geom_text(data = us_max_price, aes(x = date, y = price, label = "prices spiked on 13th June 2022 at $5.01"), family = font, size = 4, nudge_x = 350, nudge_y = -.1) +
  geom_image(data = us_flag_df, aes(x = x, y = y, image = image), size = .2) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = c(as.Date("2013-01-01"), as.Date("2025-06-23"))) +
  scale_y_continuous(labels = scales::label_currency(prefix = "$", accuracy = .5), expand = c(0,0), limits = c(1, 5.9)) +
  labs(title = title, subtitle = subtitle, x = NULL, y = NULL) +
  my_theme()


eu_max_price <- eu_gas_prices %>%
  filter(Date > as.Date("2012-12-01")) %>%
  slice_max(price_eu,n= 1)


eu_flag_df <- data.frame(
  x = as.Date("2013-01-01"),
  y = 1.5,
  image = "https://upload.wikimedia.org/wikipedia/commons/thumb/b/b7/Flag_of_Europe.svg/330px-Flag_of_Europe.svg.png"
)

eu_plot <- eu_gas_prices %>%
  filter(Date > as.Date("2012-12-01")) %>%
  ggplot(aes(x = as.Date(Date, tz=""), y = price_eu)) +
  geom_ribbon(data =eu_gas_prices %>% filter(Date > as.Date("2014-02-20")),
              aes(x = as.Date(Date, tz=""), ymin = 1, ymax = price_eu), fill = "#c9184a", alpha = .6) +
  geom_ribbon(data =eu_gas_prices %>% filter(Date > as.Date("2022-02-24")),
              aes(x = as.Date(Date, tz=""), ymin = 1, ymax = price_eu), fill = "#800f2f", alpha = .5) +
  geom_segment(x = as.Date("2014-02-20"), y = 0, yend = 2, color = "firebrick4", linetype = "dashed") +
  geom_segment(x = as.Date("2022-02-24"), y = 0, yend = 2.1, color = "firebrick4", linetype = "dashed") +
  geom_line() +
  geom_text(data = eu_max_price, aes(x = as.Date(Date, tz=""), y = price_eu, label = "prices spiked on 13th June 2022 at €2.03"), family = font, size = 4, nudge_x = 350, nudge_y = -.05) +
  geom_image(data = eu_flag_df, aes(x = x, y = y, image = image), size = .2) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = c(as.Date("2013-01-01"), as.Date("2025-06-23"))) +
  scale_y_continuous(labels = scales::label_currency(prefix = "€", accuracy = .1), expand = c(0,0), limits = c(1,2.2)) +
  labs(x = NULL, y = NULL) +
  my_theme()

final <- plot_us / eu_plot + plot_annotation(caption = "Tidytuesday 2-25-07-01 | data US: U.S. Energy Information Administration; data EU: EU Weekly Oil Bulletin | Michał Wypych")

ggsave("gasoline_ukraine.png", final, width = 8, height = 4, bg = "white")

