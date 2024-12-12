# tidytuesday sankey with changes in regimes across time

 


library(tidyverse)
library(ggsankey)
library(ggtext)
library(showtext)
library(patchwork)
data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-05/democracy_data.csv')

font_add_google("Oswald")
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)
showtext_auto(enable = TRUE)

glimpse(data)


data_regimes <- data %>%
  select(country_name, year, is_monarchy, is_democracy, is_interim_phase,
         is_communist) %>%
  mutate(regime = case_when(
    is_monarchy == TRUE ~ "monarchy",
    is_democracy == TRUE ~ "democracy",
    is_interim_phase == TRUE ~ "interim",
    is_communist == TRUE ~ "communist",
    TRUE ~ NA
  )) %>%
  select(country_name, year, regime)


title <- "Regime changes across decades\na tale about missing data"
subtitle <- "The plot shows changes in regime types (<span style = 'color:#ef233c;'>communist</span>, <span style = 'color:#d5f2e3;'>democracy</span>, <span style = 'color:#2c6e49;'>monarchy</span>, <span style = 'color:#a7c957;'>interim</span> and <span style = 'color:#73ba9b;'>other/no data</span>)<br>from 1950 to 2020. We can see that monarchies largely stayed unchanged.There were two moments in XXth Century<br>when many communist countries turned democratic: 1989 and 1999. For many countries we do not have data because <br>either they did not exist in a given year or we lack information on their regime. Many countries ceased to exist<br>or appeared in XXth Century (especially due to decolonization and fall of the Soviet block) the other/no data category is so large.<br>However, there is clearly much more missing data from continents like Africa or Americas."
caption <- "Data from {democracyData} | @michal_wypych"


countrycodes <- countrycode::codelist_panel %>%
  select(country.name.en, continent) %>%
  distinct()


get_continent_sankey <- function(cont) {
  data_regimes %>%
    filter(year %in% c(1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020)) %>%
    inner_join(countrycodes, by = c("country_name" = "country.name.en")) %>%
    pivot_wider(id_cols = c(country_name, continent), names_from = "year", values_from = "regime") %>%
    filter(continent == cont) %>%
    mutate_all(~replace(., is.na(.), "no information")) %>%
    make_long(`1950`, `1960`, `1970`, `1980`, `1990`, `2000`, `2010`, `2020`) %>%
    ggplot(aes(x = x, 
               next_x = next_x, 
               node = node, 
               next_node = next_node,
               fill = factor(node))) +
    geom_sankey() +
    scale_fill_manual(drop=FALSE, values = c("#ef233c", "#d5f2e3", "#2c6e49", "#a7c957", "#73ba9b")) + #communist, democracy, interim, monarchy, other
    labs(title = cont, x = NULL, y = NULL) +
    theme_sankey(base_size = 5) +
    theme(panel.grid.major.x = element_line(color = "grey90", linetype = "dashed")) +
    theme(text = element_text(family = "Oswald", color = "grey90"),
          plot.background = element_rect(fill = "grey10", color = "grey10"),
          panel.background = element_rect(fill = "grey10"),
          legend.position = "none",
          plot.title = element_markdown(color = "grey80", size = 10, 
                                        face = "bold", family = "Oswald"),
          plot.subtitle = element_markdown(color = "white", family = "Oswald"),
          plot.caption = element_text(color = "white"),
          plot.caption.position = "plot",
          axis.text.x = element_text(color = "grey90", family = "Oswald"))
}

sankey_europe <- get_continent_sankey("Europe")
sankey_asia<- get_continent_sankey("Asia")
sankey_africa <- get_continent_sankey("Africa")
sankey_americas <- get_continent_sankey("Americas")
sankey_oceania <- get_continent_sankey("Oceania")
empty <- plot_spacer() + theme(plot.background = element_rect(fill = "grey10", color = "grey10"), panel.background = element_rect(fill = "grey10"))

final <- (sankey_asia + sankey_africa + sankey_oceania)/(sankey_americas + sankey_europe +empty) +
  plot_annotation(title = title, subtitle = subtitle, caption = caption, theme = theme(text = element_text(family = "Oswald", color = "grey90"), plot.background = element_rect(fill = "grey10", color = "grey10"), panel.background = element_rect(fill = "grey10"),legend.position = "none",plot.title = element_markdown(color = "grey80", size = 15, face = "bold", family = "Oswald"),plot.subtitle = element_markdown(color = "white", family = "Oswald", size = 8), plot.caption = element_text(color = "white", size = 3), plot.caption.position = "plot",axis.text.x = element_text(color = "grey90", family = "Oswald")))

ggsave("C:/Users/User/Desktop/sankey.png", final, width = 8, height = 4)

