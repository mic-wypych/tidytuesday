library(tidyverse)
library(ggridges)
library(ggbump)
library(patchwork)
library(showtext)
library(ggrepel)
font_add_google("Sriracha")
showtext_opts(dpi=300)
showtext_auto(enable=T)

font <- "Sriracha"

pokemon_df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-01/pokemon_df.csv')



expanded_df <- pokemon_df %>%
  drop_na(generation_id) %>%
  group_by(pokemon) %>%
  complete(generation_id = full_seq(1:7, 1)) %>%
  fill(defense, .direction = "down") %>% 
  fill(attack, .direction = "down") %>%
  fill(hp, .direction = "down") %>%
  fill(color_1, .direction = "down") %>%
  fill(type_1, .direction = "down") %>%
  ungroup() %>%
  group_by(generation_id) %>%
  arrange(desc(defense)) %>%
  mutate(Rank_df = row_number()) %>%
  arrange(desc(attack)) %>%
  mutate(Rank_att = row_number()) %>%
  arrange(desc(hp)) %>%
  mutate(Rank_hp = row_number()) %>%
  ungroup() %>%
  filter(Rank_df <= 5 | Rank_att <= 5 | Rank_hp <= 5) %>%
  mutate(FirstGen = min(generation_id), .by = pokemon)


p_hp <- expanded_df %>%
  filter(Rank_hp <= 5) %>%
  ggplot(aes(x = generation_id, y = Rank_hp, color = type_1, group = pokemon)) +
  geom_bump(linewidth = 1, alpha = .5) +
  geom_point(size = 3, alpha = .7) +
  geom_text_repel(data = expanded_df %>% filter(generation_id == FirstGen, Rank_hp <= 5),
            aes(label = pokemon),
            size = 3, family = font, seed = 2137, nudge_y = .1) +
  scale_y_reverse() +  # Reverse so Rank 1 is on top
  scale_x_continuous(breaks = 1:7) +
  scale_color_manual(values = setNames(expanded_df %>% filter(generation_id == FirstGen, Rank_hp <= 5) %>% pull(color_1),
                                       expanded_df %>% filter(generation_id == FirstGen, Rank_hp <= 5) %>% pull(type_1))) +
  theme_minimal() +
  labs(title = "HP",
       x = NULL,
       y = NULL)+
  theme(legend.position = "none",
        text = element_text(family = font, color = "#ffb703"),
        panel.grid = element_blank(),
        plot.title = element_text(family = font, size = 10, hjust = .5, color = "#ffb703"),
        axis.text = element_text(family = font, color = "#ffb703"))

p_at <- expanded_df %>%
  filter(Rank_att <= 5) %>%
  ggplot(aes(x = generation_id, y = Rank_att, color = type_1, group = pokemon)) +
  geom_bump(linewidth = 1, alpha = .5) +
  geom_point(size = 3,alpha = .7) +
  geom_text_repel(data = expanded_df %>% filter(generation_id == FirstGen, Rank_att <= 5),
            aes(label = pokemon),
            size = 3, family = font, seed = 2137) +
  scale_y_reverse() +  # Reverse so Rank 1 is on top
  scale_x_continuous(breaks = 1:7) +
  scale_color_manual(values = setNames(expanded_df %>% filter(generation_id == FirstGen, Rank_att <= 5) %>% pull(color_1),
                                       expanded_df %>% filter(generation_id == FirstGen, Rank_att <= 5) %>% pull(type_1))) +
  theme_minimal() +
  labs(title = "attack",
       x = NULL,
       y = "Rank")+
  theme(legend.position = "none",
        text = element_text(family = font, color = "#ffb703"),
        panel.grid = element_blank(),
        plot.title = element_text(family = font, size = 10, hjust = .5, color = "#ffb703"),
        axis.text = element_text(family = font, color = "#ffb703"))

p_df <- expanded_df %>%
  filter(Rank_df <= 5) %>%
  ggplot(aes(x = generation_id, y = Rank_df, color = type_1, group = pokemon)) +
  geom_bump(linewidth = 1, alpha = .5) +
  geom_point(size = 3, alpha = .7) +
  geom_text_repel(data = expanded_df %>% filter(generation_id == FirstGen, Rank_df <= 5),
            aes(label = pokemon), 
            size = 3, family = font,face = "bold") +
  scale_y_reverse() +  # Reverse so Rank 1 is on top
  scale_x_continuous(breaks = 1:7) +
  scale_color_manual(values = setNames(expanded_df %>% filter(generation_id == FirstGen, Rank_df <= 5) %>% pull(color_1),
                                       expanded_df %>% filter(generation_id == FirstGen, Rank_df <= 5) %>% pull(type_1))) +
  theme_minimal() +
  labs(title = "Defense",
       x = "Generation",
       y = NULL)+
  theme(legend.position = "none",
        text = element_text(family = font, color = "#ffb703"),
        panel.grid = element_blank(),
        plot.title = element_text(family = font, size = 10, hjust = .5, color = "#ffb703"),
        axis.text = element_text(family = font, color = "#ffb703"))



final <- p_at + p_df + p_hp + plot_annotation(title = "Evolution of Power: Tracking the Top Pokémon by Attack, Defense, and HP Across Generations",
                                     subtitle = str_wrap("In these charts, a generation refers to the cumulative set of all Pokémon available up to that point. For example, Generation 3 includes all Pokémon introduced in Generations 1, 2, and 3. Each bump chart shows how the top-ranked Pokémon for Attack, Defense, and HP evolve as new Pokémon are introduced. In Attack generations form 1 to 4 introduced a new strongest Pokémon. In Defense since generation 2 Schuckle is strongest and since generation 3 regirock was 2nd and Steelix 3rd. In HP Since generation 2 Blissey is on top and Chansey is 2nd. Top HP Pokémons are dominated by normal type, top defense are more varied: bug, rock, steel and water. Top attack Pokémons are dragon, ground, normal and rock", 120),
                                     caption = "data: {pokemon} | 03-04-2025 | Michał Wypych",
                                     theme = theme(text = element_text(family = font, color = "#ffb703"),
                                                   plot.title = element_text(family = font, color = "#ffb703", size = 15, margin = margin(10,0,10,0)),
                                                   plot.subtitle = element_text(family = font, color = "#ffb703", size = 8, margin = margin(0,0,20,0)),
                                                   plot.caption = element_text(family = font, color = "#ffb703", size = 4),
                                                   plot.background = element_rect(fill = "#023047", color = "#023047"))                                     )



ggsave("C:/Users/wypyc/Desktop/pokemon.png", final, width = 10, height = 6, dpi = 300)

