library(tidyverse)
library(ggridges)
library(ggbump)
library(patchwork)
library(showtext)
library(ggrepel)
font_add_google("Sriracha")
showtext_auto()

font <- "Sriracha"

pokemon_df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-01/pokemon_df.csv')



expanded_df <- pokemon_df %>%
  drop_na(generation_id) %>%
  group_by(pokemon) %>%
  complete(generation_id = full_seq(1:7, 1)) %>%
  fill(defense, .direction = "down") %>% 
  fill(attack, .direction = "down") %>%
  fill(hp, .direction = "down") %>%# Carry forward attack values
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
  ggplot(aes(x = generation_id, y = Rank_hp, color = pokemon, group = pokemon)) +
  geom_bump(linewidth = 1) +
  geom_point(size = 3) +
  geom_text_repel(data = expanded_df %>% filter(generation_id == FirstGen, Rank_hp <= 5),
            aes(label = pokemon),
            hjust = -0.1, size = 5, vjust = 1, family = font) +
  scale_y_reverse() +  # Reverse so Rank 1 is on top
  scale_x_continuous(breaks = 1:7) +
  theme_minimal() +
  labs(title = "HP",
       x = "Generation",
       y = NULL)+
  theme(legend.position = "none",
        text = element_text(family = font),
        panel.grid = element_blank(),
        plot.title = element_text(family = font, size = 15, hjust = .5))

p_at <- expanded_df %>%
  filter(Rank_att <= 5) %>%
  ggplot(aes(x = generation_id, y = Rank_att, color = pokemon, group = pokemon)) +
  geom_bump(linewidth = 1, alpha = .7) +
  geom_point(size = 3) +
  geom_text_repel(data = expanded_df %>% filter(generation_id == FirstGen, Rank_att <= 5),
            aes(label = pokemon),
            hjust = -0.1, size = 5, vjust = 1, family = font) +
  scale_y_reverse() +  # Reverse so Rank 1 is on top
  scale_x_continuous(breaks = 1:7) +
  theme_minimal() +
  labs(title = "attack",
       x = "Generation",
       y = "Rank")+
  theme(legend.position = "none",
        text = element_text(family = font),
        panel.grid = element_blank(),
        plot.title = element_text(family = font, size = 15, hjust = .5))

p_df <- expanded_df %>%
  filter(Rank_df <= 5) %>%
  ggplot(aes(x = generation_id, y = Rank_df, color = pokemon, group = pokemon)) +
  geom_bump(linewidth = 1) +
  geom_point(size = 3) +
  geom_text_repel(data = expanded_df %>% filter(generation_id == FirstGen, Rank_df <= 5),
            aes(label = pokemon), 
            hjust = -0.1, size = 5, vjust = 1, family = font) +
  scale_y_reverse() +  # Reverse so Rank 1 is on top
  scale_x_continuous(breaks = 1:7) +
  theme_minimal() +
  labs(title = "Defense",
       x = "Generation",
       y = NULL)+
  theme(legend.position = "none",
        text = element_text(family = font),
        panel.grid = element_blank(),
        plot.title = element_text(family = font, size = 15, hjust = .5))



p_at + p_df + p_hp + plot_annotation(title = "Evolution of Power: Tracking the Top Pokémon by Attack, Defense, and HP Across Generations",
                                     subtitle = str_wrap("In these charts, a generation refers to the cumulative set of all Pokémon available up to that point. For example, Generation 3 includes all Pokémon introduced in Generations 1, 2, and 3. Each bump chart shows how the top-ranked Pokémon for Attack, Defense, and HP evolve as new Pokémon are introduced. In Attack generations form 1 to 4 introduced a new strongest Pokémon. In Defense since generation 2 Schuckle is strongest and since generation 3 regirock was 2nd and Steelix 3rd. In HP Since generation 2 Blissey is on top and Chansey is 2nd.", 120),
                                     theme = theme(text = element_text(family = font),
                                                   plot.title = element_text(family = font, size = 30, margin = margin(10,0,0,20)),
                                                   plot.subtitle = element_text(family = font, size = 15, margin = margin(20,0,0,20))),
                                     )

