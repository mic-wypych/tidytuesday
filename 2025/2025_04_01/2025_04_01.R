library(tidyverse)
library(ggridges)
library(ggbump)
library(patchwork)
pokemon_df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-01/pokemon_df.csv')




expanded_data_defense <- pokemon_df %>%
  group_by(pokemon) %>%
  complete(generation_id = full_seq(1:7, 1)) %>%
  fill(defense, .direction = "down") %>%  # Carry forward attack values
  ungroup()

# Compute rank per generation
expanded_data_defense <- expanded_data_defense %>%
  group_by(generation_id) %>%
  arrange(desc(defense)) %>%
  mutate(Rank = row_number()) %>%
  ungroup() %>%
  filter(Rank <= 5)

expanded_data_attack <- pokemon_df %>%
  group_by(pokemon) %>%
  complete(generation_id = full_seq(1:7, 1)) %>%
  fill(attack, .direction = "down") %>%  # Carry forward attack values
  ungroup()

# Compute rank per generation
expanded_data_attack <- expanded_data_attack %>%
  group_by(generation_id) %>%
  arrange(desc(attack)) %>%
  mutate(Rank = row_number()) %>%
  ungroup() %>%
  filter(Rank <= 5)


expanded_data_hp <- pokemon_df %>%
  group_by(pokemon) %>%
  complete(generation_id = full_seq(1:7, 1)) %>%
  fill(hp, .direction = "down") %>%  # Carry forward attack values
  ungroup()

# Compute rank per generation
expanded_data_hp <- expanded_data_hp %>%
  group_by(generation_id) %>%
  arrange(desc(hp)) %>%
  mutate(Rank = row_number()) %>%
  ungroup() %>%
  filter(Rank <= 5)

# Plot with ggbump
p_hp <- ggplot(expanded_data_hp, aes(x = generation_id, y = Rank, color = pokemon, group = pokemon)) +
  geom_bump() +
  geom_point() +
  geom_text(aes(label = pokemon), hjust = -0.1, size = 3, vjust = 1) +
  scale_y_reverse() +  # Reverse so Rank 1 is on top
  scale_color_manual(values = expanded_data_hp$color_1) +
  theme_minimal() +
  labs(title = "Top Pokémon by HP Rank Across Generations",
       x = "Generation",
       y = "HP Rank")

p_df <- ggplot(expanded_data_defense, aes(x = generation_id, y = Rank, color = pokemon, group = pokemon)) +
  geom_bump() +
  geom_point() +
  geom_text(aes(label = pokemon), hjust = -0.1, size = 3, vjust = 1) +
  scale_y_reverse() +  # Reverse so Rank 1 is on top
  scale_color_manual(values = expanded_data_hp$color_1) +
  theme_minimal() +
  labs(title = "Top Pokémon by Defense Rank Across Generations",
       x = "Generation",
       y = "Defense Rank")

p_at <- ggplot(expanded_data_attack, aes(x = generation_id, y = Rank, color = pokemon, group = pokemon)) +
  geom_bump() +
  geom_point() +
  geom_text(aes(label = pokemon), hjust = -0.1, size = 3, vjust = 1) +
  scale_y_reverse() +  # Reverse so Rank 1 is on top
  scale_color_manual(values = expanded_data_hp$color_1) +
  theme_minimal() +
  labs(title = "Top Pokémon by Attack Rank Across Generations",
       x = "Generation",
       y = "Attack Rank")

p_at + p_df + p_hp
