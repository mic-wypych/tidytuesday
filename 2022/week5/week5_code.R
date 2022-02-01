#2022 week5
library(tidyverse)
library(tidytuesdayR)
library(ggthemes)
library(broom)
library(patchwork)
library(grid)
library(ggtext)

breed_traits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')
trait_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/trait_description.csv')
breed_rank_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv')

#get top 5 dog breeds
breed_rank_all %>%
  rowwise() %>%
  mutate(mean_rank = mean(c(`2013 Rank`, `2014 Rank`, `2015 Rank`,
                            `2016 Rank`, `2017 Rank`, `2018 Rank`,
                            `2019 Rank`, `2020 Rank`), na.rm = T)) %>%
  ungroup() %>%
  select(Breed, mean_rank) %>%
  arrange(mean_rank) %>%
  slice(1:5)


#palette
palette <- c("#005F73", "#0A9396", "#94D2BD", "#BFD5B2", "#E9D8A6",
             "#E9D8A6", "#ECBA53", "#CA6702","#BB3E03",
             "#AE2012", "#9B2226","#AC484C", "#BB686B", "#C17678")


#### trait scores for top 5 dogs ####
dogs_trait_plot <- breed_traits %>%
  dplyr::select(-c(`Coat Type`, `Coat Length`)) %>%
  slice(1,3,4,5,7) %>% # had to slice instead of filtering because filtering does not work for some reason
  pivot_longer(cols = `Affectionate With Family`:`Mental Stimulation Needs`, names_to = "trait", values_to = "level") %>%
  mutate(Breed = factor(Breed, levels = c(breed_traits[1,1], breed_traits[3,1], #same here - for some reason can't use the text
                                          breed_traits[4,1], "Bulldogs", "Beagles"), ordered = T)) %>%
  group_by(Breed) %>%
  summarize(test = rep(trait, level)) %>%
  ggplot() +
  geom_dotplot(aes(x = test, fill = test, color = test),binwidth = 1, method = "histodot", stackratio = 1.25, dotsize = .5) +
  scale_y_continuous(limits = c(0, .075), expand = c(0,0), labels = NULL, breaks = NULL) +
  scale_x_discrete(labels = NULL, breaks = NULL) +
  scale_color_manual(values = palette) +
  scale_fill_manual(values = palette) +
  coord_flip() +
  facet_wrap(~Breed, nrow = 1) +
  theme_tufte() +
  labs(x = "", y = "") +
  theme(strip.text = element_text(hjust = -.002, size = 25, color = "white"),
        strip.background=element_rect(fill="grey40"),
        legend.position = "none",
        panel.spacing = unit(1, "lines"))
dogs_trait_plot



####predictors of mean rank ####

#join datasets with mean rank
mean_ranks <- breed_rank_all %>%
  rowwise() %>%
  mutate(mean_rank = mean(c(`2013 Rank`, `2014 Rank`, `2015 Rank`,
                            `2016 Rank`, `2017 Rank`, `2018 Rank`,
                            `2019 Rank`, `2020 Rank`), na.rm = T)) %>%
  select(mean_rank)


predictors <- breed_traits %>%
  cbind(mean_ranks) %>%
  select(-c(`Coat Type`, `Coat Length`, Breed)) %>%
  lm(mean_rank ~ ., data = .) %>%
  tidy(conf.int = T) %>%
  filter(term != "(Intercept)") %>%
  mutate(term = str_remove_all(term, pattern = "`")) %>%
  ggplot() +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "white") +
  geom_point(aes(x = term, y = estimate, color = term), size = 5) +
  geom_segment(aes(x = term, xend = term, y = conf.low, yend = conf.high, color = term), size = 3) +
  scale_x_discrete(labels = NULL, breaks = NULL) +
  scale_y_continuous(labels = NULL, breaks = NULL) +
  scale_color_manual(values = palette) +
  coord_flip() +
  theme_tufte() +
  labs(x = "", y = "", caption ="Note: The entire model explained around 16% of variance in rank.\nPartial Cohen's f for playfulness was equal to .29 which means the study had sufficient power to detect such an effect.\n There were also no serious problems with collinearity (highest vif for Mental Stimulation Needs was equal to 2.29") +
  theme(legend.position = "none",
        axis.text = element_text(size = 20, color = "white"),
        axis.title = element_text(size = 20, color = "white"),
        plot.title = element_text(size = 30, color = "white"),
        plot.subtitle = element_text(size = 20, color = "white"),
        plot.caption = element_text(size = 10, color = "white"))
predictors

####legend plot####

legend_data <- breed_traits %>%
  dplyr::select(-c(`Coat Type`, `Coat Length`, Breed)) %>%
  pivot_longer(cols = `Affectionate With Family`:`Mental Stimulation Needs`, names_to = "trait", values_to = "level") %>%
  select(trait) %>%
  distinct()
  
legend <- legend_data %>% 
  ggplot(aes(x = trait, y = 1, color = trait, fill = trait), size = 10) +
  geom_dotplot(fill = "grey10", color = "grey10") +
  geom_richtext(data = legend_data, aes(x = trait, y = .1, label = trait),
                fill = NA,label.color = NA, hjust = 0, size = 8,
                label.padding = grid::unit(rep(0, 8), "pt")
  ) +
  scale_color_manual(values = palette) +
  coord_flip() +
  theme_void() +
  scale_x_discrete(labels = legend_data$trait) +
  theme(legend.position = "none")

legend


#final patchwork

layout <- "
AAAABC
AAAABC
"

final <- dogs_trait_plot + legend + predictors +
  plot_layout(design = layout) +
  plot_annotation(title = "Characteristics of different dog breeds",
                  subtitle = "<p>Labrador Retrievers, German Shepherd dogs, Golden Retrievers, Bulldogs and Beagles have the best average rank from 2013 to 2020.<br>
                  The plot on the left shows the traits of these dogs on a scale from \u26aa to \u26aa\u26aa\u26aa\u26aa\u26aa <br> The plot on the right shows which traits are best predictors of rank.
                  If the horiontal lines do not cross the dashed vertical one,<br> then a given trait is a significant predictor of rank
                  When taking into account all other traits, <span style = 'color:#9B2226'>playfulness</span> seems to be the strongest predictor of rank</p>",
                  caption = "Visualization Michał Wypych | #Tidytuesday 2022 week 5 | data: Kaggle",
                  theme = theme(
                    plot.title = ggtext::element_markdown(size = 40, color = "white", family = "roboto"),
                    plot.subtitle = ggtext::element_markdown(size = 20, color = "white",lineheight=1.2, family = "roboto"),
                    plot.caption = ggtext::element_markdown(size = 5, color = "white", family = "roboto"),
                    plot.background = element_rect(color = "grey10", fill = "grey10"))
  )
#saving plot

ggsave(filename = "dogs.png", plot = final, path = "C:/Users/user/Desktop/R_related/dataviz exercise/Tidytuesday/2022/week5", dpi = 320, width = 30,height = 15, units = "in")

