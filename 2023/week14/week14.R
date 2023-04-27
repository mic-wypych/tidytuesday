library(tidyverse)
library(waffle)
library(showtext)
library(hrbrthemes)


font_add_google('Roboto')
soccer <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-04/soccer21-22.csv')
showtext::showtext_auto()


#get df
cards <- soccer %>%
  mutate(red = HR + AR,
         yellow = HY + AY) %>%
  group_by(Referee) %>%
  summarise(red =sum(red),
            yellow = sum(yellow)) %>%
  pivot_longer(cols = c(red, yellow), names_to ='card', values_to = 'number')

#plot
plot1 <- cards %>%
  mutate(Referee = fct_reorder(Referee, number)) %>%
  ggplot(aes(values = number, fill = card)) +
  waffle::geom_waffle(colour = 'white', na.rm = F, flip = T, nrows = 5, n_rows = 5) +
  facet_wrap(~Referee, nrow = 2, strip.position = "bottom") +
  scale_fill_manual(values = c('firebrick2', 'yellow3')) +
  scale_x_continuous(labels = NULL) +
  scale_y_continuous(expand = c(0,0), labels = function(x) x * 5) +
  theme_enhance_waffle() +
  theme_ipsum_rc(grid="") +
  labs(title = 'Yellow and red cards by referee') +
  theme(legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_line(linewidth = .05),
        plot.title = element_text(hjust = .5, size = 20, family = "Roboto"),
        strip.text = element_text(size = 5, family = "Roboto"),
        axis.text.y = element_text(size = 5, family = "Roboto"),
        plot.background = element_rect(fill = "white", color = "white"))
plot1


ggsave("C:/Users/User/Documents/GitHub/tidytuesday/2023/week14/week14.png", plot1, width = 10, height = 5)
