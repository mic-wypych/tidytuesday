#### week 44 of tidytuesday ####

#loading packages
library(tidytuesdayR)
library(tidyverse)
library(ggthemes)
library(ggtext)
library(mdthemes)
library(ggdist)
library(patchwork)
library(extrafont)




#getting data
ultra_rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/ultra_rankings.csv')
race <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv')

#join datasets
data <- ultra_rankings %>%
  inner_join(race, by = "race_year_id")


#create dataset for plotting men vs women differences in median rank
median_data <- data %>%
  filter(age > 18, age < 100, !is.na(gender), !is.na(age), !is.na(rank)) %>%
  mutate(age_5 = cut(age, 12)) %>%
  group_by(age, gender) %>%
  mutate(median_rank = median(rank, na.rm = T)) %>%
  select(age, median_rank, gender) %>%
  distinct() %>%
  pivot_wider(id_cols = c("age","median_rank", "gender"), names_from = "gender", 
              values_from = "median_rank") %>%
  mutate(men_higher = ifelse(M > W, T, F))

#Plot median

median_plot <- data %>%
  filter(age > 18, age < 100, !is.na(gender), !is.na(age), !is.na(rank)) %>%
  group_by(age, gender) %>%
  mutate(median_rank = median(rank, na.rm = T)) %>%
  select(age, median_rank, gender) %>%
  ggplot() +
  geom_segment(data = median_data, aes(x = age, xend = age, y = M, yend = W, color = men_higher)) +
  geom_point(aes(x = age, y = median_rank, color = gender)) +
  scale_color_manual(values = c("#8D99AE","#2B2D42", "#D90429", "#EF233C"), guide = "none") +
  scale_x_continuous(breaks = c(20,30,40,50,60,70,80)) +
  coord_flip() +
  theme_tufte() +
  labs(x = "age", y = "median rank") +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20)
  )


##Create dataset for plott differences in mean between men and women across ages
mean_data <- data %>%
  filter(age > 18, age < 100, !is.na(gender), !is.na(age), !is.na(rank)) %>%
  group_by(age, gender) %>%
  mutate(mean_rank = mean(rank, na.rm = T)) %>%
  select(age, mean_rank, gender) %>%
  distinct() %>%
  pivot_wider(id_cols = c("age","mean_rank", "gender"), names_from = "gender", 
              values_from = "mean_rank") %>%
  mutate(men_higher = ifelse(M > W, T, F))

#Plot mean

mean_plot <- data %>%
  filter(age > 18, age < 100, !is.na(gender), !is.na(age), !is.na(rank)) %>%
  group_by(age, gender) %>%
  mutate(mean_rank = mean(rank, na.rm = T)) %>%
  select(age, mean_rank, gender) %>%
  ggplot() +
  geom_segment(data = mean_data, aes(x = age, xend = age, y = M, yend = W, color = men_higher)) +
  geom_point(aes(x = age, y = mean_rank, color = gender)) +
  scale_color_manual(values = c("#8D99AE","#2B2D42", "#D90429", "#EF233C"), guide = "none") +
  scale_x_continuous(breaks = c(20,30,40,50,60,70,80)) +
  coord_flip() +
  theme_tufte() +
  labs(x = "age", y = "mean rank") +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20)
  )


###plot distributions


dist_plot <- data %>%
  filter(age > 18, age < 100, !is.na(gender), !is.na(age), !is.na(rank)) %>%
  select(age, rank, gender) %>%
  filter(!(age == 19 & gender == "W"),
         !(age == 21 & gender == "W"),
         !(age == 79 & gender == "W"),
         !(age == 81 & gender == "W")) %>%
  ggplot(aes(y = as.factor(age), x = rank, color = gender, fill = gender)) +
  stat_halfeye(alpha = .7, position = "dodge") +
  scale_x_continuous(limits = c(1,1600)) +
  theme_tufte() +
  scale_color_manual(values = c("#2B2D42", "#EF233C"), guide = "none") +
  scale_fill_manual(values = c("#2B2D42", "#EF233C"), guide = "none") +
  labs(x = "rank", y = "age") +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20)
  )


#putting plots together
 
final <- ((mean_plot / median_plot) | dist_plot) +
  plot_annotation(
    title = "The mean or the median?",
    subtitle = "Below you can see differences in mean (top) and median (bottom) rank between <span style = 'color:#2B2D42'>**Men**</span> and <span style = 'color:#EF233C'>**Women**</span> across the ages of participants.<br>
    The plot also shows if <span style = 'color:#8D99AE'>**Men**</span> or <span style = 'color:#D90429'>**Women**</span> have better mean/median rank. Results for mean and median vary dramatically.<br>
    Plot on the right shows distributions of ranks of <span style = 'color:#2B2D42'>**Men**</span> and <span style = 'color:#EF233C'>**Women**</span> across ages",
    caption = "Visualization Michał Wypych | #Tidytuesday week 44 | data: https://github.com/BjnNowak/UltraTrailRunning",
    theme = theme(plot.title = ggtext::element_markdown(size = 40, family = "roboto"),
          plot.subtitle = ggtext::element_markdown(size = 30,  family = "roboto"),
          plot.caption = element_text(size = 12)))

#save the file
ggsave(filename = "marathon.png", plot = final, path = "C:/Users/user/Desktop/dataviz exercise/Tidytuesday/2021/week44", dpi = 320, width = 30,height = 15, units = "in")
