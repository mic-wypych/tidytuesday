library(tidyverse)
library(patchwork)
library(grid)
library(stringr)
library(scales)
library(colorspace)
library(patchwork)
library(showtext)

font <- "Anton"
font_2 <- "Inter"
font_add_google(font)
font_add_google(font_2)
showtext_auto()

fide_ratings_september <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-23/fide_ratings_september.csv')

fide_ratings_september$age_bin <- cut(fide_ratings_september$bday, 7)
fide_ratings_september$rating_bin <- cut(fide_ratings_september$rating, 
                                         breaks = c(1399, 1600, 1800, 2000, 2200, 2400, 2600, 2839),
                                         labels = c("1400-1600", "1600-1800", "1800-2000", "2000-2200", "2200-2400","2400-2600", "2600-2839"))
fide_ratings_september$games_bin <- cut(fide_ratings_september$games, 7)


fide_ratings_september$games_bin <- factor(fide_ratings_september$games_bin,
                                            labels = c("0 - 5", "6 - 11", "12 - 17",
                                                       "17 - 22", "23 - 28", "29 - 34",
                                                        "34 - 40"))




df <- fide_ratings_september %>%
  count(rating_bin, games_bin, sex) %>%
  group_by(rating_bin, games_bin) %>%
  mutate(total_n = sum(n)) %>%
  ungroup() %>%
  mutate(shade = rescale(log(total_n), to = c(0.2, 1)),   # now varies across all facets
         base_col = if_else(sex == "M", "#e36414", "#0f4c5c"),
         fill_col = desaturate(base_col, amount = 1 - shade))

final <- ggplot(df, aes(x = 1, y = n, fill = fill_col)) +
  geom_col(position = "fill") +
  facet_grid(rows = vars(rating_bin), cols = vars(games_bin)) +
  coord_equal() +
  scale_fill_identity() +                 # use the colors as-is
  theme_void() +
  theme(legend.position = "none",
        strip.text = element_text(size = 30, family = font_2),
        plot.margin = margin(0, 0, 0, 0),
        plot.background = element_rect(fill = NA, color = NA))


# making legend
legend <- fide_ratings_september %>%
  count(rating_bin, games_bin, sex) %>%
  filter(rating_bin == unique(rating_bin)[1], games_bin == unique(games_bin)[1]) %>%
  ggplot(aes(x= 1, y = n, fill = sex)) +
      geom_col(position = "fill") +
      annotate(geom = "text", x = 0.1, y = 1.15, label = "Women", family = font_2, size = 9) +
      annotate(geom = "text", x = 2, y = .43, label = "Men", family = font_2, size = 9) +
      annotate(geom = "segment", x = -.1, xend = .6, y = 1.05, yend = 1.05) +
      annotate(geom = "segment", x = .6, xend = 1, y = 1.05, yend = .9) +
      annotate(geom = "segment", x = 1.5, xend = 2.1, y = .35, yend = .35) +
      annotate(geom = "segment", x = 1.5, xend = 1, y = .35, yend = .52) +
      coord_equal() +
      labs(title = "Legend") +
      scale_fill_manual(values = c(M = "#e36414",F= "#0f4c5c")) +
      theme_void()+
      theme(legend.position = "none",
            panel.spacing = unit(0, "pt"),
            plot.title = element_text(family = font_2, size = 40, hjust = .5))

canvas <- ggplot() + theme_void() + theme(plot.margin = margin(0,0,0,0),
  panel.spacing = unit(0, "pt"),
  strip.background = element_blank(),
  strip.text = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  axis.title = element_blank(), aspect.ratio = 1.4)


# annotations

#layout ideas:
# make lines that connect annotation to main plot?
# add some background to annotations?
# make the layout more "grid like? " e,g, make annotations above the plot?

# no players with a lot of games with lo or high rankings
#No player played more than 40 games in September 2025. PLayers who played the most games ranked between 2000 and 2600. PLayers below and above those rankings played below 34 games in a month
rating_note <- textGrob(
  str_wrap("No player played more than 40 games in September 2025. PLayers who played the most games ranked between 2000 and 2600. PLayers below and above those rankings played below 34 games in a month",50),just = "left", x = 0,
  gp = gpar( fontsize = 30, fontfamily = font_2,lineheight = .3)
)

#gender split
#The visible gender split shows women present across rating levels but in smaller proportions compared to men and the prortion of women declines as rating goes up.
gender_note <- textGrob(
  str_wrap("The visible gender split shows women present across rating levels but in smaller proportions compared to men and the prortion of women declines as rating goes up", 45),just = "left",x = 0,
  gp = gpar(fontsize = 30, fontfamily = font_2, lineheight = .3)
)

#n players note
# Most players don't play a lot of games with 0-5 games in september being the most common. There are also more players with lower rankings
nplayer_note <- textGrob(
  str_wrap("Most players don't play a lot of games with 0-5 games in september being the most common. There are also more players with lower rankings", 15),just = "left", x = 0,
  gp = gpar(fontsize = 30, fontfamily = font_2, lineheight = .3)
)

#title
title_note <- textGrob(
  "Chess players in September 2025\nacross rating and games played",just = "left", x = 0,
  gp = gpar( fontsize   = 80,
    fontface = "bold", fontfamily = font, lineheight = .3)
)

#subtitle
subtitle_note <- textGrob(
  str_wrap("This visualization maps the distribution of chess players across brackets of games played and rankings in September 2025, showing the number of games played on average by men and women. Each square corresponds to a combination of games played and rating, with the fill indicating percentage of women. Intensity of color indicates total number of players in the bracket - the more intense, the more players", 80),
  just = "left", x = 0,
  gp = gpar(fontface = "bold", fontsize = 40, fontfamily = font_2,lineheight = .5)
)

line_1 <- segmentsGrob(x0 = 0, y0 = 0, x1 = 1, y1 = 0,
    default.units = "npc")
line_2 <- segmentsGrob(x0 = 0, y0 = 0, x1 = 1, y1 = 0,
    default.units = "npc")
line_3 <- segmentsGrob(x0 = 0, y0 = 1, x1 = 0, y1 = 0,
    default.units = "npc")

plot_final <- canvas +
  inset_element(wrap_elements(full = legend), 
                left = 0.75, bottom = 0.644, right = .95, top = .744, align_to = "full") +
  inset_element(wrap_elements(full = title_note),
                left =.05, bottom = .9, right = .7, top = .97, align_to = "full") +
  inset_element(wrap_elements(full = subtitle_note),
                left = .05, bottom = .75, right = .8, top = .9, align_to = "full") +
  inset_element(wrap_elements(full = rating_note),
                left =.435, bottom = 0.12, right = .785, top = .18, align_to = "full") +
  inset_element(wrap_elements(full = gender_note),
                left = .28, bottom = 0.65, right = 0.58, top = .7, align_to = "full") +
  inset_element(wrap_elements(panel = nplayer_note),
                left = 0.05, bottom = .47, right = 0.17, top = .62, align_to = "full") +
  inset_element(line_1, left = .28, bottom = .65, right = .58, top = .65,
                align_to = "full", on_top = TRUE, clip = TRUE) +
  inset_element(line_2, left = .435, bottom = .125, right = .785, top = .125,
              align_to = "full", on_top = TRUE, clip = TRUE) +
  inset_element(line_3, left = .05, bottom = .485, right = .05, top = .6,
              align_to = "full", on_top = TRUE, clip = TRUE) +
  inset_element(final, left = 0.2, bottom = 0.1, right = 0.8, top = 0.70, align_to = "full") +
  patchwork::plot_annotation(caption = "Tidytuesday 2025-09-23 | data: FIDE | Michał Wypych",
                             theme = theme(plot.margin = margin(0,0,0,0),
                                     plot.caption = element_text(family = font_2, size = 20)))

ggsave("C:/Users/wypyc/Desktop/chess_1.png", plot_final, width = 10, height = 14)



