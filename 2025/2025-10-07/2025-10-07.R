library(tidyverse)
library(grid)
library(patchwork)
library(showtext)
library(ggpattern)
font <- "Orbitron"
font_2 <- "Audiowide"
font_add_google(font)
font_add_google(font_2)
showtext_auto()


euroleague_basketball <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-10-07/euroleague_basketball.csv')



#### grid option -------------
final_four_df <- euroleague_basketball %>%
  separate_rows(Years_of_FinalFour_Appearances, sep = ", ") %>%
  mutate(Years_of_FinalFour_Appearances = as.numeric(Years_of_FinalFour_Appearances)) %>%
  drop_na(Years_of_FinalFour_Appearances)


color_pal <-c("#b96784", "#88375d","#9bb6bd","#009688", "#755687","#4a2d5e", "#eb676c","#aa2041", "#b52dc8")
legend_df <- data.frame(color_pal, Team = unique(final_four_df$Team )) 

named_colors <- legend_df$color_pal
names(named_colors) <- legend_df$Team
final_four <- final_four_df %>%
  mutate(y_pos = rbeta(n(), 15, 8)) %>%
  ggplot(aes(x = Years_of_FinalFour_Appearances, y = y_pos, color = Team)) +
  geom_point(size = 4)+
  coord_radial(start = -.5*pi, end = .5*pi) +
  scale_x_continuous(expand = c(0,0), breaks = 1986:2025) +
  scale_color_manual(values = named_colors) +
  theme_void(base_size = 30) +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.line.x = element_line(color = "black", linewidth = 1.1),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 25, family = font_2, hjust = 0, vjust = 0,
        angle= 180 - 180/ length(unique(final_four_df$Years_of_FinalFour_Appearances)) * seq_along(final_four_df$Years_of_FinalFour_Appearances)),
        plot.margin = margin(0,0,0,0),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid = element_line(color = "black", linewidth = .1),
        plot.background = element_rect(fill = 'transparent', color = 'transparent'))

title_won_df <- euroleague_basketball %>%
  separate_rows(Years_of_Titles_Won, sep = ", ") %>%
  mutate(Years_of_Titles_Won = as.numeric(Years_of_Titles_Won)) 


title_won <- title_won_df %>%
  drop_na(Years_of_Titles_Won) %>%
  mutate(y_pos = rbeta(n(), 15, 8)) %>%
  ggplot(aes(x = Years_of_Titles_Won, y = y_pos, color = Team)) +
  geom_point(size = 4) +
  coord_radial(start = .5*pi, end = -.5*pi) +
  scale_x_continuous(expand = c(0,0), breaks = 1991:2025) +
  scale_color_manual(values = named_colors) +
  theme_void(base_size = 30) +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.line.x = element_line(color = "black", linewidth = 1.1),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 25, family = font_2, 
        angle= 180 - 180/ length(unique(final_four_df$Years_of_FinalFour_Appearances)) * seq_along(final_four_df$Years_of_FinalFour_Appearances)),
        plot.margin = margin(0,0,0,0),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid = element_line(color = "black", linewidth = .1),
        panel.background = element_blank(),
        plot.background = element_rect(fill = 'transparent', color = 'transparent'))


#legend plot

legend <- legend_df %>%
  ggplot() +
  geom_point(aes(x = 1, y = color_pal, color = color_pal), size = 5) +
  geom_text(aes(x = 1.2, y = color_pal, label = Team), hjust = 0, family = font_2, size = 15) +
  scale_color_identity() +
  scale_x_continuous(limits = c(.9, 2)) +
  theme_void()

### setting up a grid

canvas <- ggplot() + geom_col_pattern(
    aes(x = 1, y = 1
    ),
    pattern_orientation = "radial",
    pattern       = 'gradient',
    pattern_fill = "#efefef",
    pattern_fill2 = '#ff9e00',
    colour        = NA,
    alpha = .5
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_void() + theme(plot.margin = margin(0,0,0,0),
  legend.position = "none",
  strip.text = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  axis.title = element_blank(), aspect.ratio = 1.4)

#lines and circle
line_1 <- segmentsGrob(x0 = 0, y0 = 0, x1 = 1, y1 = 0.,
    default.units = "npc",gp = gpar(lwd = 3))
line_2 <- segmentsGrob(x0 = 0, y0 = 1, x1 = 0, y1 = 0,
    default.units = "npc",gp = gpar(lwd = 3))


#texts
title <- ggplot() +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = .7), linewidth = 1.1) +
  geom_text(aes(x = 0, y = 0, label = "Euro\nBasket"), 
                family = font, size = 30, fontface = "bold", lineheight = .4) +
  theme_void() +
  theme(
  panel.background = element_blank(),
        plot.background = element_rect(fill = 'transparent', color = 'transparent'))

#plot titles
final_four_note <- textGrob(
  "Teams that made it to final four",
  just = "center", x = 0,
  gp = gpar(fontface = "bold", fontsize = 50, fontfamily = font,lineheight = .5)
)

title_won_note <- textGrob(
  "Teams that won a title",
  just = "center", x = 0,
  gp = gpar(fontface = "bold", fontsize = 50, fontfamily = font,lineheight = .5)
)


#annotation


final <- canvas +
  inset_element(title_won, 
                left = 0.05, bottom = .7, right = .95, top = .95, align_to = "panel") +
  inset_element(final_four, 
                left = 0.05, bottom = .05, right = .95, top = .3, align_to = "panel") +
  inset_element(legend, 
                left = 0.05, bottom = .32, right = .35, top = .72, align_to = "panel") +
  inset_element(line_1, left = .01, bottom = .01, right = .99, top = .01,
                align_to = "full", on_top = TRUE, clip = TRUE) +
  inset_element(line_1, left = .01, bottom = .99, right = .99, top = .99,
                align_to = "full", on_top = TRUE, clip = TRUE) +
  inset_element(line_1, left = .01, bottom = .5, right = .99, top = .5,
                align_to = "full", on_top = TRUE, clip = TRUE) +
  inset_element(line_2, left = .01, bottom = .01, right = .01, top = .99,
              align_to = "full", on_top = TRUE, clip = TRUE) +
  inset_element(line_2, left = .99, bottom = .01, right = .99, top = .99,
              align_to = "full", on_top = TRUE, clip = TRUE) +
  inset_element(line_2, left = .245, bottom = .01, right = .245, top = .072,
              align_to = "full", on_top = TRUE, clip = TRUE) +
  inset_element(line_2, left = .755, bottom = .01, right = .755, top = .072,
              align_to = "full", on_top = TRUE, clip = TRUE) +
  inset_element(line_2, left = .245, bottom = .928, right = .245, top = .99,
              align_to = "full", on_top = TRUE, clip = TRUE) +
  inset_element(line_2, left = .755, bottom = .928, right = .755, top = .99,
              align_to = "full", on_top = TRUE, clip = TRUE) +
  inset_element(title,
                left =.39, bottom = .42, right = .61, top = .58, align_to = "full") +
    inset_element(final_four_note,
                left =.5, bottom = .31, right = .5, top = .31, align_to = "full") +
  inset_element(title_won_note,
                left =.5, bottom = .69, right = .5, top = .69, align_to = "full") +
  plot_annotation(theme = theme(plot.margin = margin(0,0,0,0)))

ggsave("C:/Users/wypyc/Desktop/basketball.png", final, width = 10, height = 14, bg = "transparent")

