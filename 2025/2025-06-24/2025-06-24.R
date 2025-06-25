library(tidyverse)
library(geofacet)
library(ggview)
library(showtext)
library(patchwork)
library(gganimate)
library(png)
library(grid)


font <- "Lato"

font_add_google(font)
showtext_auto()

cases_moshowtextcases_month <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-24/cases_month.csv')
cases_year <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-24/cases_year.csv')



# idea 1:
#' geofacet with cases of measles in Europe
#' animated to show changes across the years
#' individual plots are like a virus on a circle

cases_year_tojoin <- cases_year %>% select(country, year, total_population)

eu_legend <- cases_month %>%
  inner_join(cases_year_tojoin, by = c("country", "year")) %>%
  group_by(year) %>%
  mutate(measles_lab_per_1000000 = 100000*measles_lab_confirmed/total_population) %>%
  mutate(country = recode(country, "United Kingdom of Great Britain and Northern Ireland" = "United Kingdom",
                          "Netherlands (Kingdom of the)" = "Netherlands",
                          "Czechia" = "Czech Republic")) %>%
  filter(country == "Romania", year == 2024)  %>%
  mutate(measles_lab_confirmed = replace_na(measles_lab_confirmed, 0)) %>%
  ggplot(aes(x = month, y = measles_lab_per_1000000 , color = factor(year))) +
  geom_hline(aes(yintercept = 0), color = "#dd2d4a") +
  geom_linerange(aes(x = month, ymin = 0, ymax = measles_lab_per_1000000, color = factor(year)), 
                 position = position_dodge(width = .5), linewidth = 1, color = "#dd2d4a") +
  geom_point( position = position_dodge(width = .5), size = 1, color = "#dd2d4a") +
  scale_x_continuous(breaks = 1:12, labels = c("Jan", "Feb", "Mar", "Apr", "May",
                                               "Jun", "Jul", "Aug", "Sep", "Oct",
                                               "Nov", "Dec")) +
  scale_y_continuous(limits = c(-10, 20)) +
  coord_radial() +
  theme_void() +
  theme(panel.spacing = unit(.5, "lines"),
        axis.text.x = element_text(family = font, size = 10),
        strip.text = element_text(size = 9, family = font, face = "bold"),
        legend.position = "none",
        plot.title.position = "plot",
        plot.title = element_text(size = 20, family = font, margin = margin(20,0,20,0)),
        plot.background = element_rect(fill = "#cbeef3", color = "#cbeef3")
  )


ggsave(temp_file, eu_legend, width = 1.1, height = 1.1, dpi = 150, bg = "#cbeef3")


custom_grid <- geofacet::eu_grid1 
custom_grid$row <- custom_grid$row + 1

eu_plot <-cases_month %>%
  inner_join(cases_year_tojoin, by = c("country", "year")) %>%
  group_by(year) %>%
  mutate(measles_lab_per_1000000 = 100000*measles_lab_confirmed/total_population) %>%
  mutate(country = recode(country, "United Kingdom of Great Britain and Northern Ireland" = "United Kingdom",
                          "Netherlands (Kingdom of the)" = "Netherlands",
                          "Czechia" = "Czech Republic")) %>%
  filter(country %in% geofacet::eu_grid1$name)  %>%
  mutate(measles_lab_confirmed = replace_na(measles_lab_confirmed, 0))%>%
  ggplot(aes(x = month, y = measles_lab_per_1000000 , color = factor(year))) +
  geom_hline(aes(yintercept = 0), color = "#dd2d4a") +
  geom_linerange(aes(x = month, ymin = 0, ymax = measles_lab_per_1000000, color = factor(year)), 
                 position = position_dodge(width = .5), linewidth = 2, color = "#dd2d4a") +
  geom_point( position = position_dodge(width = .5), size = 2, color = "#dd2d4a") +
  facet_geo(~country, grid = custom_grid) +
  scale_x_continuous(breaks = 1:12, labels = c("Jan", "Feb", "Mar", "Apr", "May",
                                               "Jun", "Jul", "Aug", "Sep", "Oct",
                                               "Nov", "Dec")) +
  scale_y_continuous(limits = c(-5, 20)) +
  coord_radial() +
  theme_void() +
  theme(panel.spacing = unit(.01, "lines"),
        axis.text.x = element_blank(),
        strip.text = element_text(size = 15, family = font, face = "bold"),
        legend.position = "none",
        plot.title.position = "plot",
        plot.title = element_text(size = 25, family = font, margin = margin(40,0,20,0)),
        plot.background = element_rect(fill = "#cbeef3", color = "#cbeef3"),
        plot.caption = element_text(family = font, size = 10)
        )  +
  labs(caption = "Tidytuesday 2025-06-2 | data from World Health Organisation | Michał Wypych") +
  ggtitle("Measles lab confirmed cases per 100k in {round(frame_time, 0)}") +
  transition_time(year) +
  ease_aes("cubic-in-out", transition_length = 3)

main_gif <- animate(eu_plot,renderer = gifski_renderer(), duration = 30, width = 1200, height = 1200, bg = "#cbeef3")

anim_save("main_animation.gif", main_gif)
main_frames <- image_read("main_animation.gif")
inset_img <- image_read(temp_file)

# Composite each frame
composite_frames <- map(1:length(main_frames), ~{
  image_composite(
    main_frames[.x], 
    inset_img, 
    offset = "+900+30"  # Adjust position
  )
})

# Create final animation
final_animation <- image_join(composite_frames)
image_animate(final_animation, fps = 10)


