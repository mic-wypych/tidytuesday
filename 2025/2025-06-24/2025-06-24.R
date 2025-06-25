library(tidyverse)
library(geofacet)
library(ggview)
library(showtext)
library(patchwork)
library(gganimate)
library(png)
library(grid)


font <- "Roboto"

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
  theme(panel.spacing = unit(.5, "lines"),
        axis.text.x = element_blank(),
        strip.text = element_text(size = 15, family = font, face = "bold"),
        legend.position = "none",
        plot.title.position = "plot",
        plot.title = element_text(size = 20, family = font, margin = margin(20,0,20,0)),
        plot.background = element_rect(fill = "#cbeef3", color = "#cbeef3")
        )  +
  ggtitle("Measles lab confirmed cases per 100k in {round(frame_time, 0)}") +
  transition_time(year) +
  ease_aes("cubic-in-out", transition_length = 3)

#TODO
#' 1. add a line to the grid to make space for the legend
#' add glue to title
#' change linewidth to make animation clearer?

main_gif <- animate(eu_plot,renderer = gifski_renderer(), duration = 20, width = 1200, height = 1200, bg = "#cbeef3")

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


# IDEA 2: simple bar chart showing difference between clinical,
# linked and confirmed cases in Poland in 2019

title2 <- "Number of cases of measles in Poland, 2019"
subtitle2 <- str_wrap("The plot shows the number of suspected, clinical, epi linked and lab confirmed cases of measles in Poland in 2019. The most cases were present from the beginning of the hyear to may. Second half of the year saw much fewer cases.", 80)


legend_plot <- cases_month %>%
  filter(country == "Poland", year == 2019, month == 4) %>%
  pivot_longer(cols = measles_clinical:measles_lab_confirmed,names_to = "type", values_to = "n_cases") %>%
  mutate(type = str_remove(type, "measles_"),
         type = str_replace(type, "_", " "),
         type = factor(type, ordered = TRUE,
                       levels = c("clinical",
                                  "epi linked", "lab confirmed"))) %>%
  ggplot(aes(x = n_cases, y = factor(month), fill = type)) +
  geom_col(position = "stack", alpha = .8, width = .4) +
  geom_text(
            aes(label = type), position = position_stack(vjust = .5),vjust = -1, family = font,size = 4, fontface = "bold") +
  scale_fill_manual(values = c("#ffc2d1", "#f49cbb", "#f26a8d")) +
  theme_void() +
  theme(legend.position = "none")


clinical_exp <- "Clinically-compatible measles cases: A suspected case with fever and maculopapular (non-vesicular) rash and at least one of cough, coryza or conjunctivitis, but no adequate clinical specimen was taken and the case has not been linked epidemiologically to a laboratory-confirmed case of measles or other communicable disease"
epi_exp <- "Epidemiologically-linked measles cases: A suspected case of measles that has not been confirmed by a laboratory, but was geographically and temporally related with dates of rash onset occurring 7–23 days apart from a laboratory-confirmed case or another epidemiologically linked measles case"
lab_exp <- "Laboratory-confirmed measles cases: A suspected case of measles that has been confirmed positive by testing in a proficient laboratory, and vaccine-associated illness has been ruled out"

bar_p <- cases_month %>%
  filter(country == "Poland", year == 2019) %>%
  pivot_longer(cols = measles_clinical:measles_lab_confirmed,names_to = "type", values_to = "n_cases") %>%
  mutate(type = str_remove(type, "measles_"),
         type = factor(type, ordered = TRUE,
                       levels = c("clinical",
                                  "epi_linked", "lab_confirmed")),
         month = lubridate::month(month, label = TRUE, locale = "en"),
         month = fct_rev(month)) %>%
  filter(n_cases > 0) %>%
  ggplot(aes(x = n_cases, y = month, fill = type)) +
  geom_col(position = "stack", alpha = .8, width = .7) +
  geom_text(aes(label = n_cases), position = position_stack(vjust = .5),
                  size =6, family = font, fontface = "bold") +
  scale_fill_manual(values = c( "#ffc2d1", "#f49cbb", "#f26a8d")) +
  scale_x_continuous(expand = c(0,0)) +
  labs(title = title2, x = "number of cases", y = NULL) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        text = element_text(family = font),
        axis.text = element_text(size = 15, family = font, face = "bold"),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(size = 20, family = font),
        legend.title = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(size = 35, family = font),
        plot.subtitle = element_text(size = 15, family = font, lineheight = .5),
        plot.background = element_rect(fill = "white", color = "white"))

bar_final <- bar_p + inset_element(legend_plot, left = .3, bottom = .05, right = .9, top = .15)
bar_final
ggsave("C:/Users/wypyc/Desktop/measles_pl.png", bar_final,bg = "white", width = 6, height = 6)
