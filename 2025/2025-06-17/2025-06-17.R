library(tidyverse)
library(treemapify)
library(showtext)
library(ggtext)
library(ggforce)
library(patchwork)
library(gt)

font <- "Quantico"

font_add_google(font)  
showtext_auto()

api_categories <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-17/api_categories.csv')
api_info <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-17/api_info.csv')
api_logos <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-17/api_logos.csv')
api_origins <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-17/api_origins.csv')
apisguru_apis <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-17/apisguru_apis.csv')


main_pal <- c("Apache" = "#C94C68","MIT" = "#5DC2B2","Creative Commons" = "#D8B645", "Other" = "#8368B3", "missing" = "#4C9ACB")


#' title etc
title <- "API licenses"
subtitle <- str_wrap("APIgurus licenses across time and categories. 
                     The left plot shows when the CC, Apache, Mit and other licenses were added to APIs guru.
                     Most Creative Common and Apache license APIs were added in 2020s.
                     Notice that there is a lot of missing information, especially on pre-2021 APIs.
                     The right plot shows most common categories of APIs within each license type.
                     The table displays most common providers for each type of those APIs.", 130)
caption <- "Tidytuesday 2025-06-17 | data from APs Guru| Michał Wypych"

 
#apis over time
p_time <- api_info %>%
  inner_join(apisguru_apis, by = "name") %>%
  mutate(apache_cc = case_when(
    grepl("Creative|CC", license_name, ignore.case = TRUE) ~ "Creative Commons",
    grepl("Apache", license_name, ignore.case = TRUE) ~ "Apache",
    grepl("MIT", license_name, ignore.case = TRUE) ~ "MIT",
    is.na(license_name) ~ "missing",
    TRUE ~ "Other"
  )) %>%
  mutate(apache_cc = factor(apache_cc, ordered = T, levels = c("missing", "Other", "Creative Commons", "Apache", "MIT"))) %>%
  ggplot(aes(x =as.Date(added), y = apache_cc, color = apache_cc)) +
  geom_count() +
  geom_text(aes(x = as.Date("2025-08-01"), y = apache_cc, label = str_wrap(apache_cc, 10), color = apache_cc), lineheight = .5, size = 10, fontface = "bold", hjust = .5, inherit.aes = FALSE) +
  scale_y_discrete(position = "right") +
  scale_x_date(breaks = as.Date(paste0(2015:2024, "-01-01")), date_labels = "%Y", expand = c(0,0)) +
  scale_color_manual(values = main_pal) +
  coord_cartesian(xlim = c(as.Date("2014-12-01"), as.Date("2026-12-01"))) +
  labs(y = NULL, x = NULL) +
  guides(size = guide_legend(position = "top", hjust = 1, title = "number of licenses"), color = guide_none()) +
  theme_minimal() +
  theme(axis.text = element_text(hjust = .5, family = font, size = 15, face = "bold"),
        axis.text.y = element_blank(),
        text = element_text(family = font, size = 30),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.justification = "left",
        legend.box.just = "left",
        plot.margin = margin(t = 0, r = 10, b = 0, l = 0)) 





#provider table
t_prov <- api_info %>%
  inner_join(apisguru_apis, by = "name") %>%
  mutate(apache_cc = case_when(
    grepl("Creative|CC", license_name, ignore.case = TRUE) ~ "Creative Commons",
    grepl("Apache", license_name, ignore.case = TRUE) ~ "Apache",
    grepl("MIT", license_name, ignore.case = TRUE) ~ "MIT",
    is.na(license_name) ~ "missing",
    TRUE ~ "Other"
  )) %>%
  count(apache_cc, provider_name) %>%
  group_by(apache_cc) %>%
  arrange(desc(n)) %>%
  slice_max(n = 1, order_by = n) %>%
  ungroup() %>%
  rename("provider" = "provider_name") %>%
  gt(rowname_col = "apache_cc") %>%
  tab_header(title = "most common providers")  %>%
  opt_table_font(font = google_font(font), size = 30) 
  



#bar chart type
p_cat <- api_info %>%
  inner_join(api_categories, by = "name") %>%
  mutate(apache_cc = case_when(
    grepl("Creative|CC", license_name, ignore.case = TRUE) ~ "Creative Commons",
    grepl("Apache", license_name, ignore.case = TRUE) ~ "Apache",
    grepl("MIT", license_name, ignore.case = TRUE) ~ "MIT",
    is.na(license_name) ~ "missing",
    TRUE ~ "Other"
  )) %>%
  mutate(apache_cc = factor(apache_cc, ordered = T, levels = c("missing", "Other", "Creative Commons", "Apache", "MIT")),
         apisguru_category = str_replace(apisguru_category, pattern = "_", " ")) %>%
  count(apache_cc, apisguru_category) %>%
  group_by(apache_cc) %>%
  slice_max(n = 3, order_by = n) %>%
  ggplot(aes(x = n, y = apache_cc, color = apache_cc, group = fct_reorder(apisguru_category, n))) +
  geom_point(position = position_dodge(width = .9), size = 2) +
  geom_linerange(aes(xmin = 0, xmax = n, y = apache_cc,
                     color = apache_cc, group = fct_reorder(apisguru_category, n)),
                 position = position_dodge(width = .9), linewidth = 1.6) +
  geom_text(aes(label = paste0(apisguru_category, " : ", n)),
            position = position_dodge(width = .9), hjust = -.1, family = font, size = 8) +
  scale_y_discrete(labels = NULL) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = main_pal) +
  coord_cartesian(xlim = c(0, 800)) +
  labs(y = NULL, x = NULL) +
  theme_minimal() +
  theme(legend.position= "none",
        axis.text = element_text(hjust = .5, family = font, size = 15, face = "bold"),
        text = element_text(family = font, size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(0,0,0,0)) 



#final


final <- p_time + p_cat +
  inset_element(t_prov, left = .53, top = 1, bottom = .6, right = .53) +
  plot_annotation(title = title, subtitle = subtitle, caption = caption,
                                 theme = theme(text = element_text(family = font),
                                               plot.title = element_text(size = 40, family = font),
                                               plot.subtitle = element_text(size = 25, lineheight = .5, family = font),
                                               plot.margin = margin(t=10,r = 20, b = 10, l = 20)))

ggsave("apis.png", final, width = 10, height = 6, dpi = 300)

final +
  ggview::canvas(10, 6)

