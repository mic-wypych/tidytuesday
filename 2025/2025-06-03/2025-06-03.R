library(tidyverse)
library(treemapify)
library(showtext)
library(patchwork)
library(ggrepel)
font <- "Modern Antiqua"

font_add_google(font)
showtext_auto()



gutenberg_authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-03/gutenberg_authors.csv')
gutenberg_languages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-03/gutenberg_languages.csv')
gutenberg_metadata <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-03/gutenberg_metadata.csv')
gutenberg_subjects <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-03/gutenberg_subjects.csv')



## ideas:
# coauthored books in the gutenberg project
#what can I show about them>
# 1. topics
# 2. birthdates and death dates?
coauth <- gutenberg_metadata %>%
  count(gutenberg_id) %>%
  filter(n > 1)



coauth <- coauth %>%
  inner_join(gutenberg_metadata, by = "gutenberg_id")

#drop alice in wonderland, for some reason is listed twice 
coauth <- coauth[3:nrow(coauth),]

#select bookshelves and show which ones have more coauthored books?
#show number of coautors per book?

#get main bookshelves
coauth <- coauth %>%
  mutate(bookshelf = str_remove_all(gutenberg_bookshelf, "[Bb]rowsing[: ]*")) %>%
  mutate(main_shelf = str_extract(bookshelf, "^[^/]*"))

coauth %>%
  count(gutenberg_id) %>%
  ggplot(aes(x = n)) +
  geom_histogram()



#age difference of coauthors

#ok what if there are many many coauthors?
#then I'll need age diff between all of them




calc_pairwise_diffs <- function(row_values, col_names) {
  valid_idx <- !is.na(row_values)
  valid_values <- row_values[valid_idx]
  valid_names <- col_names[valid_idx]


  n <- length(valid_values)
  pairs <- combn(n, 2, simplify = FALSE)
  

  map_dfr(pairs, function(pair) {
    i <- pair[1]
    j <- pair[2]
    tibble(
      col1 = valid_names[i],
      col2 = valid_names[j],
      difference = valid_values[i] - valid_values[j]
    )
  })
}


calculate_pairwise_differences <- function(df, cols = paste0("a_", 1:12)) {
  # Select the specified columns
  col_data <- df %>% select(all_of(cols))
  
  # Apply the function rowwise
  df %>%
    mutate(row_id = row_number()) %>%
    rowwise() %>%
    summarise(
      row_id = row_id,
      pairwise_diffs = list(calc_pairwise_diffs(
        c_across(all_of(cols)), 
        cols
      )),
      .groups = "drop"
    ) %>%
    unnest(pairwise_diffs) %>%
    filter(nrow(.) > 0 | n() == 0)
}


#get birthdates

coauth <- coauth %>%
  inner_join(gutenberg_authors, by = "gutenberg_author_id")

age_diffs <-coauth %>%
  group_by(gutenberg_id) %>%
  mutate(auth_pos = paste0("a_",1:n())) %>%
  pivot_wider(id_cols = "gutenberg_id", names_from = auth_pos, values_from = birthdate) %>%
  calculate_pairwise_differences() %>%
  mutate(difference = abs(difference))
  

pal_hist <- colorRampPalette(c("#936639","#6f1d1b", "#292019"), interpolate = "spline")

pal_agediffs <- pal_hist(2626)

p_agediffs <- age_diffs %>%
  ggplot(aes(x = difference+ 1)) +
  geom_histogram(binwidth = 1, linewidth = 1, aes(fill = after_stat(factor(x))), color = "#392619") +
  geom_vline(aes(xintercept = mean(difference)), linewidth = 1.5, color = "#6f1d1b") +
  annotate(geom = "text", x = 1750, y = 30, label = "One person listed Homer\nas coauthor with\nage difference of 2625!", family = font) +
  annotate(geom = "text", x = 50, y = 130, label = paste0("Mean age difference is ", round(mean(age_diffs$difference), 2), " years"), family = font) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(breaks = c(1, 6, 11, 51, 101, 1001, 2001), 
                     labels =c(1, 6, 11, 51, 101, 1001, 2001) - 1,
                     expand = c(0,0)) +
  scale_fill_manual(values = pal_agediffs) +
  coord_trans(x = "log10", xlim = c(.8, 2650)) +
  labs(x = "age difference", y = "number of coauthors") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        text = element_text(family = font, size = 15))
  

#ok so this looks nice but there are some weird things like difference of over 2k
#year cuz Homer is listed as coauthor??? ANd a few books with diff of over 75 years

#titles etc

title <- "Coauthored books in the Gutenberg Project"
subtitle <- "By far the most common shelf of coauthored books is Culture Other common shelves include Literature, Children & Young Adult Reading and History.
             \nAverage age difference between coautrors is almost 27 years but the most common difference is 5 years."
caption <- "Data: {gutenbergr} for Tidytuesday 2025-06-03 | Michał Wypych"
#### topics ------


#most common shelves for coauthored books
#theme it further: font, colors etc

#color palette: 187 colors needed
set.seed(2137)
col_palette <- c("#ffe6a7", "#99582a", "#e07a5f","#a9927d", "#bb9457", "#6f1d1b", "#936639", "#9e2a2b")

pal <- sample(col_palette, 187, replace = T)

p_treemap <- coauth %>%
  distinct(gutenberg_id, .keep_all = TRUE) %>%
  count(main_shelf) %>%
  ggplot() +
  aes(area = n, value = n, label = paste(main_shelf, ":", n), fill = main_shelf) +
  geom_treemap(color = "black") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    family = font,
                    fontface = "bold",
                    size = 15) +
  scale_fill_manual(values = pal) +
  theme(legend.position = "none")



p_agediffs + inset_element(p_treemap, left = .6, bottom = .6, right = 1, top = 1) +
  plot_annotation(title = title, caption = caption, subtitle = subtitle,
                  theme = theme(text = element_text(family = font),
                                plot.title = element_text(family = font, size = 30),
                                plot.subtitle = element_text(family = font, size = 15, lineheight = .5)))
