library(tidyverse)
library(patchwork)
fide_ratings_august <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-23/fide_ratings_august.csv')
fide_ratings_september <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-23/fide_ratings_september.csv')

fide_ratings_september$age_bin <- cut(fide_ratings_september$bday, 8)
fide_ratings_september$rating_bin <- cut(fide_ratings_september$rating, 8)
fide_ratings_september$games_bin <- cut(fide_ratings_september$games, 8)





plot_a_list <- function(master_list_with_plots, no_of_rows, no_of_cols) {

  patchwork::wrap_plots(master_list_with_plots, 
                        nrow = no_of_rows, ncol = no_of_cols)
}


plots_list <- list()

for(row in 1:8) {
  for (col in 1:8) {
    p <- fide_ratings_september %>%
      filter(rating_bin == unique(rating_bin)[row], age_bin == unique(age_bin)[col]) %>%
      count(sex) %>%
      ggplot(aes(x= 1, y = n, fill = sex)) +
      geom_col(position = "fill") +
      scale_fill_manual(values = c(M = "#f9dbbd",F= "#a53860")) +
      theme_void() +
      theme(legend.position = "none",
            plot.margin = margin(0,0,0,0),)
    
    plots_list <- append(plots_list, list(p))
  }
}



final <- plot_a_list(plots_list, 8, 8) + plot_annotation(theme = theme(plot.background = element_rect(fill = "white")))
#o kwe have the basic grid, unfortunately not a full board

#need to add all annotations:
#custom legend
#title and explanations + comment


ggsave("C:/Users/wypyc/Desktop/chess_1.png", final, width = 4, height = 4)
