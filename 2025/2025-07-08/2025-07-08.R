library(tidyverse)
library(data.table)
library(patchwork)
library(showtext)

font <- "Poppins"
font_add_google(font)
showtext_auto()

answers showtextanswers <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-08/answers.csv')
color_ranks <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-08/color_ranks.csv')
users <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-08/users.csv')



#join answers and users

ans_user <- merge(answers, users, by = "user_id") |> merge(color_ranks, by = "rank")


title <- "Overlap in decscriptions of colors from xkcd color survey"
caption <- "Tidytuesday 8.07.2025 | data: xkcd color survey | Michał Wypych"


#### full dataset


full_list <- list()

for(color in all_colors) {
  picked_color <- color
  
  subset_colors <- all_colors[which(all_colors != picked_color)]
  
  list_plots <- map(subset_colors, \(x) intersect(ans_user[which(ans_user$color == picked_color), "hex.x"], ans_user[which(ans_user$color == x), "hex.x"]))
  plots <- imap(list_plots, ~ {.x |> ggplot(aes(ymin = 1, ymax = 2, x = hex.x, color = hex.x)) + 
      geom_linerange(linewidth = 1) + 
      scale_color_identity() + 
      theme_void() +
      theme(legend.position = "none") + 
      coord_radial() +
      labs(title = paste0(picked_color, " and ", subset_colors[.y])) +
      theme(text = element_text(family = font),
            plot.title = element_text(family = font, size = 20),
            strip.text = element_text(family = font, hjust = .5)
            )
    })
  
  full_list <- append(full_list, plots)
}

final <- (plot_spacer() | full_list[[1]] | full_list[[2]] | full_list[[3]] | full_list[[4]]) / (plot_spacer() | full_list[[6]] | full_list[[7]] | full_list[[8]]) /  (plot_spacer() |  plot_spacer() | full_list[[11]] | full_list[[12]]) /  (plot_spacer() | plot_spacer() | plot_spacer() | full_list[[16]]) +
  plot_annotation(title = title, caption = caption, theme = theme(plot.title = element_text(family = font, size = 40)))

ggsave("C:/Users/wypyc/Desktop/xkcd_colors.png", final, width = 8, height = 8)
