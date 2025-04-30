library(tidyverse)
library(glue)
library(plotly)
library(showtext)
library(htmlwidgets)
font_add_google("Jost")
showtext_auto()
user2025 <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-29/user2025.csv")


font <- "Jost"

waffle_df <- user2025 %>%
  group_by(room) %>%
  arrange(room, date, time) %>%
  mutate(
    id = row_number(),
    row = (id - 1) %/% 10,
    col = (id - 1) %% 10,
    content = str_wrap(content, 100),
    date = factor(date)
  ) %>%
  ungroup() 

# Tooltip text
waffle_df <- waffle_df %>%
  mutate(tooltip = glue("<b>speakers</b>: {speakers}<br><b>title</b>: {title}<br><b>date</b>: {date}<br><b>time</b>: {time}<br><b>content</b>: {content}<br><b>video available</b>: {video_recording}"))



p <- ggplot(waffle_df, aes(x = col, y = time, fill = date, text = tooltip)) +
  geom_tile(color = "#caf0f8", width = 0.9, height = 0.9) +
  facet_wrap(~room) +
  scale_fill_manual(values = c("#023e8a","#0096c7","#48cae4", "#ade8f4")) +
  coord_equal() +
  labs(title = "useR!2025 timetable") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(size= 12, family = font),
    text = element_text(family = font),
    plot.title = element_text(family = font, size = 20)
  )

# Convert to interactive plotly
plotly_fin <- ggplotly(p, tooltip = "text") %>%
  layout(
    hoverlabel = list(
      bgcolor = "#caf0f8", 
      bordercolor = "black", 
      font = list(family = "'Jost', sans-serif", size = 12)
    ),
    font = list(family = "'Quantico', sans-serif"),
    title = list(text = "useR! 2025 Schedule by room",
                 y = .98,
                 yanchor = "top"),
    annotations = list(
      list(
        x = 0,
        y = 1,
        xref = "paper",
        yref = "paper",
        text = "This is an interactive plot that lets you see the talks by room in useR!2025.<br>Hover over each square to find more information about the talk",
        showarrow = FALSE,
        align = "left",
        xanchor = "left",
        yanchor = "bottom",
        xshift = 0,
        yshift = 35, 
        font = list(
          family = "'Jost', sans-serif",
          size = 14
        )
      )),
    legend = list(orientation = "h", y = -0.1),
    margin = list(
      t = 120
    ),
    paper_bgcolor = "#caf0f8",
    plot_bgcolor = "#caf0f8"
  )



saveWidget(
  widget = plotly_fin,
  file = "Documents/my_plot.html",
  selfcontained = TRUE
)
