library(tidyverse)
library(lubridate)
library(showtext)
library(ggtext)
library(glue)


font_add_google('Open Sans')
showtext_auto()
drugs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-14/drugs.csv')


#calculate days from decision to opinion
drugs$decision_date <- as.Date(drugs$decision_date)

drugs$date_of_opinion <- as.Date(drugs$date_of_opinion)
drugs$days_to_dec <- drugs$decision_date - drugs$date_of_opinion

#drop missings from authorisation status and relevel
drugs <- drugs %>%
  drop_na(authorisation_status) %>%
  mutate(authorisation_status = factor(authorisation_status, levels = c('authorised', 'withdrawn', 'refused')))

#text to put on plot
subtitle <- glue("Drugs that <span style = 'color:#457b9d'>receive</span> additional monitoring usually receive a decision
                 faster than those that <span style = 'color:#e63946'>don\'t</span>.<br>
                 The few drugs that were refused received their decisions very quickly.<br>
                 Surprisingly there were some negative waiting times - opinions were published after the decision.<br>
                 The most extreme case was Betaferon, a drug developed by Bayer AG for Multiple Sclerosis.<br> 10000 days passed from opinion to decision!")

#plot
plot <- ggplot(drugs %>% filter(additional_monitoring==TRUE)) +
ggdist::stat_dots(aes(x=days_to_dec, y = authorisation_status,color= additional_monitoring, fill = additional_monitoring),alpha = .8, side = 'right', scale = .5, dotsize = 1) +
ggdist::stat_dots(data = drugs %>% filter(additional_monitoring==FALSE), aes(x=days_to_dec, y = fct_reorder(as.factor(authorisation_status), days_to_dec),color= additional_monitoring, fill = additional_monitoring),alpha = .8, side = 'left') +
geom_richtext(aes(y = 'refused', x = 7500, label = subtitle), size = 5,lineheight=.6, fill = '#f8f7ff', family = 'Open Sans',label.size=0) +
scale_fill_manual(values = c('#e63946', '#457b9d')) +
scale_color_manual(values = c('#e63946', '#457b9d')) +
labs(x = 'Days to decision', y = NULL,
     title = 'From opinion to decision on a drug',
     caption = "Tidytuesday 2023 week 11 | Data: European Medicines Agency |@michal_wypych") +
theme_minimal() +
theme(legend.position = 'none',
      text = element_text(family='Open Sans', size = 18),
      plot.title = element_markdown(family = 'Open Sans', size = 40),
      plot.subtitle = element_markdown(family = 'Open Sans', size = 15,lineheight = .35),
      plot.caption = element_text(family = 'Open Sans', size = 10, color = 'grey60'),
      plot.background = element_rect(fill='#f8f7ff',color='#f8f7ff'),
      panel.grid.major = element_line(linetype='dashed', size = .2, color = 'grey80'),
      panel.grid.minor = element_line(linetype = 'dashed', size = .1, color='grey80'))
plot

#save it
ggsave('drugs.png', plot, height = 4,width=6)
