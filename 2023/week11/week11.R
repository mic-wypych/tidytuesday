library(tidyverse)
library(lubridate)
library(showtext)
library(ggtext)
library(glue)
library(ggforce)

font_add_google('Open Sans')
showtext_auto()
drugs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-14/drugs.csv')


#calculate days from decision to opinion
drugs$date_of_opinion <- as.Date(drugs$date_of_opinion)
drugs <- drugs %>%  unite(marketing_date,c('marketing_authorisation_date', 'date_of_refusal_of_marketing_authorisation'), sep='', remove = FALSE, na.rm = TRUE)

drugs$days_to_market <- as.Date(drugs2$marketing_date) - drugs2$date_of_opinion


#drop missings from authorisation status and relevel
drugs <- drugs %>%
  drop_na(authorisation_status) %>%
  mutate(authorisation_status = factor(authorisation_status, levels = c('authorised', 'withdrawn', 'refused')))

#text to put on plot
subtitle <- glue("Drugs that <span style = 'color:#457b9d'>receive</span> additional monitoring usually receive a decision
                 after similar amount of time as those that <span style = 'color:#e63946'>don\'t</span>.
                 Some drugs received marketing much earlier than the opinion.<br>Azomyr, an anti-allergic drug got it's approval almost 8000 days before the opinion!
                 Surprisingly there were some negative times - opinions were published after the decision.<br>
                 The most extreme case was Grastofil, a drug developed by Accord Healthcare S.L.U. for Neutropenia. Almost 4000 days passed from opinion to decision!<br>
                 The zoom shows distribution of drugs that received the decision within a year from the opinion.")

#plot
plot <- ggplot(drugs2 %>% filter(additional_monitoring==TRUE)) +
  ggdist::stat_dots(aes(x=days_to_market, y = authorisation_status,color= additional_monitoring, fill = additional_monitoring),alpha = .8, side = 'right', scale = .5, dotsize = 2) +
  ggdist::stat_dots(data = drugs2 %>% filter(additional_monitoring==FALSE), aes(x=days_to_market, y = fct_reorder(as.factor(authorisation_status), days_to_dec),color= additional_monitoring, fill = additional_monitoring),alpha = .8, side = 'left') +
  facet_zoom(xlim=c(0,365)) +
  scale_color_manual(values = c('#e63946', '#457b9d')) +
  theme_minimal() +
  labs(x = 'Days to decision', y = NULL,
       title = 'From opinion to decision on a drug',
       subtitle = subtitle,
       caption = "Tidytuesday 2023 week 11 | Data: European Medicines Agency |@michal_wypych") +
  theme(legend.position = 'none',
        text = element_text(family='Open Sans', size = 18),
        plot.title = element_markdown(family = 'Open Sans', size = 40),
        plot.subtitle = element_markdown(family = 'Open Sans', size = 15,lineheight = .35),
        plot.caption = element_text(family = 'Open Sans', size = 10, color = 'grey60'),
        plot.background = element_rect(fill = '#f5efff', color = '#f5efff'),
        strip.background = element_rect(fill = '#f5efff', color = '#f5efff'),
        panel.grid.major = element_line(linetype='dashed', size = .2, color = 'grey80'),
        panel.grid.minor = element_line(linetype = 'dashed', size = .1, color='grey80'),
        axis.ticks.y = element_blank(),
        zoom.x = element_rect(fill='#e5d9f2'),
        zoom.y = element_rect(fill=NA))

#save it
ggsave('drugs.png', plot, height = 4,width=6)
