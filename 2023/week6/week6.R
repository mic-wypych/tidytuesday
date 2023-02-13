library(tidyverse)
library(showtext)
library(ggpath)

font_add_google("Merriweather", "Merriweather")

showtext_auto()

big_tech_stock_prices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_stock_prices.csv')
big_tech_companies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_companies.csv')


#march 2020 market crash

#prepare data
data <- big_tech_stock_prices %>%
  inner_join(big_tech_companies) %>%
  mutate(date = as_date(date))



#Logos
data <- data %>%
  filter(date %in% c(as_date('2020-02-19'), as_date('2020-03-09'))) %>%
  select(company, date, close) %>%
  mutate(
    simple_name = tolower(str_extract(company, '[a-zA-Z]*')),
    simple_name = case_when(
      str_detect(simple_name, "international") ~ "ibm",
      str_detect(simple_name, "amazon") ~ "amazon",
      TRUE ~ simple_name
    ), # Please note that you might need to change the paths. I had plenty of problems with here::here()
    path = paste0(here::here(), "/GitHub/tidytuesday/2023/week6/logos/", paste0(simple_name, ".png"))
  ) 

#paths for annotations
# Please note that you might need to change the paths. I had plenty of problems with here::here()
adobe <- paste0(here::here(), "/GitHub/tidytuesday/2023/week6/logos/adobe.png")
meta <- paste0(here::here(), "/GitHub/tidytuesday/2023/week6/logos/meta.png")
tesla <- paste0(here::here(), "/GitHub/tidytuesday/2023/week6/logos/tesla.png")
subtitle <- 'All big tech stock prices fell\nduring the March 2020 market crash.\nThe plot shows differences in stock prices between\nFebruary 19th 2020 (day before)\nand March 9th 2020 (Black Monday)'

#plot
plot <- data %>% 
  mutate(path = fct_reorder(path, close, .fun = max, desc = TRUE)) %>%
  ggplot() +
  geom_from_path(aes(x = -15, y = path, path = path), width =.05, height = .05) +
  geom_line(aes(x = close, y = path, color = as.factor(date), group = company),arrow = arrow(length=unit(0.20,"cm"), ends="first", type = "closed"),color = '#e63946', size = .75) +
  geom_curve(aes(x = 125, xend=169, y = adobe, yend = meta),
             arrow = arrow(length=unit(0.10,"cm"), type = "closed"), curvature = .2,
             color = 'gray80', size = .55) +
  geom_curve(aes(x = 230, xend=217, y = adobe, yend = meta),
             arrow = arrow(length=unit(0.10,"cm"), type = "closed"), curvature = -.2,
             color = 'gray80', size = .55,) +
  annotate(geom='text', x = 230,y= adobe, label = 'Feb. 19th 2020',lineheight=.5,family = 'Merriweather', size = 3) +
  annotate(geom='text', x = 125,y= adobe, label = 'Mar. 9th 2020',lineheight=.5,family = 'Merriweather', size = 3, vjust = .5) +
  annotate(geom='text', x = 400,y= tesla, label = subtitle ,lineheight=1,family = 'Merriweather', size = 4, hjust = 1, color = 'gray20') +
  labs(title = 'March 2020 market crash',
       x = 'closing stock price', y = NULL) +
  scale_y_discrete(labels = NULL) +
  scale_x_continuous(labels=scales::dollar_format()) +
  theme_minimal() +
  theme(legend.position = 'none',
        legend.title = element_blank(),
        text = element_text(family = 'Merriweather', color = 'gray20', size = 7),
        plot.title = element_text(family = 'Merriweather', hjust = .5, size =30, color = '#e63946'),
        plot.subtitle = element_text(family = 'Merriweather', hjust = .5, size =15, color = 'gray20', lineheight = .5),
        axis.text = element_text(family = 'Merriweather', color = 'gray20', size = 10),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(linetype = 'dashed'),
        panel.grid.minor.x = element_line(linetype = 'dashed'),
        plot.background = element_rect(fill='white', color = 'white'))



ggsave(paste0(here::here(), "/GitHub/tidytuesday/2023/week6/plot.png"), plot)


## Note: add bouncing back? How they went back up after the crash?
