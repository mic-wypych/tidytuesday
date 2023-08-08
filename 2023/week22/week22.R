library(tidyverse)
library(ggchicklet)

centenarians <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-30/centenarians.csv')

centenarians %>%
  ggplot() +
  ggchicklet:::geom_rrect(
    aes(
      xmin = birth_date, 
      xmax = death_date, 
      ymin = rank-.2, 
      ymax = rank + .2
    ),
    # Use relative npc unit (values between 0 and 1)
    # This ensures that radius is not too large for your canvas
    r = unit(0.5, 'npc'),
    fill = "black"
  )
