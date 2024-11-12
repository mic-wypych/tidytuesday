library(tidyverse)
library(ggmap)

population <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-22/population.csv')


uktraine <- population[population$coo_name == "Ukraine",]

ukraine_2022 <- uktraine[uktraine$year == 2022,]

ukraine_2022 %>%
  arrange(desc(refugees)) %>%
  select(coa_name)


#try to make a map of refugees with size of arrows indicating how many have fled to a given country +
#some bar chart showin exaxt numbers?