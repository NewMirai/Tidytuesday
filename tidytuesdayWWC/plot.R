library(readr)
library(tidyverse)
library(ggridges)
library(viridis)

wwc_outcomes <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/wwc_outcomes.csv")
squads <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/squads.csv")
codes <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/codes.csv")

wwc_outcomes <- left_join(wwc_outcomes, codes, by = "team")


median_age_plot <- squads %>%
  group_by(country) %>% 
  mutate(median_age=median(age)) %>% 
  ungroup() %>% 
  mutate(country=fct_reorder(country,-median_age)) %>% 
  ggplot(aes(x=age,y=country))+
  stat_density_ridges(quantile_lines = T,scale=.9,rel_min_height = 0.01)+
  scale_x_continuous(breaks = seq(10,45,5))

median_age_plot